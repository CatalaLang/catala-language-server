#!/usr/bin/env node
/**
 * Combined i18n / l10n linter. Two independent checks:
 *
 * 1. React-intl (src/locales/*.json):
 *    Every key in a locale file must appear as a string literal somewhere in
 *    src/**\/*.{ts,tsx}. Uses the TypeScript AST for precision.
 *
 * 2. vscode.l10n (l10n/bundle.l10n.fr.json):
 *    - Every key in the bundle must be referenced by a vscode.l10n.t() call.
 *    - Every vscode.l10n.t() string literal must have a translation in the bundle.
 */

/* eslint-disable @typescript-eslint/no-var-requires */

const fs = require('fs');
const glob = require('glob');
const ts = require('typescript');

// ─── helpers ────────────────────────────────────────────────────────────────

function readJSON(file) {
  try {
    return JSON.parse(fs.readFileSync(file, 'utf8'));
  } catch (e) {
    console.error(`Failed to read/parse JSON: ${file}\n${e.message}`);
    process.exit(1);
  }
}

/** Flatten { a: { b: "x" }, c: "y" } → ["a.b", "c"] */
function flattenKeys(obj, prefix = '') {
  const keys = [];
  if (!obj || typeof obj !== 'object') return keys;
  for (const [k, v] of Object.entries(obj)) {
    const full = prefix ? `${prefix}.${k}` : k;
    if (v && typeof v === 'object' && !Array.isArray(v)) {
      keys.push(...flattenKeys(v, full));
    } else {
      keys.push(full);
    }
  }
  return keys;
}

/** Collect all string literals from a TS/TSX file via the TypeScript AST. */
function collectStringsFromAst(fileName, sourceText) {
  const found = new Set();
  try {
    const scriptKind = fileName.endsWith('.tsx')
      ? ts.ScriptKind.TSX
      : ts.ScriptKind.TS;
    const sf = ts.createSourceFile(
      fileName,
      sourceText,
      ts.ScriptTarget.Latest,
      true,
      scriptKind
    );
    function visit(node) {
      if (
        ts.isStringLiteral?.(node) ||
        node.kind === ts.SyntaxKind.StringLiteral
      ) {
        found.add(node.text);
      }
      if (
        ts.isNoSubstitutionTemplateLiteral?.(node) ||
        node.kind === ts.SyntaxKind.NoSubstitutionTemplateLiteral
      ) {
        found.add(node.text);
      }
      ts.forEachChild(node, visit);
    }
    visit(sf);
  } catch (e) {
    console.warn(`Warning: Failed to parse ${fileName}: ${e.message}`);
  }
  return found;
}

/**
 * Collect React-intl message IDs actually used in source via:
 *   <FormattedMessage id="some.key" />
 *   intl.formatMessage({ id: 'some.key' })
 */
function collectReactIntlIds(fileName, sourceText) {
  const ids = new Set();
  try {
    const scriptKind = fileName.endsWith('.tsx')
      ? ts.ScriptKind.TSX
      : ts.ScriptKind.TS;
    const sf = ts.createSourceFile(
      fileName,
      sourceText,
      ts.ScriptTarget.Latest,
      true,
      scriptKind
    );
    function visit(node) {
      // <FormattedMessage id="some.key" /> or <FormattedMessage id={"some.key"}>
      if (ts.isJsxOpeningElement(node) || ts.isJsxSelfClosingElement(node)) {
        if (
          ts.isIdentifier(node.tagName) &&
          node.tagName.text === 'FormattedMessage'
        ) {
          for (const attr of node.attributes.properties) {
            if (
              ts.isJsxAttribute(attr) &&
              ts.isIdentifier(attr.name) &&
              attr.name.text === 'id'
            ) {
              if (attr.initializer && ts.isStringLiteral(attr.initializer)) {
                ids.add(attr.initializer.text);
              } else if (
                attr.initializer &&
                ts.isJsxExpression(attr.initializer) &&
                attr.initializer.expression &&
                ts.isStringLiteral(attr.initializer.expression)
              ) {
                ids.add(attr.initializer.expression.text);
              }
            }
          }
        }
      }
      // intl.formatMessage({ id: 'some.key' }) or formatMessage({ id: '...' })
      if (ts.isCallExpression(node)) {
        const callee = node.expression;
        const isFormatMessage =
          (ts.isIdentifier(callee) && callee.text === 'formatMessage') ||
          (ts.isPropertyAccessExpression(callee) &&
            callee.name.text === 'formatMessage');
        if (isFormatMessage && node.arguments.length >= 1) {
          const firstArg = node.arguments[0];
          if (ts.isObjectLiteralExpression(firstArg)) {
            for (const prop of firstArg.properties) {
              if (
                ts.isPropertyAssignment(prop) &&
                ts.isIdentifier(prop.name) &&
                prop.name.text === 'id' &&
                ts.isStringLiteral(prop.initializer)
              ) {
                ids.add(prop.initializer.text);
              }
            }
          }
        }
      }
      ts.forEachChild(node, visit);
    }
    visit(sf);
  } catch (e) {
    console.warn(`Warning: Failed to parse ${fileName}: ${e.message}`);
  }
  return ids;
}

// Matches the first string argument of vscode.l10n.t() — single or double quotes,
// handles multi-line call layout.
const L10N_CALL_RE =
  /vscode\.l10n\.t\(\s*(?:'((?:[^'\\]|\\.)*)'|"((?:[^"\\]|\\.)*)")/gs;

function extractL10nKeys(source) {
  const keys = new Set();
  for (const match of source.matchAll(L10N_CALL_RE)) {
    keys.add(match[1] ?? match[2]);
  }
  return keys;
}

// ─── check 1: React-intl locale keys ────────────────────────────────────────

function checkReactIntl(tsxFiles) {
  const localeFiles = glob.sync('src/locales/*.json', { nodir: true });
  if (localeFiles.length === 0) {
    console.log('React-intl: no locale files found, skipping.');
    return true;
  }

  const allStringLiterals = new Set();
  const allSourceIds = new Set();
  for (const file of tsxFiles) {
    const content = fs.readFileSync(file, 'utf8');
    for (const s of collectStringsFromAst(file, content)) allStringLiterals.add(s);
    for (const id of collectReactIntlIds(file, content)) allSourceIds.add(id);
  }

  let ok = true;

  // Direction 1: locale keys absent from source (dead translations)
  let deadTotal = 0;
  const deadByLocale = new Map();
  for (const locFile of localeFiles) {
    const keys = flattenKeys(readJSON(locFile));
    const dead = keys.filter((k) => !allStringLiterals.has(k));
    if (dead.length > 0) {
      deadByLocale.set(locFile, dead);
      deadTotal += dead.length;
    }
  }
  if (deadTotal > 0) {
    console.error('\nReact-intl: keys not found in any TS/TSX source (dead):');
    for (const [file, keys] of deadByLocale.entries()) {
      console.error(`  ${file}`);
      for (const k of keys) console.error(`    • ${k}`);
    }
    ok = false;
  }

  // Direction 2: source IDs absent from a locale file (missing translations)
  let missingTotal = 0;
  const missingByLocale = new Map();
  for (const locFile of localeFiles) {
    const localeKeys = new Set(flattenKeys(readJSON(locFile)));
    const missing = [...allSourceIds].filter((id) => !localeKeys.has(id));
    if (missing.length > 0) {
      missingByLocale.set(locFile, missing);
      missingTotal += missing.length;
    }
  }
  if (missingTotal > 0) {
    console.error('\nReact-intl: source IDs missing from locale file(s):');
    for (const [file, ids] of missingByLocale.entries()) {
      console.error(`  ${file}`);
      for (const id of ids) console.error(`    • ${id}`);
    }
    ok = false;
  }

  if (ok) {
    const total = [...new Set(localeFiles.flatMap((f) => flattenKeys(readJSON(f))))].length;
    console.log(
      `React-intl: all ${total} key(s) across ${localeFiles.length} locale file(s) consistent with source.`
    );
  }
  return ok;
}

// ─── check 2: vscode.l10n bundle ────────────────────────────────────────────

function checkVscodeLtl0n(tsxFiles) {
  const BUNDLE = 'l10n/bundle.l10n.fr.json';
  if (!fs.existsSync(BUNDLE)) {
    console.log(`vscode.l10n: ${BUNDLE} not found, skipping.`);
    return true;
  }

  const bundleKeys = new Set(Object.keys(readJSON(BUNDLE)));
  const allSourceKeys = new Set();
  for (const file of tsxFiles) {
    for (const key of extractL10nKeys(fs.readFileSync(file, 'utf8'))) {
      allSourceKeys.add(key);
    }
  }

  const dead = [...bundleKeys].filter((k) => !allSourceKeys.has(k));
  const missing = [...allSourceKeys].filter((k) => !bundleKeys.has(k));

  if (dead.length > 0) {
    console.error('\nvscode.l10n: dead translations (in bundle but not in source):');
    dead.forEach((k) => console.error(`  - "${k}"`));
  }
  if (missing.length > 0) {
    console.error('\nvscode.l10n: missing translations (in source but not in bundle):');
    missing.forEach((k) => console.error(`  - "${k}"`));
  }

  if (dead.length > 0 || missing.length > 0) return false;

  console.log(`vscode.l10n: all ${bundleKeys.size} key(s) accounted for.`);
  return true;
}

// ─── main ────────────────────────────────────────────────────────────────────

function main() {
  const tsxFiles = glob.sync('src/**/*.{ts,tsx}', {
    nodir: true,
    ignore: ['**/*.d.ts'],
  });

  if (tsxFiles.length === 0) {
    console.error('Error: no TS/TSX files found under src/.');
    process.exit(1);
  }

  const ok1 = checkReactIntl(tsxFiles);
  const ok2 = checkVscodeLtl0n(tsxFiles);

  process.exit(ok1 && ok2 ? 0 : 1);
}

if (require.main === module) {
  main();
}
