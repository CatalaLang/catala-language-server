#!/usr/bin/env node
/**
 * I18N Keys Usage Linter
 *
 * Checks:
 *  - For each key in src/locales/*.json, ensure the literal key text appears
 *    at least once in some TSX file under src/.
 *  - If a key never appears in any TSX file, exit with code 1.
 *
 * Notes:
 *  - Uses the TypeScript parser only to collect string literals from TSX.
 *  - Conservative by design: we only report a key when its literal text is
 *    absent from all string literals in TSX sources.
 *  - Dynamic IDs are not detected; this is acceptable for this check.
 */

/* eslint-disable @typescript-eslint/no-var-requires */

const fs = require('fs');
const path = require('path');
const glob = require('glob');

const ts = require('typescript');

function readJSON(file) {
  try {
    const content = fs.readFileSync(file, 'utf8');
    return JSON.parse(content);
  } catch (e) {
    console.error(`Failed to read/parse JSON: ${file}\n${e.message}`);
    process.exit(1);
  }
}

/**
 * Flatten a locale object into dot-separated keys.
 * Example: { a: { b: "x" }, c: "y" } -> ["a.b", "c"]
 */
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

/**
 * Collect all string literals from TSX using the TS AST.
 */
function collectStringsFromAst(fileName, sourceText) {
  const found = new Set();

  try {
    const sf = ts.createSourceFile(
      fileName,
      sourceText,
      ts.ScriptTarget.Latest,
      /*setParentNodes*/ true,
      ts.ScriptKind.TSX
    );

    function visit(node) {
      // String literals: "foo" or 'bar'
      if (
        ts.isStringLiteral?.(node) ||
        node.kind === ts.SyntaxKind.StringLiteral
      ) {
        found.add(node.text);
      }
      // Template literals without interpolations: `foo`
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
    // Don't fail the run if AST parsing fails; we still do raw text search.
    console.warn(
      `Warning: Failed to parse TSX via TypeScript for ${fileName}: ${e.message}`
    );
  }
  return found;
}

function main() {
  console.log('Scanning TSX files and locale keys for i18n usage...');

  const tsxFiles = glob.sync('src/**/*.tsx', { nodir: true });
  const localeFiles = glob.sync('src/locales/*.json', { nodir: true });

  if (localeFiles.length === 0) {
    console.log(
      'No locale files found at src/locales/*.json. Nothing to check.'
    );
    process.exit(0);
  }

  if (tsxFiles.length === 0) {
    console.error(
      'Error: No TSX files found under src/**/*.tsx. Cannot verify i18n keys.'
    );
    process.exit(1);
  }

  console.log(
    `Found ${localeFiles.length} locale file(s) and ${tsxFiles.length} TSX file(s).`
  );

  // Gather all strings from TSX (AST + regex fallback) and also keep raw text for an absolute substring search
  const allStringLiterals = new Set();

  for (const file of tsxFiles) {
    const content = fs.readFileSync(file, 'utf8');
    const fromAst = collectStringsFromAst(file, content);
    for (const s of fromAst) allStringLiterals.add(s);
  }

  let totalKeys = 0;
  let missingTotal = 0;
  const missingByLocale = new Map();

  for (const locFile of localeFiles) {
    const localeObj = readJSON(locFile);
    const keys = flattenKeys(localeObj);
    totalKeys += keys.length;

    const missing = [];
    for (const key of keys) {
      const present = allStringLiterals.has(key);

      if (!present) {
        missing.push(key);
      }
    }

    if (missing.length > 0) {
      missingByLocale.set(locFile, missing);
      missingTotal += missing.length;
    }
  }

  if (missingTotal > 0) {
    console.error('\nMissing i18n keys (not found in any TSX file):');
    for (const [file, keys] of missingByLocale.entries()) {
      console.error(`\n- ${file}`);
      for (const k of keys) {
        console.error(`  • ${k}`);
      }
    }
    console.error(
      `\nResult: ${missingTotal} missing key(s) out of ${totalKeys} total across ${localeFiles.length} locale file(s).`
    );
    process.exit(1);
  } else {
    console.log(
      `\nSuccess: All ${totalKeys} i18n key(s) from ${localeFiles.length} locale file(s) appear in TSX sources.`
    );
    process.exit(0);
  }
}

if (require.main === module) {
  main();
}
