import * as vscode from 'vscode';
import * as path from 'path';
import { spawn } from 'child_process';
import type { LanguageClient } from 'vscode-languageclient/node';
import { clerkPath, getCwd } from '../shared/util_client';
import { exceptionsAt } from './lspRequests';
import type { ExceptionsArgs } from './lspRequests';
import { getClient } from '../extension';

export type RulePos = {
  filename: string;
  start_line: number;
  start_column: number;
  end_line: number;
  end_column: number;
  law_headings?: string[];
};
export type ExceptionRule = { pos: RulePos; condition_text?: string };
export type ExceptionNode = {
  label: string;
  rules: ExceptionRule[];
  exceptions: ExceptionNode[];
};
export type ExceptionsResult = {
  scope: string;
  variable: string;
  is_condition: boolean;
  trees: ExceptionNode[];
  declPos: RulePos;
};

type OpenMsg = {
  command: 'open';
  file: string;
  line: number;
  col: number;
  endLine: number;
  endCol: number;
};

function escapeHtml(s: string): string {
  return s
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}

function posAttrs(pos: RulePos): string {
  return `data-file="${escapeHtml(pos.filename)}" data-line="${pos.start_line}" data-col="${pos.start_column}" data-endline="${pos.end_line}" data-endcol="${pos.end_column}"`;
}

/** Renders an exception tree node as HTML inside a <pre>, mimicking the CLI
 *  box-drawing format. Positions are rendered as clickable <span> elements. */
function renderExceptionNode(
  node: ExceptionNode,
  prefix: string,
  isLast: boolean,
  depth: number
): string {
  const connector = depth === 0 ? '' : isLast ? '└── ' : '├── ';
  const continuation = depth === 0 ? '' : isLast ? '    ' : '│   ';
  const childPfx = prefix + continuation;
  const hasChildren = node.exceptions.length > 0;
  const bar = hasChildren ? '│' : ' ';

  // Label: "label_name"
  let html = `${escapeHtml(prefix + connector)}<span class="label">&quot;${escapeHtml(node.label)}&quot;</span>`;

  // Rules: ➤ position [condition] or (always)
  for (const rule of node.rules) {
    const shortName = rule.pos.filename.replace(/.*\//, '');
    const posStr = `${shortName}:${rule.pos.start_line}:${rule.pos.start_column}`;
    const posSpan = `<span class="pos" ${posAttrs(rule.pos)}>${escapeHtml(posStr)}</span>`;
    const heading = rule.pos.law_headings;
    const headingHtml =
      heading && heading.length > 0
        ? ` <span class="heading" title="${escapeHtml(heading.join(' › '))}">${escapeHtml(heading[heading.length - 1])}</span>`
        : '';
    // Condition starts on its own line; continuation lines align after '['.
    const condPrefix = escapeHtml(childPfx + bar + '   ');
    const cond = rule.condition_text
      ? '\n' +
      condPrefix +
      '[' +
      rule.condition_text
        .split('\n')
        .map((l) => `<span class="condition">${escapeHtml(l)}</span>`)
        .join('\n' + condPrefix + ' ') +
      ']'
      : ` <span class="always">${escapeHtml(vscode.l10n.t('(always)'))}</span>`;
    html += `\n${escapeHtml(childPfx + bar + ' ➤ ')}${posSpan}${headingHtml}${cond}`;
  }

  // Children with box-drawing connectors
  const lastIdx = node.exceptions.length - 1;
  for (let i = 0; i <= lastIdx; i++) {
    html +=
      '\n' +
      renderExceptionNode(
        node.exceptions[i],
        childPfx,
        i === lastIdx,
        depth + 1
      );
  }

  return html;
}

function renderExceptionsWebview(
  result: ExceptionsResult,
  nonce: string
): string {
  let treeContent: string;
  if (result.trees.length === 0) {
    treeContent = `<span class="empty">${escapeHtml(vscode.l10n.t('No definitions found.'))}</span>`;
  } else if (result.is_condition) {
    // Condition-typed: show "(default: false)" as root, trees as children
    const lastIdx = result.trees.length - 1;
    const childrenHtml = result.trees
      .map((t, i) => '\n' + renderExceptionNode(t, '', i === lastIdx, 1))
      .join('');
    treeContent =
      `<span class="default-false">${escapeHtml(vscode.l10n.t('(default: false)'))}</span>` +
      childrenHtml;
  } else {
    // Non-condition: each tree separated by a blank line
    treeContent = result.trees
      .map((t) => renderExceptionNode(t, '', true, 0))
      .join('\n\n');
  }

  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta http-equiv="Content-Security-Policy" content="default-src 'none'; script-src 'nonce-${nonce}'; style-src 'unsafe-inline';">
  <style>
    body { font-family: 'Segoe UI', sans-serif; padding: 1em 1.5em; font-size: 13px; color: var(--vscode-foreground); background: var(--vscode-editor-background); }
    h1 { font-size: 1em; font-weight: normal; margin: 0 0 1em; }
    h1 code { font-weight: bold; }
    pre { font-family: var(--vscode-editor-font-family, monospace); font-size: var(--vscode-editor-font-size, 13px); line-height: 1; white-space: pre; margin: 0; }
    .label { color: var(--vscode-symbolIcon-classForeground, #569cd6); font-weight: bold; }
    .default-false { color: var(--vscode-charts-yellow, #d7ba7d); }
    .pos { color: var(--vscode-textLink-foreground, #4ec9b0); cursor: pointer; text-decoration: underline; }
    .pos:hover { color: var(--vscode-textLink-activeForeground); }
    .condition { color: var(--vscode-symbolIcon-variableForeground, #9cdcfe); }
    .always { color: var(--vscode-descriptionForeground); font-style: italic; }
    .heading { color: var(--vscode-descriptionForeground); font-style: italic; margin-left: 1ch; }
    .empty { color: var(--vscode-descriptionForeground); }
  </style>
</head>
<body>
  <h1>${escapeHtml(vscode.l10n.t('Definition tree for'))} <code><span class="pos" ${posAttrs(result.declPos)}>${escapeHtml(result.variable)}</span></code> ${escapeHtml(vscode.l10n.t('in scope'))} <code>${escapeHtml(result.scope)}</code></h1>
  <pre>${treeContent}</pre>
  <script nonce="${nonce}">
    const vscode = acquireVsCodeApi();
    document.querySelectorAll('.pos').forEach(span => {
      span.addEventListener('click', () => {
        vscode.postMessage({
          command: 'open',
          file: span.dataset.file,
          line: parseInt(span.dataset.line),
          col: parseInt(span.dataset.col),
          endLine: parseInt(span.dataset.endline),
          endCol: parseInt(span.dataset.endcol),
        });
      });
    });
  </script>
</body>
</html>`;
}

function handleOpenMsg(msg: OpenMsg, cwd: string): void {
  const absFile = path.isAbsolute(msg.file)
    ? msg.file
    : path.resolve(cwd, msg.file);
  const fileUri = vscode.Uri.file(absFile);
  vscode.window.showTextDocument(fileUri, {
    selection: new vscode.Range(
      msg.line - 1,
      msg.col - 1,
      msg.endLine - 1,
      msg.endCol - 1
    ),
    preserveFocus: true,
  });
}

class ExceptionsViewProvider implements vscode.WebviewViewProvider {
  private _view?: vscode.WebviewView;
  private _pendingResult?: ExceptionsResult;
  private _cwd: string = '';

  resolveWebviewView(webviewView: vscode.WebviewView): void {
    this._view = webviewView;
    webviewView.webview.options = { enableScripts: true };
    if (this._pendingResult) {
      this._setContent(this._pendingResult);
      this._pendingResult = undefined;
    }
    webviewView.webview.onDidReceiveMessage((msg: OpenMsg) => {
      if (msg.command === 'open') handleOpenMsg(msg, this._cwd);
    });
  }

  async show(result: ExceptionsResult, cwd: string): Promise<void> {
    this._cwd = cwd;
    // Reveal the panel view (triggers resolveWebviewView if not yet done)
    await vscode.commands.executeCommand('catala.exceptionsView.focus');
    if (this._view) {
      this._setContent(result);
    } else {
      // resolveWebviewView hasn't fired yet; set pending so it picks it up
      this._pendingResult = result;
    }
  }

  private _setContent(result: ExceptionsResult): void {
    if (!this._view) return;
    const nonce = Array.from({ length: 32 }, () =>
      'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'.charAt(
        Math.floor(Math.random() * 62)
      )
    ).join('');
    this._view.title = `${result.variable} (${result.scope})`;
    this._view.webview.html = renderExceptionsWebview(result, nonce);
  }
}

export const exceptionsViewProvider = new ExceptionsViewProvider();

export async function showExceptions(args: ExceptionsArgs): Promise<void> {
  const {
    uri,
    scope,
    variable,
    declFile,
    declLine,
    declCol,
    declEndLine,
    declEndCol,
  } = args;
  const cwd = getCwd(uri) ?? path.dirname(uri);

  let jsonOutput = '';
  let stderrOutput = '';
  try {
    await new Promise<void>((resolve, reject) => {
      const proc = spawn(
        clerkPath,
        [
          'exceptions',
          uri,
          '--scope',
          scope,
          '--variable',
          variable,
          '--output-format=json',
        ],
        { cwd }
      );
      proc.stdout.on('data', (data: Buffer) => {
        jsonOutput += data.toString();
      });
      proc.stderr.on('data', (data: Buffer) => {
        stderrOutput += data.toString();
      });
      proc.on('close', (code) => {
        if (code === 0) resolve();
        else
          reject(new Error(stderrOutput || `clerk exited with code ${code}`));
      });
      proc.on('error', reject);
    });
  } catch (err) {
    vscode.window.showErrorMessage(
      vscode.l10n.t('clerk exceptions failed: {0}', String(err))
    );
    return;
  }

  let result: ExceptionsResult;
  try {
    const parsed = JSON.parse(jsonOutput) as Omit<ExceptionsResult, 'declPos'>;
    result = {
      ...parsed,
      declPos: {
        filename: declFile,
        start_line: declLine,
        start_column: declCol,
        end_line: declEndLine,
        end_column: declEndCol,
      },
    };
  } catch {
    vscode.window.showErrorMessage(
      vscode.l10n.t('Failed to parse clerk exceptions output as JSON.')
    );
    return;
  }

  await exceptionsViewProvider.show(result, path.dirname(uri));
}

export async function showExceptionsAtCursor(
): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) return;
  const args = await exceptionsAt(
    editor.document.uri,
    editor.selection.active
  );
  if (!args) {
    vscode.window.setStatusBarMessage(
      vscode.l10n.t('No scope variable at cursor position.'),
      3000
    );
    return;
  }
  await showExceptions(args);
}
