import * as vscode from 'vscode';
import { execFile } from 'node:child_process';
import { promisify } from 'node:util';
import * as path from 'node:path';
import { catalaPath, getCwd } from '../shared/util_client';
import type { RunArgs } from '../shared/util_client';

const execFileAsync = promisify(execFile);

// stdout threshold for graphs (default is 1Mo).
const MAX_DOT_BUFFER = 64 * 1024 * 1024;

// custom URL scheme passed as `catala explain --url-base`, so node links point
// to `<url-base>/<file><line-suffix>`
const CATALA_OPEN_SCHEME = 'catala-open:';

function parseCatalaOpenHref(
  href: string | undefined
): { file: string; line?: number } | null {
  if (!href?.startsWith(CATALA_OPEN_SCHEME)) return null;
  let rest = href.slice(CATALA_OPEN_SCHEME.length);
  let line: number | undefined;
  const m = /#L(\d+)$/.exec(rest);
  if (m) {
    line = Number.parseInt(m[1]);
    rest = rest.slice(0, m.index);
  }
  const file = rest.replace(/^\//, '');
  if (!file) return null;
  return { file, line };
}

async function openSourceFile(
  href: string,
  cwd: string | undefined
): Promise<void> {
  const parsed = parseCatalaOpenHref(href);
  if (!parsed) return;
  const filePath =
    path.isAbsolute(parsed.file) || !cwd
      ? parsed.file
      : path.join(cwd, parsed.file);
  const line = parsed.line && parsed.line > 0 ? parsed.line - 1 : 0;
  const position = new vscode.Position(line, 0);
  try {
    await vscode.window.showTextDocument(vscode.Uri.file(filePath), {
      selection: new vscode.Range(position, position),
      preview: false,
    });
  } catch {
    vscode.window.showErrorMessage(`Catala explain: cannot open '${filePath}'`);
  }
}

type PanelState = { panel: vscode.WebviewPanel; dot: string };
const panels = new Map<string, PanelState>();

function isDarkTheme(): boolean {
  const kind = vscode.window.activeColorTheme.kind;
  return (
    kind === vscode.ColorThemeKind.Dark ||
    kind === vscode.ColorThemeKind.HighContrast
  );
}

async function computeDot(args: RunArgs): Promise<string> {
  const { stdout } = await execFileAsync(
    catalaPath,
    [
      'explain',
      args.uri,
      '--scope',
      args.scope,
      '--dot',
      '--theme',
      isDarkTheme() ? 'dark' : 'light',
      '--url-base',
      CATALA_OPEN_SCHEME,
    ],
    { cwd: getCwd(args.uri), maxBuffer: MAX_DOT_BUFFER }
  );
  return stdout;
}

function getHtmlForWebview(
  context: vscode.ExtensionContext,
  webview: vscode.Webview
): string {
  const scriptUri = webview.asWebviewUri(
    vscode.Uri.joinPath(context.extensionUri, 'dist', 'ui.js')
  );
  return `<!doctype html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Catala Explain</title>
  <style>
    html, body { height: 100%; margin: 0; padding: 0; overflow: hidden; }
    #graph { width: 100vw; height: 100vh; }
    #graph svg { width: 100%; height: 100%; }
    /* Shrink only the label text; node boxes keep their Graphviz layout size. */
    #graph svg text { font-size: 10px; }
    #error {
      margin: 0;
      padding: 1rem;
      white-space: pre-wrap;
      font-family: var(--vscode-editor-font-family), monospace;
      color: var(--vscode-errorForeground);
    }
  </style>
</head>
<body>
  <div id="graph"></div>
  <pre id="error" hidden></pre>
  <script src="${scriptUri}"></script>
  <script>window.Ui.renderExplainGraph();</script>
</body>
</html>`;
}

export async function showExplainGraph(
  context: vscode.ExtensionContext,
  args: RunArgs
): Promise<void> {
  let dot: string;
  try {
    dot = await computeDot(args);
  } catch (e) {
    const err = e as { stderr?: string; message?: string };
    const detail = err.stderr?.trim() ?? err.message;
    vscode.window.showErrorMessage(
      `Catala explain failed for scope '${args.scope}': ${detail}`
    );
    return;
  }

  const key = `${args.uri}::${args.scope}`;
  const existing = panels.get(key);
  if (existing) {
    existing.dot = dot;
    existing.panel.reveal(
      existing.panel.viewColumn ?? vscode.ViewColumn.Active
    );
    void existing.panel.webview.postMessage({ kind: 'renderDot', dot });
    return;
  }

  const panel = vscode.window.createWebviewPanel(
    'catala.explainGraph',
    `Explain: ${args.scope}`,
    vscode.ViewColumn.Active,
    { enableScripts: true, retainContextWhenHidden: true }
  );
  const state: PanelState = { panel, dot };
  panels.set(key, state);
  panel.onDidDispose(() => panels.delete(key));

  const cwd = getCwd(args.uri);
  panel.webview.onDidReceiveMessage(
    (message: { kind?: string; href?: string }) => {
      if (message?.kind === 'ready') {
        void panel.webview.postMessage({ kind: 'renderDot', dot: state.dot });
      } else if (message?.kind === 'openLink' && message.href) {
        void openSourceFile(message.href, cwd);
      }
    }
  );

  panel.webview.html = getHtmlForWebview(context, panel.webview);
}
