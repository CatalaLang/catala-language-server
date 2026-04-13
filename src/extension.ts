import * as vscode from 'vscode';
import type {
  Executable,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient/node';
import { LanguageClient } from 'vscode-languageclient/node';
import { TestCaseEditorProvider } from './extension/testCaseEditorProvider';
import { logger } from './extension/logger';
import * as net from 'net';
import * as path from 'path';
import { spawn } from 'child_process';
import {
  clerkPath,
  getConfig,
  getCwd,
  hasResourceUri,
  resolveBinaryPath,
} from './shared/util_client';
import type { RunArgs } from './shared/util_client';
import { initTests } from './extension/testAndCoverage';
import type { CatalaEntrypoint } from './extension/lspRequests';
import { listEntrypoints } from './extension/lspRequests';
import { ScopeInputController } from './scope-editor/ScopeInputController';

let client: LanguageClient;

async function selectScope(with_inputs: boolean): Promise<RunArgs | undefined> {
  if (!client) {
    vscode.window.showErrorMessage(
      'Catala LSP is not running: cannot select a scope.'
    );
    return undefined;
  }
  const entrypoints: Array<CatalaEntrypoint> = await listEntrypoints(
    client,
    with_inputs
      ? [{ kind: 'InputScope' }]
      : [{ kind: 'Test' }, { kind: 'NoInputScope' }],
    undefined,
    false,
    with_inputs ? false : true
  );
  const uniq_sorted_files: vscode.QuickPickItem[] = Array.from(
    new Set(entrypoints.map((file) => file.path))
  )
    .sort((a, b) => a.localeCompare(b))
    .map((f) => {
      return { label: f };
    });

  const file: vscode.QuickPickItem | undefined =
    await vscode.window.showQuickPick([
      {
        label: 'Catala source files',
        kind: vscode.QuickPickItemKind.Separator,
      },
      ...uniq_sorted_files,
    ]);

  if (file) {
    const init: vscode.QuickPickItem[] = [];
    const scopes: vscode.QuickPickItem[] = entrypoints
      .filter((f) => f.path == file.label)
      .reduce((acc, e) => {
        if (e.entrypoint.kind == 'Test' && e.entrypoint.value.kind == 'Test') {
          const item: vscode.QuickPickItem = {
            label: e.entrypoint.value.value.scope,
          };
          return [item, ...acc];
        } else if (e.entrypoint.kind == 'InputScope') {
          const item: vscode.QuickPickItem = {
            label: e.entrypoint.value.scope,
          };
          return [item, ...acc];
        } else {
          return acc;
        }
      }, init)!
      .reverse();

    const scopes_to_choose: vscode.QuickPickItem[] = [
      {
        label: 'Catala scopes',
        kind: vscode.QuickPickItemKind.Separator,
      },
      ...scopes,
    ];

    const scope: vscode.QuickPickItem | undefined =
      await vscode.window.showQuickPick(scopes_to_choose);
    vscode.workspace.openTextDocument(vscode.Uri.file(file.label));

    if (scope)
      return { uri: file.label, scope: scope.label, inputs: undefined };
  }
}

async function runScope(args?: RunArgs): Promise<void> {
  const inputs = args?.inputs;
  args ??= await selectScope(inputs ? true : false);
  if (args) {
    const cwd = getCwd(args.uri);
    const termName = `${args.scope} execution`;
    vscode.window.terminals.find((t) => t.name === termName)?.dispose();
    const term = vscode.window.createTerminal({ name: termName, cwd });
    const extra_args = inputs ? ['--input', `'${JSON.stringify(inputs)}'`] : [];
    term.show();
    term.sendText(
      [clerkPath, 'run', args.uri, '--scope', args.scope, ...extra_args].join(
        ' '
      )
    );
  }
}

vscode.commands.registerCommand('catala.run', (_ctx) => runScope());
vscode.commands.registerCommand('catala.selectScope', selectScope);

async function listTestableScopes(
  path: string
): Promise<Array<{ path: string; scopes: string[] }>> {
  const entrypoints = await listEntrypoints(
    client,
    [{ kind: 'InputScope' }],
    path,
    true,
    true
  );
  let m: Map<string, string[]> = new Map();
  entrypoints.forEach((e) => {
    if (e.entrypoint.kind == 'InputScope') {
      const arr = m.get(e.path) ?? [];
      m.set(e.path, [...arr, e.entrypoint.value.scope]);
    }
  });
  return Array.from(m).map((e) => {
    return { path: e[0], scopes: e[1] };
  });
}

vscode.commands.registerCommand(
  'catala.listTestableScopes',
  listTestableScopes
);

async function debugScope(args: RunArgs | undefined): Promise<void> {
  args ??= await selectScope(false);
  if (!args) return;
  const file = args.uri;
  const scope = args.scope;
  const workspace = vscode.workspace.getWorkspaceFolder(vscode.Uri.parse(file));
  const config: vscode.DebugConfiguration = {
    type: 'catala-debugger',
    request: 'launch',
    stopOnEntry: true,
    name: `Debug: ${scope}`,
    args: args,
  };
  const success = await vscode.debug.startDebugging(workspace, config);
  if (!success) {
    vscode.window.showErrorMessage('Failed to start a debugging session');
  }
}
vscode.commands.registerCommand('catala.debug', debugScope);

type ExceptionsArgs = {
  uri: string;
  scope: string;
  variable: string;
  declFile: string;
  declLine: number;
  declCol: number;
  declEndLine: number;
  declEndCol: number;
};

type RulePos = {
  filename: string;
  start_line: number;
  start_column: number;
  end_line: number;
  end_column: number;
};
type ExceptionRule = { pos: RulePos; condition_text?: string };
type ExceptionNode = {
  label: string;
  rules: ExceptionRule[];
  exceptions: ExceptionNode[];
};
type ExceptionsResult = {
  scope: string;
  variable: string;
  is_condition: boolean;
  trees: ExceptionNode[];
  declPos: RulePos;
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
    const cond = rule.condition_text
      ? ` [<span class="condition">${escapeHtml(rule.condition_text)}</span>]`
      : ` <span class="always">(always)</span>`;
    html += `\n${escapeHtml(childPfx + bar + ' ➤ ')}${posSpan}${cond}`;
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
    treeContent = '<span class="empty">No definitions found.</span>';
  } else if (result.is_condition) {
    // Condition-typed: show "(default: false)" as root, trees as children
    const lastIdx = result.trees.length - 1;
    const childrenHtml = result.trees
      .map((t, i) => '\n' + renderExceptionNode(t, '', i === lastIdx, 1))
      .join('');
    treeContent =
      `<span class="default-false">(default: false)</span>` + childrenHtml;
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
    .empty { color: var(--vscode-descriptionForeground); }
  </style>
</head>
<body>
  <h1>Definitions and exceptions for <code><span class="pos" ${posAttrs(result.declPos)}>${escapeHtml(result.variable)}</span></code> in scope <code>${escapeHtml(result.scope)}</code></h1>
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

async function showExceptions(args: ExceptionsArgs): Promise<void> {
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
    vscode.window.showErrorMessage(`clerk exceptions failed: ${err}`);
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
      'Failed to parse clerk exceptions output as JSON.'
    );
    return;
  }

  await exceptionsViewProvider.show(result, path.dirname(uri));
}

vscode.commands.registerCommand('catala.showExceptions', showExceptions);

type OpenMsg = {
  command: 'open';
  file: string;
  line: number;
  col: number;
  endLine: number;
  endCol: number;
};

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
      msg.endCol
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

const exceptionsViewProvider = new ExceptionsViewProvider();

export async function activate(
  context: vscode.ExtensionContext
): Promise<void> {
  vscode.debug.registerDebugAdapterDescriptorFactory('catala-debugger', {
    createDebugAdapterDescriptor(_session) {
      const dap_path = resolveBinaryPath('catala-dap', context, 'main_dap.exe');
      if (dap_path) {
        const server = net.createServer((socket) => {
          const adapter = spawn(dap_path, []);
          adapter.stdout.pipe(socket);
          socket.pipe(adapter.stdin);
          const output = vscode.window.createOutputChannel('Debugger Output');
          adapter.stderr.on('data', (data: Buffer) => {
            output.append(data.toString());
          });
        });
        server.listen(0);
        const port = (server.address() as net.AddressInfo).port;
        return new vscode.DebugAdapterServer(port);
      }
    },
  });

  vscode.commands.registerCommand('catala.debugScope', debugScope);

  vscode.commands.registerCommand('catala.runScope', runScope);

  // Open the current resource with the custom Test Case Editor
  context.subscriptions.push(
    vscode.commands.registerCommand(
      'catala.openWithTestEditor',
      async (arg?: vscode.Uri | { resourceUri: vscode.Uri }) => {
        const uri =
          arg instanceof vscode.Uri
            ? arg
            : hasResourceUri(arg)
              ? arg.resourceUri
              : vscode.window.activeTextEditor?.document.uri;
        if (!uri) {
          return;
        }
        await vscode.commands.executeCommand(
          'vscode.openWith',
          uri,
          'catala.testCaseEditor'
        );
      }
    )
  );

  const lsp_path = resolveBinaryPath(
    'catala-lsp',
    context,
    'main_lsp.exe',
    getConfig('lspServerPath')
  );
  if (lsp_path) {
    const run: Executable = { command: lsp_path };
    const serverOptions: ServerOptions = { run, debug: run };
    const clientOptions: LanguageClientOptions = {
      markdown: { isTrusted: true, supportHtml: true },
      documentSelector: [
        {
          scheme: 'file',
          language: 'catala_en',
          pattern: '**/*.catala_en{,.md}',
        },
        {
          scheme: 'file',
          language: 'catala_fr',
          pattern: '**/*.catala_fr{,.md}',
        },
      ],
      synchronize: {
        fileEvents: [
          vscode.workspace.createFileSystemWatcher(
            '**/*.{catala_en,catala_en.md}'
          ),
          vscode.workspace.createFileSystemWatcher(
            '**/*.{catala_fr,catala_fr.md}'
          ),
        ],
      },
    };
    client = new LanguageClient(
      'catala-lsp',
      'Catala Language Server Protocol',
      serverOptions,
      clientOptions
    );
    await Promise.all([client.start(), initTests(context, client)]);
  }

  // Always register the custom editor provider
  context.subscriptions.push(TestCaseEditorProvider.register(context));

  context.subscriptions.push(
    vscode.window.registerWebviewViewProvider(
      'catala.exceptionsView',
      exceptionsViewProvider
    )
  );

  // register_memoryFileProvider(context);

  context.subscriptions.push(
    vscode.commands.registerCommand(
      'catala.openInputEditor',
      async (x?: RunArgs) => {
        if (x == undefined) {
          const y = await selectScope(true);
          if (y == undefined) return;
          x = y;
        }
        const inputWebView = new ScopeInputController();
        inputWebView.createWebview(context, x.uri, x.scope);
      }
    )
  );

  // Ensure the logger is disposed when the extension is deactivated
  context.subscriptions.push({ dispose: () => logger.dispose() });
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
