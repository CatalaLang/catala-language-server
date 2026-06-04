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
import { spawn } from 'child_process';
import {
  exceptionsViewProvider,
  showExceptions,
  showExceptionsAtCursor,
} from './extension/exceptionsView';
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
import { resolveDebugArgs } from './extension/debugConfig';

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

vscode.commands.registerCommand('catala.debug', (_ctx) => debugScope());
vscode.commands.registerCommand('catala.run', (_ctx) => runScope());
vscode.commands.registerCommand('catala.selectScope', selectScope);
vscode.commands.registerCommand('catala.debugScope', debugScope);
vscode.commands.registerCommand('catala.runScope', runScope);

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

async function debugScope(args?: RunArgs): Promise<void> {
  const inputs = args?.inputs;
  if (!args || (args && !args.scope)) {
    // Started from package.json debugging config
    args = await selectScope(inputs ? true : false);
  }
  if (args) {
    const file = args.uri;
    const scope = args.scope;
    const workspace = vscode.workspace.getWorkspaceFolder(
      vscode.Uri.parse(file)
    );
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
}

export async function activate(
  context: vscode.ExtensionContext
): Promise<void> {
  vscode.debug.registerDebugConfigurationProvider('catala-debugger', {
    resolveDebugConfiguration(
      _folder,
      config: vscode.DebugConfiguration
    ): Promise<vscode.DebugConfiguration | undefined> {
      return resolveDebugArgs(config, () => selectScope(false));
    },
  });

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
    ),
    vscode.commands.registerCommand('catala.showExceptions', showExceptions),
    vscode.commands.registerCommand('catala.showExceptionsAtCursor', () =>
      showExceptionsAtCursor(client)
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
