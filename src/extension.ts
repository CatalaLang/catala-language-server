import * as vscode from 'vscode';
import type {
  Executable,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient/node';
import { LanguageClient } from 'vscode-languageclient/node';
import * as path from 'path';
import { TestCaseEditorProvider } from './testCaseEditor';
import { logger } from './logger';
import * as fs from 'fs';
import cmd_exists from 'command-exists';
import * as net from 'net';
import { spawn } from 'child_process';
import { clerkPath, getCwd, hasResourceUri } from './util_client';
import { initTests } from './testAndCoverage';
import type { CatalaEntrypoint } from './lspRequests';
import { listEntrypoints } from './lspRequests';
import { ScopeInputController } from './scope_input_editor/ScopeInputController';

let client: LanguageClient;

interface IRunArgs {
  uri: string;
  scope: string;
  inputs?: JSON;
}

async function selectScope(with_inputs: boolean): Promise<IRunArgs | undefined> {
  if (!client) {
    vscode.window.showErrorMessage(
      'Catala LSP is not running: cannot select a scope.'
    );
    return undefined;
  }
  const entrypoints: Array<CatalaEntrypoint> = await listEntrypoints(
    client,
    with_inputs ? [{ kind: 'InputScope' }] : [{ kind: 'Test' }, { kind: 'NoInputScope' }],
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
      }, [])!
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

    if (scope) return { uri: file.label, scope: scope.label };
  }
}

async function runScope(args?: IRunArgs): Promise<void> {
  const inputs = args?.inputs
  args ??= await selectScope(inputs ? true : false);
  logger.log("calling runscope with " + JSON.stringify(inputs));
  if (args) {
    const cwd = getCwd(args.uri);
    const termName = `${args.scope} execution`;
    const extra_args = inputs ? ['--input', `'${JSON.stringify(inputs)}'`] : []
    let term = vscode.window.terminals.find((t) => t.name === termName);
    term ??= vscode.window.createTerminal({ name: termName, cwd });
    term.show();
    term.sendText(
      [clerkPath, 'run', args.uri, '--scope', args.scope, ...extra_args].join(' ')
    );
  }
}

vscode.commands.registerCommand('catala.run', runScope);
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

async function debugScope(): Promise<void> {
  const args: IRunArgs | undefined = await selectScope(false);
  if (args) {
    await vscode.debug.startDebugging(undefined, {
      name: 'Run Catala program',
      type: 'catala-debugger',
      request: 'launch',
      args: args,
      stopOnEntry: true,
    });
  }
}
vscode.commands.registerCommand('catala.debug', debugScope);

export async function activate(
  context: vscode.ExtensionContext
): Promise<void> {
  vscode.debug.registerDebugAdapterDescriptorFactory('catala-debugger', {
    createDebugAdapterDescriptor(_session) {
      const local_path = path.join(
        '_build',
        'default',
        'server',
        'src',
        'main_dap.exe'
      );
      let dap_command;
      if (fs.existsSync(local_path)) {
        dap_command = context.asAbsolutePath(local_path);
      } else if (cmd_exists.sync('catala-dap')) {
        dap_command = 'catala-dap';
      } else if (cmd_exists.sync('catala-dap.exe')) {
        dap_command = 'catala-dap.exe';
      } else {
        vscode.window.showErrorMessage(
          'Catala debugger not found on the system. Please refer to the installation procedure https://github.com/CatalaLang/catala-language-server/?tab=readme-ov-file#installation'
        );
        return undefined;
      }
      const server = net.createServer((socket) => {
        const adapter = spawn(dap_command, []);
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
    },
  });

  vscode.commands.registerCommand('catala.debugScope', async (args: IRunArgs) => {
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
      vscode.window.showErrorMessage('Fail to start a debugging session');
    }
  });

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

  const lsp_server_config_path = vscode.workspace
    .getConfiguration('catala')
    .get<string>('lspServerPath');
  const is_binary_path_configured =
    lsp_server_config_path != undefined && lsp_server_config_path != '';

  const local_path = context.asAbsolutePath(
    path.join('_build', 'default', 'server', 'src', 'main_lsp.exe')
  );
  const lsp_binary_prefix = 'catala-lsp';
  let lsp_binary = lsp_binary_prefix;

  const is_local_binary_present = fs.existsSync(local_path);

  const configured_binary_exists =
    is_binary_path_configured && fs.existsSync(lsp_server_config_path!);
  const is_configured_binary_present = configured_binary_exists;
  if (is_binary_path_configured && !configured_binary_exists) {
    vscode.window.showErrorMessage(
      "Configured LSP path (catala.lspServerPath): '" +
      lsp_server_config_path +
      "' not found. Using default values..."
    );
  }

  let is_binary_in_path = false;
  if (cmd_exists.sync(lsp_binary_prefix)) {
    is_binary_in_path = true;
  } else {
    lsp_binary = lsp_binary_prefix + '.exe';
    is_binary_in_path = cmd_exists.sync(lsp_binary);
  }

  if (
    !is_binary_in_path &&
    !is_local_binary_present &&
    !is_configured_binary_present
  ) {
    const err_msg =
      'Catala LSP not found on the system: please refer to the installation procedure https://github.com/CatalaLang/catala-language-server/?tab=readme-ov-file#installation';
    vscode.window.showErrorMessage(err_msg);
  } else {
    let cmd;
    if (is_configured_binary_present) {
      cmd = lsp_server_config_path!;
    } else if (is_local_binary_present) {
      cmd = local_path;
    } else {
      cmd = lsp_binary;
    }
    const run: Executable = { command: cmd };
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


  // register_memoryFileProvider(context);

  context.subscriptions.push(
    vscode.commands.registerCommand(
      'catala.openWithScopeInputEditor',
      async (x?: IRunArgs) => {
        if (x == undefined) {
          const y = await selectScope(true);
          if (y == undefined) return;
          x = y
        }
        const inputWebView = new ScopeInputController()
        inputWebView.createWebview(context, x.uri, x.scope)
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
