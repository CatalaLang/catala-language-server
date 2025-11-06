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

let client: LanguageClient;

function getCwd(bufferPath: string): string | undefined {
  return vscode.workspace.getWorkspaceFolder(vscode.Uri.parse(bufferPath))?.uri
    ?.fsPath;
}

function pathFromConfig(confId: string, defaultCmd: string): string {
  const confPath = vscode.workspace
    .getConfiguration('catala')
    .get<string>(confId);
  if (confPath === undefined || confPath === null || confPath.trim() === '')
    return defaultCmd;
  if (!fs.existsSync(confPath)) {
    vscode.window.showWarningMessage(
      `Could not find executable for ${confId} at ${confPath}, falling back to default`
    );
    return defaultCmd;
  }
  return confPath;
}

const clerkPath: string = pathFromConfig('clerkPath', 'clerk');

interface IRunArgs {
  uri: string;
  scope: string;
}

async function selectScope(): Promise<IRunArgs | undefined> {
  if (!client) {
    vscode.window.showErrorMessage(
      'Catala LSP is not running: cannot select a scope.'
    );
    return undefined;
  }

  const files_scopes_map: { path: string; scopes: string[] }[] =
    await client.sendRequest(
      'catala.getRunnableScopes',
      vscode.workspace.getWorkspaceFolder
    );
  const possible_files: vscode.QuickPickItem[] = [
    {
      label: 'Catala source files',
      kind: vscode.QuickPickItemKind.Separator,
    },
    ...files_scopes_map
      .map((file) => {
        return { label: file.path };
      })
      .sort((a, b) => a.label.localeCompare(b.label)),
  ];

  const file: vscode.QuickPickItem | undefined =
    await vscode.window.showQuickPick(possible_files);
  let possible_scopes: vscode.QuickPickItem[];
  if (file) {
    const file_scopes = files_scopes_map.find((f) => f.path == file.label);
    if (file_scopes) {
      possible_scopes = file_scopes.scopes.map((scope) => {
        return { label: scope };
      });
      const scopes_to_choose: vscode.QuickPickItem[] = [
        {
          label: 'Catala scopes',
          kind: vscode.QuickPickItemKind.Separator,
        },
        ...possible_scopes,
      ];
      const scope: vscode.QuickPickItem | undefined =
        await vscode.window.showQuickPick(scopes_to_choose);
      vscode.workspace.openTextDocument(vscode.Uri.file(file.label));
      if (scope) return { uri: file.label, scope: scope.label };
    }
  }
}

async function runScope(): Promise<void> {
  const args: IRunArgs | undefined = await selectScope();
  if (args) {
    const cwd = getCwd(args.uri);
    const termName = `${args.scope} execution`;
    let term = vscode.window.terminals.find((t) => t.name === termName);
    if (!term) term = vscode.window.createTerminal({ name: termName, cwd });
    term.show();
    term.sendText(
      [clerkPath, 'run', args.uri, '--scope', args.scope].join(' ')
    );
  }
}
vscode.commands.registerCommand('catala.run', runScope);

async function debugScope(): Promise<void> {
  const args: IRunArgs | undefined = await selectScope();
  if (args) {
    await vscode.debug.startDebugging(undefined, {
      name: 'Run Catala program',
      type: 'catala-debugger', // ou "extensionHost" si c'est pour développer l'extension
      request: 'launch',
      args: args,
      stopOnEntry: true,
    });
  }
}
vscode.commands.registerCommand('catala.debug', debugScope);

export function activate(context: vscode.ExtensionContext): void {
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

  vscode.commands.registerCommand('catala.debugScope', async (args) => {
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

  vscode.commands.registerCommand('catala.runScope', async (args) => {
    const file = args.uri;
    const scope = args.scope;
    const cwd = getCwd(file);
    const termName = `${scope} execution`;
    let term = vscode.window.terminals.find((t) => t.name === termName);
    if (!term) term = vscode.window.createTerminal({ name: termName, cwd });
    term.show();
    term.sendText([clerkPath, 'run', file, '--scope', scope].join(' '));
  });

  // Open the current resource with the custom Test Case Editor
  context.subscriptions.push(
    vscode.commands.registerCommand(
      'catala.openWithTestEditor',
      async (arg?: vscode.Uri | { resourceUri: vscode.Uri }) => {
        const uri =
          arg instanceof vscode.Uri
            ? arg
            : ((arg as any)?.resourceUri ??
              vscode.window.activeTextEditor?.document.uri);
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

  vscode.commands.registerCommand(
    'catala.getTestableScopes',
    async (workspacePath?: string) => {
      if (!client) {
        vscode.window.showErrorMessage(
          'Catala LSP is not running: cannot list testable scopes.'
        );
        return [];
      }
      const files_scopes_map: { path: string; scopes: string[] }[] =
        await client.sendRequest(
          'catala.getTestableScopes',
          workspacePath ?? null
        );
      return files_scopes_map;
    }
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
        { scheme: 'file', language: 'catala_en', pattern: '**/*.catala_en' },
        { scheme: 'file', language: 'catala_fr', pattern: '**/*.catala_fr' },
      ],
      synchronize: {
        fileEvents: [
          vscode.workspace.createFileSystemWatcher('**/*.catala_en'),
          vscode.workspace.createFileSystemWatcher('**/*.catala_fr'),
        ],
      },
    };
    client = new LanguageClient(
      'catala-lsp',
      'Catala Language Server Protocol',
      serverOptions,
      clientOptions
    );
    client.start();
  }

  // Always register the custom editor provider
  context.subscriptions.push(TestCaseEditorProvider.register(context));

  // Ensure the logger is disposed when the extension is deactivated
  context.subscriptions.push({ dispose: () => logger.dispose() });
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
