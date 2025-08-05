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

export function activate(context: vscode.ExtensionContext): void {
  const factory: vscode.DebugAdapterDescriptorFactory = {
    createDebugAdapterDescriptor(
      _session: vscode.DebugSession,
      _executable: vscode.DebugAdapterExecutable | undefined
    ): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
      return new vscode.DebugAdapterServer(8730, 'localhost');
    },
  };

  context.subscriptions.push(
    vscode.debug.registerDebugAdapterDescriptorFactory(
      'catala-debugger',
      factory
    )
  );

  vscode.commands.registerCommand('catala.debugScope', async (args) => {
    const file = args.uri;
    const scope = args.scope;
    const workspace = vscode.workspace.getWorkspaceFolder(
      vscode.Uri.parse(file)
    );
    const config: vscode.DebugConfiguration = {
      type: 'catala-debugger',
      request: 'attach',
      name: `Debug: ${scope}`,
      args,
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
    term.sendText([clerkPath, 'run', file, '--scope', scope].join(' ')); // TODO: output in termninal to get fancy colors !
  });

  const lsp_server_config_path = vscode.workspace
    .getConfiguration('catala')
    .get<string>('lspServerPath');
  const is_binary_path_configured =
    lsp_server_config_path != undefined && lsp_server_config_path != '';

  const local_path = context.asAbsolutePath(
    path.join('_build', 'default', 'server', 'src', 'main.exe')
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
