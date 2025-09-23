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
import type { ExecException} from 'child_process';
import { execFileSync, spawn } from 'child_process';
import { basename } from 'path';
import { log } from 'console';

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

type ClerkTestResult = Array<{
  file: string;
  tests: {
    scopes: [
      {
        scope_name: string;
        success: boolean;
        time: number;
        errors: Array<{
          message: string;
          position: {
            fname: string;
            start_lnum: number;
            start_cnum: number;
            end_lnum: number;
            end_cnum: number;
          };
        }>;
      },
    ];
    'inline-tests': [{ cmd: string; success: boolean }];
  };
}>;

// TODO: handle errors
function clerkRunTest(cwd: string, uri: string[]): ClerkTestResult {
  log(
    `Running clerk test on files: ${[cwd, clerkPath, 'test', '--quiet', '--report-format', 'json'].concat(uri).join(' ')}`
  );
  let output;

  try {
    output = execFileSync(
      clerkPath,
      ['test', '--quiet', '--report-format', 'json'].concat(uri),
      { ...(cwd && { cwd }) }
    );
  } catch (e) {
    if (e.stdout) {
      output = e.stdout;
    } else {
      vscode.window.showErrorMessage(
        `Error running clerk test: ${(e as ExecException).message}`
      );
      return [];
    }
  }

  log(`Output from clerk test command: ${output.toString()}`);

  return JSON.parse(output.toString()) as ClerkTestResult;
}

async function selectScope(): Promise<IRunArgs | undefined> {
  if (!client) {
    vscode.window.showErrorMessage(
      'Catala LSP is not running: cannot select a scope.'
    );
    return undefined;
  }

  const files_scopes_map: {
    path: string;
    scopes: { name: string; range: vscode.Range }[];
  }[] = await client.sendRequest(
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
        return { label: scope.name };
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

async function initTests(
  context: vscode.ExtensionContext,
  client: LanguageClient
): Promise<void> {
  const ctrl = vscode.tests.createTestController('testController', 'Bla bla');
  context.subscriptions.push(ctrl);

  const test_scopes_map: {
    path: string;
    scopes: { name: string; range: vscode.Range }[];
  }[] = await client.sendRequest('catala.getTestScopes');
  const cwd = getCwd(test_scopes_map?.[0]?.path);

  vscode.window.showInformationMessage(
    `Found ${test_scopes_map.length} test files.`
  );

  test_scopes_map.forEach(({ path, scopes }) => {
    const uri = vscode.Uri.file(path);
    const test_item = ctrl.createTestItem(uri.path, basename(uri.path), uri);

    scopes.forEach((scope) => {
      log(`Adding test scope ${uri.path}:${scope.name}`);
      const item: vscode.TestItem = ctrl.createTestItem(
        `${uri.path}:${scope.name}`,
        scope.name,
        uri
      );
      item.range = scope.range;
      test_item.children.add(item);
    });
    ctrl.items.add(test_item);
  });

  const runHandler = async (
    request: vscode.TestRunRequest,
    _cancellation: vscode.CancellationToken
  ): Promise<void> => {
    const run = ctrl.createTestRun(request);
    vscode.window.showInformationMessage(
      `Running tests: ${request.include?.toString()}`
    );
    const testFiles =
      request.include
        ?.map(({ uri }) => uri?.path)
        ?.filter((p) => p !== undefined) ?? [];

    try {
      vscode.window.showInformationMessage(
        `Running tests in files: ${testFiles.join(', ')}`
      );
      const test_results = clerkRunTest(cwd!, testFiles);
      vscode.window.showInformationMessage(
        `Received test results for ${test_results.length} files.`
      );

      test_results.forEach(({ file, tests }) => {
        tests.scopes.forEach((scope_test_result) => {
          log(
            `Tested scope ${scope_test_result.scope_name} in file ${file}: ${
              scope_test_result.success ? 'success' : 'failure'
            }`
          );
          // find the corresponding test item
          const test_id = `${file}:${scope_test_result.scope_name}`;
          log(`Looking for test item with id ${test_id}`);
          const test_item = ctrl.items.get(file)?.children.get(test_id);

          log(`Found test item: ${test_item?.id}`);

          if (test_item) {
            run.started(test_item);
            if (scope_test_result.success) {
              run.passed(test_item, scope_test_result.time);
            } else {
              const messages = scope_test_result.errors.map((error) => {
                const msg = new vscode.TestMessage(error.message);
                msg.location = new vscode.Location(
                  vscode.Uri.file(error.position.fname),
                  new vscode.Range(
                    new vscode.Position(
                      error.position.start_lnum - 1,
                      error.position.start_cnum - 1
                    ),
                    new vscode.Position(
                      error.position.end_lnum - 1,
                      error.position.end_cnum - 1
                    )
                  )
                );
                return msg;
              });
              run.failed(test_item, messages, scope_test_result.time);
            }
          }
        });
      });
    } catch (e) {
      console.error('Error while processing test results:', e);
      vscode.window.showErrorMessage(
        'An error occurred while processing test results. Check the console for details.'
      );
    }
    run.end();
  };

  ctrl.createRunProfile(
    'Run tests',
    vscode.TestRunProfileKind.Run,
    runHandler,
    true,
    undefined,
    false
  );
}

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
    await client.start();
    await initTests(context, client);
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
