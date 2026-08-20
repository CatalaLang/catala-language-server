import * as vscode from 'vscode';
import type {
  Executable,
  LanguageClientOptions,
  ServerOptions,
  Command,
} from 'vscode-languageclient/node';
import { LanguageClient } from 'vscode-languageclient/node';
import { TestCaseEditorProvider } from './extension/testCaseEditorProvider';
import { TraceEditorProvider } from './extension/traceEditorProvider';
import { initTraceCache } from './trace-editor/traceRunner';
// Emitted to dist as `codicon.css`; linked into the trace-editor webview so the
// vscode-elements icon component can find the Codicons font.
import codiconsCssPath from '@vscode/codicons/dist/codicon.css?url';
import { logger } from './extension/logger';
import * as net from 'net';
import { tmpdir } from 'os';
import path, { join } from 'path';
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
  tryBinaryPath,
} from './shared/util_client';
import type { Binary, RunArgs } from './shared/util_client';
import { initTests, ResultController } from './extension/testAndCoverage';
import type { CatalaEntrypoint } from './extension/lspRequests';
import { listEntrypoints } from './extension/lspRequests';
import { ScopeInputController } from './scope-editor/ScopeInputController';
import { TestMacroController } from './test-case-editor/TestMacroController';

type ItemParam = {
  label: string;
  descr?: string | undefined;
  icon?: vscode.ThemeIcon | undefined;
  command?: vscode.Command | undefined;
};

class Item extends vscode.TreeItem {
  readonly descr: string | undefined;
  readonly icon: vscode.ThemeIcon | undefined;
  public children: Item[] = [];

  constructor(param: ItemParam) {
    super(param.label, vscode.TreeItemCollapsibleState.None);
    this.descr = param.descr;
    this.icon = param.icon;
    this.command = param.command;
    this.collapsibleState = vscode.TreeItemCollapsibleState.None;
  }

  public add_child(child: Item): void {
    this.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
    this.children.push(child);
  }
}

export class tree_view implements vscode.TreeDataProvider<Item> {
  private switches: Item[] = [];
  private m_onDidChangeTreeData: vscode.EventEmitter<Item | undefined> =
    new vscode.EventEmitter<Item | undefined>();
  readonly onDidChangeTreeData?: vscode.Event<Item | undefined> =
    this.m_onDidChangeTreeData.event;
  private refresher?: () => Promise<Item[]>;

  public constructor(switches: Item[], refresher?: () => Promise<Item[]>) {
    this.switches = switches;
    this.refresher = refresher;
  }

  public getTreeItem(
    element: Item
  ): vscode.TreeItem | Thenable<vscode.TreeItem> {
    const item = new vscode.TreeItem(element.label!, element.collapsibleState);
    item.description = element.descr;
    item.iconPath = element.icon;
    item.command = element.command;
    return item;
  }

  public getChildren(element: Item | undefined): vscode.ProviderResult<Item[]> {
    if (element === undefined) {
      return this.switches;
    } else {
      return element.children;
    }
  }

  public async refresh(): Promise<void> {
    if (this.refresher) {
      this.switches = await this.refresher();
      this.m_onDidChangeTreeData.fire(undefined);
    }
  }
}

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

function asyncRun(
  command: string,
  args: string[],
  cwd: string | undefined
): Promise<void> {
  return new Promise((resolve, reject) => {
    const options = cwd ? { cwd } : undefined;
    const proc = spawn(command, args, options);
    proc.stdout.on('data', (data: Buffer) => {
      logger.log(data.toString());
    });
    proc.stderr.on('data', (data: Buffer) => {
      logger.log(data.toString());
    });
    proc.on('error', reject);
    proc.on('close', () => resolve());
  });
}

async function runScope(args?: RunArgs): Promise<void> {
  const inputs = args?.inputs;
  args ??= await selectScope(inputs ? true : false);
  if (!args) {
    return;
  }
  const cwd = getCwd(args.uri);
  // Single-quote a shell argument so spaces in paths survive. PowerShell
  // escapes an embedded quote by doubling it; POSIX shells by '\''.
  const sq = (s: string): string =>
    args.headless
      ? s
      : process.platform === 'win32'
        ? `'${s.replace(/'/g, "''")}'`
        : `'${s.replace(/'/g, "'\\''")}'`;

  let inputArgs: string[] = [];
  if (inputs) {
    const json = JSON.stringify(inputs);
    // Single-quote the JSON; on Windows (PowerShell) also backslash-escape the
    // inner double quotes so they survive the native-command re-parse.
    const input = args.headless
      ? json
      : process.platform === 'win32'
        ? `'${json.replace(/"/g, '\\"')}'`
        : `'${json}'`;
    inputArgs = ['--input', input];
  }

  let traceOutputFile = args.traceOutputFile;
  if (args.withTrace && traceOutputFile === undefined) {
    traceOutputFile = join(tmpdir(), `${args.scope}_trace.json`);
  }
  const traceArgs =
    args.withTrace && traceOutputFile !== undefined
      ? ['--trace', traceOutputFile]
      : [];
  const buildDirArgs = args.buildDir ? ['--build-dir', sq(args.buildDir)] : [];
  const ninjaOutputArgs = args.ninjaOutput
    ? ['--ninja-output-file', sq(args.ninjaOutput)]
    : [];

  const clerkArgs = [
    'run',
    sq(args.uri),
    '--scope',
    args.scope,
    ...inputArgs,
    ...traceArgs,
    ...buildDirArgs,
    ...ninjaOutputArgs,
  ];

  if (args.headless) {
    await asyncRun(clerkPath, clerkArgs, cwd);
  } else {
    const termName = `${args.scope} ${args.withTrace ? 'trace' : 'execution'}`;
    vscode.window.terminals.find((t) => t.name === termName)?.dispose();
    const term = vscode.window.createTerminal({
      name: termName,
      cwd,
      // Pin PowerShell on Windows so the --input quoting below is deterministic
      // (cmd.exe would need the opposite escaping).
      ...(process.platform === 'win32' && { shellPath: 'powershell.exe' }),
    });
    term.show();
    term.sendText([clerkPath, ...clerkArgs].join(' '));
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
    // Uri.file, not Uri.parse: args.uri is an OS path.
    const workspace = vscode.workspace.getWorkspaceFolder(
      vscode.Uri.file(file)
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

async function opamSwitch(): Promise<string[]> {
  return new Promise((resolve) => {
    let proc = spawn('opam', ['switch', 'list', '-s']);
    let ocamlSwitch: string | undefined;

    proc.stdout.on('data', (data) => {
      ocamlSwitch = data.toString();
    });

    proc.on('close', (code: number | null) => {
      if (code != null && code == 0) {
        let splitted = ocamlSwitch?.split('\n');
        resolve(splitted ?? []);
      } else {
        resolve([]);
      }
    });
  });
}

type Toolchain = {
  catalaPath?: Binary;
  clerkPath?: Binary;
  catalaFormatPath?: Binary;
  lspServerPath?: Binary;
};

// Binary name behind each setting: friendlier to read than the setting key in
// the confirmation modal.
const toolchainBinaryNames: Record<keyof Toolchain, string> = {
  catalaPath: 'catala',
  clerkPath: 'clerk',
  catalaFormatPath: 'catala-format',
  lspServerPath: 'catala-lsp',
};

/**
 * Renders toolchain entries as a bulleted list for a modal 'detail' field. That
 * field is plain text in a proportional font, so padding cannot be used to line
 * columns up: each entry gets its own bullet instead, with the version between
 * the binary name and its path.
 */
function formatToolchain(entries: [string, Binary][]): string {
  return entries
    .map(([key, value]) => {
      const name = toolchainBinaryNames[key as keyof Toolchain] ?? key;
      return `•  ${name} →  ${value.path}${value.version != undefined ? ` (version ${value.version})` : ''}`;
    })
    .join('\n');
}

async function createItemSwitch(
  title: string,
  singlePath: string
): Promise<Item | undefined> {
  const [catala, clerk, catalaFormat, lsp] = await Promise.all([
    tryBinaryPath('catala', singlePath),
    tryBinaryPath('clerk', singlePath),
    tryBinaryPath('catala-format', singlePath),
    //  opam show catala-lsp --switch=/home/arnaud/catala-pj --field version --raw
    // We can use this command on opam switch to get the lsp version
    tryBinaryPath('catala-lsp', singlePath, true),
  ]);

  const toolchain: Toolchain = {
    ...(catala && { catalaPath: catala }),
    ...(clerk && { clerkPath: clerk }),
    ...(catalaFormat && { catalaFormatPath: catalaFormat }),
    ...(lsp && { lspServerPath: lsp }),
  };

  const keys = Object.keys(toolchain);
  if (keys.length === 0) return undefined;

  let commandUpdateToolchain: (toolchain: Toolchain) => vscode.Command = (
    toolchain: Toolchain
  ): Command => {
    return {
      title: 'Update toolchain',
      command: 'catala.useToolchain',
      arguments: [toolchain],
    };
  };

  let catalaSwitch = new Item({
    label: title,
    command: commandUpdateToolchain(toolchain),
  });
  for (const [key, value] of Object.entries(toolchain)) {
    let littleItem = new Item({
      label: value.path,
      descr: value.version,
      command: commandUpdateToolchain({ [key]: value }),
    });
    catalaSwitch.add_child(littleItem);
  }
  return catalaSwitch;
}

async function searchSwitches(): Promise<Item[]> {
  let opamItem: Item = new Item({
    label: 'Catala switches (OPAM)',
  });

  let ocamlSwitches = await opamSwitch();

  let opamRoot = await new Promise<string | undefined>((resolve) => {
    let proc = spawn('opam', ['var', 'root']);
    let root: string | undefined;
    proc.stdout.on('data', (data) => {
      root = data.toString();
    });

    proc.on('close', (code: number | null) => {
      if (code != null && code == 0) {
        resolve(root?.trim());
      } else {
        resolve(undefined);
      }
    });
  });

  for (const ocSwitch of ocamlSwitches) {
    let switchPath: string;
    if (path.isAbsolute(ocSwitch)) {
      switchPath = path.join(ocSwitch, '_opam', 'bin');
    } else if (opamRoot) {
      switchPath = path.join(opamRoot, ocSwitch, 'bin');
    } else {
      continue;
    }
    let item = await createItemSwitch(ocSwitch, switchPath);
    if (item) {
      opamItem.add_child(item);
    }
  }

  let pathItem: Item = new Item({
    label: 'Catala switches (PATH)',
  });

  let envPath = process.env.PATH;
  let currentSwitch = process.env.OPAM_SWITCH_PREFIX;
  if (envPath) {
    let paths = envPath.split(':');
    for (const singlePath of paths) {
      if (currentSwitch && singlePath.includes(currentSwitch)) continue;
      let item = await createItemSwitch(singlePath, singlePath);
      if (item) {
        pathItem.add_child(item);
      }
    }
  }
  let items = [];
  if (opamItem.children.length > 0) items.push(opamItem);
  if (pathItem.children.length > 0) items.push(pathItem);
  return items;
}

export async function activate(
  context: vscode.ExtensionContext
): Promise<void> {
  // Enable the persistent trace cache (stored under global storage).
  initTraceCache(context.globalStorageUri.fsPath);
  vscode.debug.registerDebugAdapterDescriptorFactory('catala-debugger', {
    createDebugAdapterDescriptor(_session) {
      const dap_path = resolveBinaryPath('catala-dap', context, 'main_dap.exe');
      if (dap_path) {
        const server = net.createServer((socket) => {
          const adapter = spawn(dap_path, [], {
            shell: process.platform === 'win32',
          });
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

  const ctrl = vscode.tests.createTestController('catalaTests', 'Catala Tests');
  // Placeholder to display something while tests are retrieved
  ctrl.items.add(ctrl.createTestItem('loading', 'Loading tests...'));

  const lsp_path = resolveBinaryPath(
    'catala-lsp',
    context,
    'main_lsp.exe',
    getConfig('lspServerPath')
  );

  const language = vscode.env.language;

  let resultController = new ResultController(context.workspaceState, language);
  if (lsp_path) {
    const run: Executable = {
      command: lsp_path,
      options: process.platform === 'win32' ? { shell: true } : undefined,
    };
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
        {
          scheme: 'file',
          language: 'catala_pl',
          pattern: '**/*.catala_pl{,.md}',
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
          vscode.workspace.createFileSystemWatcher(
            '**/*.{catala_pl,catala_pl.md}'
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

    let entrypointsRequest = listEntrypoints(
      client,
      [{ kind: 'GUI' }, { kind: 'Test' }],
      undefined,
      false,
      true
    ).finally(() => ctrl.items.replace([]));

    initTests(entrypointsRequest, context, client, ctrl, resultController);

    const macroTestsView = new TestMacroController();
    context.subscriptions.push(
      vscode.commands.registerCommand(
        'catala.debugAllTests',
        async (_arg?: vscode.Uri | { resourceUri: vscode.Uri }) => {
          const columnToShowIn = vscode.window.activeTextEditor
            ? vscode.window.activeTextEditor.viewColumn
            : undefined;
          macroTestsView.show(
            client,
            context,
            entrypointsRequest,
            resultController,
            ctrl,
            columnToShowIn
          );
        }
      )
    );
  }

  let command: Command = {
    title: language == 'fr' ? 'Vue globale des tests' : 'General tests view',
    command: 'catala.debugAllTests',
  };
  let catala_utils = new Item({
    label: language == 'fr' ? 'Ouvrir les tests' : 'Open all tests',
    icon: new vscode.ThemeIcon('beaker'),
    command,
  });
  context.subscriptions.push(
    // note: we need to provide the same name here as we added in the package.json file
    vscode.window.registerTreeDataProvider(
      'catala.openAllTests',
      new tree_view([catala_utils])
    )
  );

  logger.log(`Register "Catala Tests" data in th Tree data provider`);

  vscode.commands.registerCommand(
    'catala.useToolchain',
    async (toolchain: Toolchain) => {
      let entries = Object.entries(toolchain);
      const yes: vscode.MessageItem = { title: vscode.l10n.t('Yes') };
      // isCloseAffordance makes 'No' replace the Cancel button VSCode adds to
      // every modal, instead of sitting next to it.
      const no: vscode.MessageItem = {
        title: vscode.l10n.t('No'),
        isCloseAffordance: true,
      };
      const answer = await vscode.window.showWarningMessage(
        vscode.l10n.t('You are about to change Catala user settings'),
        {
          modal: true,
          detail: `${vscode.l10n.t(
            'The following settings will be updated:'
          )}\n\n
            ${formatToolchain(entries)})}`,
        },
        yes,
        no
      );
      if (answer !== yes) {
        return;
      }
      const cfg = vscode.workspace.getConfiguration('catala');
      for (const [key, value] of entries) {
        await cfg.update(key, value.path);
      }
      await vscode.window.showInformationMessage(
        vscode.l10n.t('Settings changed !'),
        {
          modal: true,
          detail: vscode.l10n.t(
            'Your settings were changed, reload the window to notice some changes'
          ),
        }
      );
    }
  );

  let items = await searchSwitches();

  let switchTree = new tree_view(items, searchSwitches);

  vscode.commands.registerCommand('catala.refreshSwitches', async () => {
    await switchTree.refresh();
  });

  context.subscriptions.push(
    // note: we need to provide the same name here as we added in the package.json file
    vscode.window.registerTreeDataProvider('catala.switches', switchTree)
  );

  let command_books: Command = {
    title: language == 'fr' ? 'Ouvrir le livre Catala' : 'Open Catala book',
    command: 'vscode.open',
    arguments: [
      vscode.Uri.parse(`https://book.catala-lang.org/${language}/0-intro.html`),
    ],
  };
  let catala_books = new Item({
    label:
      language == 'fr'
        ? 'Apprendre à faire du Catala'
        : 'Learn how to do Catala',
    icon: new vscode.ThemeIcon('book'),
    command: command_books,
  });

  let command_github: Command = {
    title: language == 'fr' ? 'Ouvrir Github' : 'Open Github',
    command: 'vscode.open',
    arguments: [vscode.Uri.parse(`https://github.com/CatalaLang/catala`)],
  };
  let catala_github = new Item({
    label:
      language == 'fr'
        ? 'Répertoire Github Catala'
        : 'Catala Github repository',
    icon: new vscode.ThemeIcon('github'),
    command: command_github,
  });
  context.subscriptions.push(
    // note: we need to provide the same name here as we added in the package.json file
    vscode.window.registerTreeDataProvider(
      'catala.help',
      new tree_view([catala_books, catala_github])
    )
  );
  logger.log(
    `Register "Catala Help and feedback" data in th Tree data provider`
  );

  // Always register the custom editor providers
  context.subscriptions.push(
    TestCaseEditorProvider.register(context, codiconsCssPath, resultController)
  );
  logger.log(`Register "Catala Test case editor"`);

  context.subscriptions.push(
    TraceEditorProvider.register(context, () => client, codiconsCssPath)
  );
  logger.log(`Register "Catala Trace Editor"`);

  context.subscriptions.push(
    vscode.commands.registerCommand(
      'catala.openWithTraceEditor',
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
          TraceEditorProvider.viewType
        );
      }
    )
  );

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
  logger.log(`Register "Catala Exception View"`);

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
  logger.log(`Activate Catala extension`);
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
