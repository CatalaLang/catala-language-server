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

// `icon` are codicon id, the (id without the `codicon-` prefix).
// `new vscode.ThemeIcon('github')`
type ItemParam = {
  label: string;
  descr?: string | undefined;
  icon?: vscode.ThemeIcon | undefined;
  command: vscode.Command;
};

class Item extends vscode.TreeItem {
  // we'll use the file and line later...
  readonly descr: string | undefined;
  readonly icon: vscode.ThemeIcon | undefined;
  // children represent branches, which are also items
  public children: Item[] = [];

  // add all members here, file and line we'll need later
  // the label represent the text which is displayed in the tree
  // and is passed to the base class
  constructor(param: ItemParam) {
    super(param.label, vscode.TreeItemCollapsibleState.None);
    this.descr = param.descr;
    this.icon = param.icon;
    this.command = param.command;
    this.collapsibleState = vscode.TreeItemCollapsibleState.None;
  }

  // a public method to add childs, and with additional branches
  // we want to make the item collabsible
  public add_child(child: Item): void {
    this.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
    this.children.push(child);
  }
}

// 1. we'll export this class and use it in our extension later
// 2. we need to implement vscode.TreeDataProvider
export class tree_view implements vscode.TreeDataProvider<Item> {
  // m_data holds all tree items
  private switches: Item[] = [];
  // with the vscode.EventEmitter we can refresh our  tree view
  private m_onDidChangeTreeData: vscode.EventEmitter<Item | undefined> =
    new vscode.EventEmitter<Item | undefined>();
  // and vscode will access the event by using a readonly onDidChangeTreeData (this member has to be named like here, otherwise vscode doesnt update our treeview.
  readonly onDidChangeTreeData?: vscode.Event<Item | undefined> =
    this.m_onDidChangeTreeData.event;

  public constructor(switches: Item[]) {
    this.switches = switches;
  }

  // we need to implement getTreeItem to receive items from our tree view
  public getTreeItem(
    element: Item
  ): vscode.TreeItem | Thenable<vscode.TreeItem> {
    const item = new vscode.TreeItem(element.label!, element.collapsibleState);
    item.description = element.descr;
    item.iconPath = element.icon;
    item.command = element.command;
    return item;
  }

  // and getChildren
  public getChildren(element: Item | undefined): vscode.ProviderResult<Item[]> {
    if (element === undefined) {
      return this.switches;
    } else {
      return element.children;
    }
  }
}

type Binary = { path: string; version?: string };

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

function formatToolchain(entries: [string, Binary][]): string {
  return entries
    .map(([key, value]) => {
      const name = toolchainBinaryNames[key as keyof Toolchain] ?? key;
      return `•  ${name} →  ${value.path}${value.version != undefined ? ` (version ${value.version})` : ''}`;
    })
    .join('\n');
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

async function runScope(args?: RunArgs): Promise<void> {
  const inputs = args?.inputs;
  args ??= await selectScope(inputs ? true : false);
  if (args) {
    const cwd = getCwd(args.uri);
    const termName = `${args.scope} execution`;
    vscode.window.terminals.find((t) => t.name === termName)?.dispose();
    const term = vscode.window.createTerminal({
      name: termName,
      cwd,
      // Pin PowerShell on Windows so the --input quoting below is deterministic
      // (cmd.exe would need the opposite escaping).
      ...(process.platform === 'win32' && { shellPath: 'powershell.exe' }),
    });
    // Single-quote a shell argument so spaces in paths survive. PowerShell
    // escapes an embedded quote by doubling it; POSIX shells by '\''.
    const sq = (s: string): string =>
      process.platform === 'win32'
        ? `'${s.replace(/'/g, "''")}'`
        : `'${s.replace(/'/g, "'\\''")}'`;
    let extra_args: string[] = [];
    if (inputs) {
      const json = JSON.stringify(inputs);
      // Single-quote the JSON; on Windows (PowerShell) also backslash-escape the
      // inner double quotes so they survive the native-command re-parse.
      const quoted =
        process.platform === 'win32'
          ? `'${json.replace(/"/g, '\\"')}'`
          : `'${json}'`;
      extra_args = ['--input', quoted];
    }
    term.show();
    term.sendText(
      [
        clerkPath,
        'run',
        sq(args.uri),
        '--scope',
        args.scope,
        ...extra_args,
      ].join(' ')
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

export async function activate(
  context: vscode.ExtensionContext
): Promise<void> {
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

  const lsp_path = resolveBinaryPath(
    'catala-lsp',
    context,
    'main_lsp.exe',
    getConfig('lspServerPath')
  );
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
    await Promise.all([client.start(), initTests(context, client)]);
  }

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

  const language = vscode.env.language;

  let command_books: vscode.Command = {
    title: 'Open Catala book',
    command: 'vscode.open',
    arguments: [
      vscode.Uri.parse(`https://book.catala-lang.org/${language}/0-intro.html`),
    ],
  };
  let catala_books = new Item({
    label: 'Learn how to do catala',
    icon: new vscode.ThemeIcon('book'),
    command: command_books,
  });
  catala_books.iconPath;

  let command_github: vscode.Command = {
    title: 'Open Github',
    command: 'vscode.open',
    arguments: [vscode.Uri.parse(`https://github.com/CatalaLang/catala`)],
  };
  let catala_github = new Item({
    label: 'Catala Github repository',
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
