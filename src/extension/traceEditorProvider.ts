import * as vscode from 'vscode';
import { readFileSync } from 'fs';
import type { LanguageClient } from 'vscode-languageclient/node';
import { listEntrypoints } from './lspRequests';
import type { CatalaEntrypoint } from './lspRequests';
import { parseTestFile } from '../test-case-editor/testCaseCompilerInterop';
import { logger } from './logger';
import { getCwd } from '../shared/util_client';
import type { JsonValue } from '../shared/util_client';
import type {
  TraceDownMessage,
  TraceUpMessage,
} from '../trace-editor/messages';
import { readTraceFile, runTrace } from '../trace-editor/traceRunner';
import type { SettingsSource } from './settingsStore';
import { resolveLanguage } from '../shared/settings';
import type { TraceElement } from '../trace-editor/traceUtils';
import type { Test } from '../generated/catala_types';
import { writeTest } from '../generated/catala_types';
import { TestCaseEditorProvider } from './testCaseEditorProvider';

const fileLineCache = new Map<string, string>();

function extractLine(file: string, line: number): string | null {
  const key = `${file}:${line}`;
  const cached = fileLineCache.get(key);
  if (cached !== undefined) {
    return cached;
  }
  let lines: string[];
  try {
    lines = readFileSync(file, 'utf8').split(/\r?\n/);
  } catch {
    return null;
  }
  if (line < 1 || line > lines.length) {
    return null;
  }
  const text = lines[line - 1];
  fileLineCache.set(key, text);
  return text;
}

function scopeName(e: CatalaEntrypoint): string {
  const k = e.entrypoint;
  if (k.kind === 'Test') {
    return k.value.value.scope;
  } else {
    return k.value.scope;
  }
}

export type TraceEditorInputs = {
  scope?: string;
  test?: Test;
  language?: string;
  trace?: TraceElement[];
  run?: boolean;
};

export class TraceEditorProvider implements vscode.CustomTextEditorProvider {
  public static readonly viewType = 'catala.traceEditor';

  private static readonly pendingInputs = new Map<string, TraceEditorInputs>();

  private static readonly openEditors = new Map<
    string,
    {
      panel: vscode.WebviewPanel;
      sendInit: (inputs?: TraceEditorInputs) => Promise<void>;
    }
  >();

  private static activePanel: vscode.WebviewPanel | undefined;

  public static snippetMenuItem(kind: 'viewWithFilter' | 'addToFilter'): void {
    void TraceEditorProvider.activePanel?.webview.postMessage({
      kind,
    } satisfies TraceDownMessage);
  }

  public static openWith(
    uri: vscode.Uri,
    inputs: TraceEditorInputs
  ): Thenable<unknown> {
    const existing = TraceEditorProvider.openEditors.get(uri.fsPath);
    if (existing !== undefined) {
      existing.panel.reveal();
      return existing.sendInit(inputs);
    }
    TraceEditorProvider.pendingInputs.set(uri.fsPath, inputs);
    return vscode.commands.executeCommand(
      'vscode.openWith',
      uri,
      TraceEditorProvider.viewType
    );
  }

  public static register(
    context: vscode.ExtensionContext,
    getClient: () => LanguageClient | undefined,
    codiconsCssPath: string,
    settings: SettingsSource
  ): vscode.Disposable {
    const provider = new TraceEditorProvider(
      context,
      getClient,
      codiconsCssPath,
      settings
    );
    logger.log(`Registering ${TraceEditorProvider.viewType}`);
    return vscode.window.registerCustomEditorProvider(
      TraceEditorProvider.viewType,
      provider,
      {
        supportsMultipleEditorsPerDocument: true,
        webviewOptions: { retainContextWhenHidden: true },
      }
    );
  }

  constructor(
    private readonly context: vscode.ExtensionContext,
    private readonly getClient: () => LanguageClient | undefined,
    private readonly codiconsCssPath: string,
    private readonly settings: SettingsSource
  ) {}

  public async resolveCustomTextEditor(
    document: vscode.TextDocument,
    webviewPanel: vscode.WebviewPanel,
    _token: vscode.CancellationToken
  ): Promise<void> {
    const webview = webviewPanel.webview;
    webview.options = { enableScripts: true };

    const file = document.uri.fsPath;
    const inputs = TraceEditorProvider.pendingInputs.get(file);
    TraceEditorProvider.pendingInputs.delete(file);

    const language =
      inputs?.language ??
      resolveLanguage(this.settings.get(), file, vscode.env.language);
    webview.html = this.getHtmlForWebview(webview, language);

    let following: vscode.Disposable = new vscode.Disposable(() => {});

    function postToWebView(message: TraceDownMessage): void {
      webview.postMessage(message);
    }

    const asScopeList = (
      scopes: Map<string, Test | undefined>
    ): [string, JsonValue][] =>
      [...scopes].map(([s, test]) => [
        s,
        test ? (writeTest(test) as JsonValue) : null,
      ]);

    const scopesFromFile = async (
      scope: string | undefined
    ): Promise<Map<string, Test | undefined>> => {
      const lang = file.match(/\.catala_(\w+)/)?.[1];
      const parsed = await parseTestFile(document.getText(), file, lang, scope);
      if (parsed.kind !== 'Results') {
        logger.log(`Trace editor: could not parse tests (${parsed.kind}).`);
        return new Map();
      }
      return new Map(parsed.value.map((t) => [t.testing_scope, t]));
    };

    const scopesFromProject = async (): Promise<string[]> => {
      const client = this.getClient();
      if (!client) return [];
      const entrypoints = await listEntrypoints(
        client,
        [{ kind: 'Test' }, { kind: 'GUI' }, { kind: 'NoInputScope' }],
        file,
        false,
        true
      );
      return entrypoints.map(scopeName);
    };

    const sendInit = async (ins?: TraceEditorInputs): Promise<void> => {
      const scope = ins?.test ? ins.test.testing_scope : ins?.scope;
      const known: Map<string, Test | undefined> = ins?.test
        ? new Map([[ins.test.testing_scope, ins.test]])
        : await scopesFromFile(scope).catch((e) => {
            logger.log(`Trace editor: could not read tests: ${String(e)}`);
            return new Map<string, Test | undefined>();
          });

      postToWebView({
        kind: 'init',
        file,
        cwd: getCwd(file) ?? '',
        scopes: asScopeList(known),
        scope,
        trace: ins?.trace,
        run: ins?.run,
      });

      if (ins?.test) return;
      void scopesFromProject()
        .then((names) => {
          const merged = new Map(known);
          for (const name of names) {
            if (!merged.has(name)) merged.set(name, undefined);
          }
          if (merged.size !== known.size) {
            postToWebView({ kind: 'scopes', scopes: asScopeList(merged) });
          }
        })
        .catch((e) =>
          logger.log(`Trace editor: could not list scopes: ${String(e)}`)
        );
    };

    webviewPanel.webview.onDidReceiveMessage(async (raw: unknown) => {
      const message = raw as TraceUpMessage;
      switch (message.kind) {
        case 'ready':
          following.dispose();
          following = this.settings.follow(webview, file);
          await sendInit(inputs);
          break;
        case 'run': {
          const scope = message.scope.trim();
          if (!scope) {
            postToWebView({
              kind: 'result',
              ok: false,
              error: 'No scope selected.',
            });
            return;
          }
          const result = await runTrace(file, scope);
          postToWebView({ kind: 'result', ...result });
          break;
        }
        case 'loadFile': {
          const path = message.path.trim();
          if (!path) {
            postToWebView({
              kind: 'result',
              ok: false,
              error: 'No trace file path provided.',
            });
            return;
          }
          postToWebView({ kind: 'result', ...readTraceFile(path) });
          break;
        }
        case 'openLocation': {
          const range = new vscode.Range(
            new vscode.Position(
              Math.max(0, message.start.line - 1),
              Math.max(0, message.start.character - 1)
            ),
            new vscode.Position(
              Math.max(0, message.end.line - 1),
              Math.max(0, message.end.character - 1)
            )
          );
          try {
            const doc = await vscode.workspace.openTextDocument(
              vscode.Uri.file(message.file)
            );
            await vscode.window.showTextDocument(doc, {
              selection: range,
              preview: false,
            });
          } catch (e) {
            vscode.window.showErrorMessage(
              `Cannot open ${message.file}: ${String(e)}`
            );
          }
          break;
        }
        case 'requestExtract': {
          const text = extractLine(message.file, message.line);
          postToWebView({ kind: 'extract', id: message.id, text });
          break;
        }
        case 'updateData': {
          logger.log(`Log data value: ${JSON.stringify(message.value)}`);
          let result = await TestCaseEditorProvider.focusDataInput(
            document.uri,
            message.value
          );
          if (!result) {
            vscode.window.showErrorMessage(
              'Unexpected error when trying to focus'
            );
          }
          break;
        }
      }
    });

    TraceEditorProvider.openEditors.set(file, {
      panel: webviewPanel,
      sendInit,
    });
    if (webviewPanel.active) {
      TraceEditorProvider.activePanel = webviewPanel;
    }
    webviewPanel.onDidChangeViewState(() => {
      if (webviewPanel.active) {
        TraceEditorProvider.activePanel = webviewPanel;
      }
    });
    webviewPanel.onDidDispose(() => {
      following.dispose();
      if (TraceEditorProvider.openEditors.get(file)?.panel === webviewPanel) {
        TraceEditorProvider.openEditors.delete(file);
      }
      if (TraceEditorProvider.activePanel === webviewPanel) {
        TraceEditorProvider.activePanel = undefined;
      }
    });
  }

  private getHtmlForWebview(webview: vscode.Webview, language: string): string {
    const scriptUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this.context.extensionUri, 'dist', 'ui.js')
    );
    // vscode-elements' icon component looks up this stylesheet by id to load
    // the Codicons font into its shadow DOM.
    const codiconsUri = webview.asWebviewUri(
      vscode.Uri.joinPath(
        this.context.extensionUri,
        'dist',
        this.codiconsCssPath
      )
    );
    return `
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>Catala Trace Editor</title>
        <link href="${codiconsUri}" id="vscode-codicon-stylesheet" rel="stylesheet" />
        <style>
          body { padding: 10px; }
        </style>
      </head>
      <body>
        <div id="root"></div>
      </body>
      <script src="${scriptUri}"></script>
      <script>
        window.Ui.renderTraceUi("${language}");
      </script>
      </html>
    `;
  }
}
