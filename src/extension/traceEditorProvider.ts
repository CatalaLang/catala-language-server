import * as vscode from 'vscode';
import { readFileSync } from 'fs';
import { dirname, join } from 'path';
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
import {
  PersistentCache,
  hashDir,
  readTraceFile,
  runTrace,
} from '../trace-editor/traceRunner';
import type { TraceElement } from '../trace-editor/traceUtils';
import type { ParseResults, Test } from '../generated/catala_types';
import {
  readParseResults,
  writeParseResults,
  writeTest,
} from '../generated/catala_types';

const fileLineCache = new Map<string, string>();

let readTestCache: PersistentCache<ParseResults, unknown> | undefined;

export function initReadTestCache(storageDir: string): void {
  readTestCache = new PersistentCache<ParseResults, unknown>(
    join(storageDir, 'read-test-cache.json'),
    (v) => writeParseResults(v),
    (v) => readParseResults(v)
  );
}

function cachedReadTests(
  content: string,
  file: string,
  lang: string | undefined,
  scope: string | undefined
): ParseResults {
  const cwd = getCwd(file) ?? dirname(file);
  const hash = hashDir(join(cwd, '_build'));
  const hit = readTestCache?.find(hash, file, scope ?? '');
  if (hit !== undefined) {
    logger.log(
      `Tests for "${file}"${scope ? ` (scope "${scope}")` : ''} served from cache.`
    );
    return hit;
  }
  const result = parseTestFile(content, file, lang, scope);
  readTestCache?.set(hash, file, scope ?? '', result);
  return result;
}

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

  public static openWith(
    uri: vscode.Uri,
    inputs: TraceEditorInputs
  ): Thenable<unknown> {
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
    codiconsCssPath: string
  ): vscode.Disposable {
    const provider = new TraceEditorProvider(
      context,
      getClient,
      codiconsCssPath
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
    private readonly codiconsCssPath: string
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
      file.match(/\.catala_(\w+)/)?.[1] ??
      vscode.env.language;
    webview.html = this.getHtmlForWebview(webview, language);

    function postToWebView(message: TraceDownMessage): void {
      webview.postMessage(message);
    }

    webviewPanel.webview.onDidReceiveMessage(async (raw: unknown) => {
      const message = raw as TraceUpMessage;
      switch (message.kind) {
        case 'ready': {
          let scopesWithInfo: Map<string, Test | undefined> = new Map();
          let scope = inputs?.scope;
          if (inputs?.test) {
            const test = inputs.test;
            scope = test.testing_scope;
            scopesWithInfo = new Map([[test.testing_scope, test]]);
          } else {
            const client = this.getClient();
            if (client) {
              try {
                const entrypoints = await listEntrypoints(
                  client,
                  [{ kind: 'Test' }, { kind: 'GUI' }, { kind: 'NoInputScope' }],
                  file,
                  false,
                  true
                );
                const scopes = entrypoints.map(scopeName);
                const lang = file.match(/\.catala_(\w+)/)?.[1];
                const parsed = cachedReadTests(
                  document.getText(),
                  file,
                  lang,
                  scope
                );
                if (parsed.kind === 'Results') {
                  scopesWithInfo = new Map(
                    scopes.map((s) => [
                      s,
                      parsed.value.find((t) => t.testing_scope == s),
                    ])
                  );
                } else {
                  scopesWithInfo = new Map(scopes.map((s) => [s, undefined]));
                  logger.log(
                    `Trace editor: could not parse tests (${parsed.kind}).`
                  );
                }
              } catch (e) {
                logger.log(`Trace editor: could not list scopes: ${String(e)}`);
              }
            }
          }
          postToWebView({
            kind: 'init',
            file,
            cwd: getCwd(file) ?? '',
            scopes: [...scopesWithInfo].map(
              ([s, test]): [string, JsonValue] => [
                s,
                test ? (writeTest(test) as JsonValue) : null,
              ]
            ),
            scope,
            trace: inputs?.trace,
            run: inputs?.run,
          });
          break;
        }
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
