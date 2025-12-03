import * as vscode from 'vscode';
import { logger } from './logger';
import { assertUnreachable } from './util';
import { getLocalizedMessages } from './i18n/messages';

import type {
  ParseResults,
  TestRunResults,
  UpMessage,
} from './generated/test_case';
import {
  type DownMessage,
  readUpMessage,
  writeDownMessage,
} from './generated/test_case';
import * as path from 'path';
import PQueue from 'p-queue';
import {
  runTestScope,
  parseTestFile,
  generate,
  getAvailableScopes,
} from './testCaseCompilerInterop';
import { renameIfNeeded } from './testCaseUtils';
import { CatalaTestCaseDocument } from './CatalaTestCaseDocument';

export function parseContents(
  content: Uint8Array,
  uri: vscode.Uri,
  language: string
): ParseResults {
  const documentText = new TextDecoder('utf-8').decode(content);
  return parseTestFile(documentText, language, uri.fsPath);
}

// This class contains the 'backend' part of the test case editor that
// sets up the UI, provide initial data and exchanges messages with the
// web view whose entry point is in `uiEntryPoint.ts`
export class TestCaseEditorProvider
  implements vscode.CustomEditorProvider<CatalaTestCaseDocument>
{
  private testQueue: PQueue;

  private _onDidChangeCustomDocument = new vscode.EventEmitter<
    vscode.CustomDocumentEditEvent<CatalaTestCaseDocument>
  >();
  public readonly onDidChangeCustomDocument =
    this._onDidChangeCustomDocument.event;

  constructor(private readonly context: vscode.ExtensionContext) {
    this.testQueue = new PQueue({ concurrency: 1 });
  }

  saveCustomDocument(
    document: CatalaTestCaseDocument,
    cancellation: vscode.CancellationToken
  ): Thenable<void> {
    return document.save(cancellation);
  }

  saveCustomDocumentAs(
    document: CatalaTestCaseDocument,
    destination: vscode.Uri,
    cancellation: vscode.CancellationToken
  ): Thenable<void> {
    return document.saveAs(destination, cancellation);
  }

  revertCustomDocument(
    document: CatalaTestCaseDocument,
    cancellation: vscode.CancellationToken
  ): Thenable<void> {
    return document.revert(cancellation);
  }

  backupCustomDocument(
    document: CatalaTestCaseDocument,
    context: vscode.CustomDocumentBackupContext,
    cancellation: vscode.CancellationToken
  ): Thenable<vscode.CustomDocumentBackup> {
    return document.backup(context.destination, cancellation);
  }

  async openCustomDocument(
    uri: vscode.Uri,
    openContext: vscode.CustomDocumentOpenContext,
    _token: vscode.CancellationToken
  ): Promise<CatalaTestCaseDocument> {
    // Read document
    const document = await CatalaTestCaseDocument.create(
      uri,
      openContext.backupId
    );

    const docChangeSubscription = document.onDidChange((e) => {
      this._onDidChangeCustomDocument.fire(e);
    });
    document.onDidDispose(() => {
      docChangeSubscription.dispose();
    });

    return document;
  }

  public static register(context: vscode.ExtensionContext): vscode.Disposable {
    const provider = new TestCaseEditorProvider(context);
    logger.log(`Registering ${TestCaseEditorProvider.viewType}`);
    const providerRegistration = vscode.window.registerCustomEditorProvider(
      TestCaseEditorProvider.viewType,
      provider,
      {
        supportsMultipleEditorsPerDocument: false,
        webviewOptions: {
          retainContextWhenHidden: true,
        },
      }
    );
    return providerRegistration;
  }

  async resolveCustomEditor(
    document: CatalaTestCaseDocument,
    webviewPanel: vscode.WebviewPanel,
    _token: vscode.CancellationToken
  ): Promise<void> {
    const config = vscode.workspace.getConfiguration('catala');
    const isCustomEditorEnabled = config.get<boolean>(
      'enableCustomTestCaseEditor'
    );

    if (!isCustomEditorEnabled) {
      // If the custom editor is not enabled, show the default text editor
      await vscode.commands.executeCommand(
        'vscode.openWith',
        document.uri,
        'default'
      );
      return;
    }

    webviewPanel.webview.options = {
      enableScripts: true,
    };

    webviewPanel.webview.html = this.getHtmlForWebview(webviewPanel.webview);

    // We want to restrict shell -> webview messages to instances
    // of DownMessage
    function postMessageToWebView(message: DownMessage): void {
      webviewPanel.webview.postMessage(writeDownMessage(message));
    }
    TestCaseEditorProvider.registerWebview(document.uri, postMessageToWebView);

    async function runTest(
      fileName: string,
      scope: string
    ): Promise<TestRunResults> {
      return runTestScope(fileName, scope);
    }

    function applyGuiEdit(
      typed_msg: Extract<UpMessage, { kind: 'GuiEdit' }>
    ): void {
      document.scheduleChange(typed_msg.value[0], typed_msg.value[1]);
    }

    webviewPanel.webview.onDidReceiveMessage(async (message: unknown) => {
      const typed_msg = readUpMessage(message);
      switch (typed_msg.kind) {
        // listen for a 'ready' message from the web view, then send the initial
        // document (in parsed form)
        case 'Ready': {
          logger.log(`Got ready message from webview, sending parsed document`);
          postMessageToWebView({
            kind: 'Update',
            value: document.parseResults,
          });
          TestCaseEditorProvider.markReady(document.uri);
          break;
        }
        case 'GuiEdit': {
          applyGuiEdit(typed_msg);
          break;
        }
        case 'TestRunRequest': {
          // Always save before running the test (no prompt)
          try {
            await saveSpecificDocument(document.uri);
          } catch (err) {
            postMessageToWebView({
              kind: 'TestRunResults',
              value: {
                scope: typed_msg.value.scope,
                reset_outputs: typed_msg.value.reset_outputs,
                results: {
                  kind: 'Error',
                  value:
                    'Failed to save before running: ' +
                    (err instanceof Error ? err.message : String(err)),
                },
              },
            });
            return;
          }

          const { scope, reset_outputs } = typed_msg.value;
          if (reset_outputs) {
            const messages = getLocalizedMessages(vscode.env.language);

            const confirmation = await vscode.window.showInformationMessage(
              messages.resetOutputsConfirmation,
              { modal: true },
              { title: messages.resetButton, action: 'Reset' }
            );

            if (confirmation?.action !== 'Reset') {
              // the user has requested an outputs reset but
              // did not confirm -- we do not need to run the
              // test at all.
              postMessageToWebView({
                kind: 'TestRunResults',
                value: {
                  scope,
                  reset_outputs,
                  results: { kind: 'Cancelled' },
                },
              });
              return;
            }
          }
          const results = await this.testQueue.add(() =>
            runTest(document.uri.fsPath, scope)
          ); // assumes that the document is local (fsPath)

          postMessageToWebView({
            kind: 'TestRunResults',
            value: { scope, reset_outputs, results },
          });

          if (reset_outputs) {
            // reset assertions in the document model, update UI
            if (results.kind === 'Ok') {
              document.resetTestOutputs(scope, results.value.test_outputs);
            }
          }

          break;
        }
        case 'TestGenerateRequest': {
          const { scope_under_test, filename } = typed_msg.value;
          const results = generate(scope_under_test, filename);
          if (results.kind === 'Results') {
            const newTest = results.value;

            const currentTests = document.parseResults;

            if (currentTests.kind === 'Results') {
              newTest[0] = renameIfNeeded(currentTests.value, newTest[0]);
              const updatedTests = [...currentTests.value, newTest[0]];

              document.scheduleChange(updatedTests, false);

              postMessageToWebView({
                kind: 'Update',
                value: { kind: 'Results', value: updatedTests },
              });
            }
          } else {
            vscode.window.showErrorMessage(
              `Failed to generate test: ${results.value}`
            );
          }
          break;
        }
        case 'OpenInTextEditor':
          vscode.commands.executeCommand(
            'vscode.openWith',
            document.uri,
            'default'
          );
          break;
        case 'OpenTestScopePicker': {
          try {
            const ws = vscode.workspace.getWorkspaceFolder(document.uri);
            const wsPath = ws?.uri.fsPath;
            const entries = (await vscode.commands.executeCommand(
              'catala.getTestableScopes',
              wsPath
            )) as { path: string; scopes: string[] }[];

            const browseItem: vscode.QuickPickItem = {
              label: '$(folder-opened) Choose from file…',
              alwaysShow: true,
            };

            const scopeItems: vscode.QuickPickItem[] = entries.flatMap((e) =>
              e.scopes.map((scope) => ({
                label: scope,
                description: e.path,
              }))
            );

            const picked = await vscode.window.showQuickPick(
              [
                browseItem,
                {
                  label: 'Catala scopes',
                  kind: vscode.QuickPickItemKind.Separator,
                },
                ...scopeItems,
              ],
              {
                matchOnDescription: true,
                placeHolder: 'Select a scope to create a test',
              }
            );

            if (!picked) break;

            let filename: string | undefined;
            let scopeUnderTest: string | undefined;

            if (picked === browseItem) {
              const fileUri = await vscode.window.showOpenDialog({
                filters: {
                  'Catala Files': ['catala_fr', 'catala_en', 'catala_pl'],
                },
              });
              if (!fileUri?.[0]) break;
              filename = fileUri[0].fsPath;

              const scopes = await getAvailableScopes(filename);
              const pickedScope = await vscode.window.showQuickPick(
                scopes.map((s) => ({ label: s.name })),
                {
                  placeHolder: `Select a scope in ${path.basename(filename)}`,
                }
              );
              if (!pickedScope) break;
              scopeUnderTest = pickedScope.label;
            } else {
              scopeUnderTest = picked.label;
              filename = picked.description;
            }

            if (!filename || !scopeUnderTest) break;

            const results = generate(scopeUnderTest, filename);
            if (results.kind === 'Results') {
              const newTest = results.value;

              const currentTests = document.parseResults;
              if (currentTests.kind === 'Results') {
                newTest[0] = renameIfNeeded(currentTests.value, newTest[0]);
                const updatedTests = [...currentTests.value, newTest[0]];

                document.scheduleChange(updatedTests, false);

                postMessageToWebView({
                  kind: 'Update',
                  value: { kind: 'Results', value: updatedTests },
                });
              }
            } else {
              vscode.window.showErrorMessage(
                `Failed to generate test: ${results.value}`
              );
            }
          } catch (err) {
            logger.log(
              `OpenTestScopePicker failed: ${
                err instanceof Error ? err.message : String(err)
              }`
            );
          }
          break;
        }
        case 'ConfirmRequest': {
          const { id, action } = typed_msg.value;
          const messages = getLocalizedMessages(vscode.env.language);
          const prompt =
            action.kind === 'DeleteArrayElement'
              ? messages.deleteArrayElementConfirmation
              : action.kind === 'DeleteAssertion'
                ? messages.deleteAssertionConfirmation
                : assertUnreachable(action as never);
          const confirmation = await vscode.window.showWarningMessage(
            prompt,
            { modal: true },
            { title: messages.deleteButton, action: 'Delete' }
          );
          postMessageToWebView({
            kind: 'ConfirmResult',
            value: { id, confirmed: confirmation?.action === 'Delete' },
          });
          break;
        }
        default:
          assertUnreachable(typed_msg);
      }
    });

    const changeSubscription = document.onDidChangeContent((_e) => {
      // update GUI
      postMessageToWebView({
        kind: 'Update',
        value: document.parseResults,
      });
    });

    webviewPanel.onDidDispose(() => {
      // Any disposal code should go here
      // e.g. subscriptions to vs code 'system' events
      // (content change monitoring...)
      TestCaseEditorProvider.unregisterWebview(document.uri);
      changeSubscription.dispose();
    });
  }

  public static readonly viewType = 'catala.testCaseEditor';

  // Registry of open custom editor webviews to send messages to
  private static webviews = new Map<
    string,
    { post: (msg: DownMessage) => void; ready: boolean; queue: DownMessage[] }
  >();

  private static registerWebview(
    uri: vscode.Uri,
    post: (m: DownMessage) => void
  ): void {
    const key = uri.toString();
    const existing = TestCaseEditorProvider.webviews.get(key);
    if (existing) {
      existing.post = post;
      TestCaseEditorProvider.webviews.set(key, existing);
    } else {
      TestCaseEditorProvider.webviews.set(key, {
        post,
        ready: false,
        queue: [],
      });
    }
  }

  private static unregisterWebview(uri: vscode.Uri): void {
    TestCaseEditorProvider.webviews.delete(uri.toString());
  }

  private static markReady(uri: vscode.Uri): void {
    const key = uri.toString();
    const entry = TestCaseEditorProvider.webviews.get(key);
    if (!entry) return;
    entry.ready = true;
    while (entry.queue.length) {
      const m = entry.queue.shift()!;
      entry.post(m);
    }
  }

  private static postOrQueue(uri: vscode.Uri, msg: DownMessage): boolean {
    const key = uri.toString();
    const entry = TestCaseEditorProvider.webviews.get(key);
    if (!entry) {
      // Create placeholder entry with the message queued; resolveCustomEditor will register later.
      TestCaseEditorProvider.webviews.set(key, {
        post: () => {},
        ready: false,
        queue: [msg],
      });
      return false;
    }
    if (!entry.ready) {
      entry.queue.push(msg);
      return false;
    }
    entry.post(msg);
    return true;
  }

  public static async focusDiffInCustomEditor(
    uri: vscode.Uri,
    scope: string,
    results: TestRunResults
  ): Promise<boolean> {
    const config = vscode.workspace.getConfiguration('catala');
    const isEnabled = config.get<boolean>('enableCustomTestCaseEditor');
    if (!isEnabled) return false;

    try {
      await vscode.commands.executeCommand(
        'vscode.openWith',
        uri,
        TestCaseEditorProvider.viewType
      );
    } catch {
      return false;
    }

    // Deliver immediately if ready, or queue until the webview signals Ready.
    TestCaseEditorProvider.postOrQueue(uri, {
      kind: 'TestRunResults',
      value: { scope, reset_outputs: false, results },
    });
    return true;
  }

  public static updateOpenCustomEditorWithResults(
    uri: vscode.Uri,
    scope: string,
    results: TestRunResults
  ): boolean {
    const key = uri.toString();
    const entry = TestCaseEditorProvider.webviews.get(key);
    if (!entry) return false;

    const msg: DownMessage = {
      kind: 'TestRunResults',
      value: { scope, reset_outputs: false, results },
    };

    if (entry.ready) {
      entry.post(msg);
    } else {
      entry.queue.push(msg);
    }
    return true;
  }

  private getHtmlForWebview(webview: vscode.Webview): string {
    const scriptUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this.context.extensionUri, 'dist', 'ui.js')
    );

    const language = vscode.env.language;

    return `
          <!DOCTYPE html>
          <html lang="en">
          <head>
              <meta charset="UTF-8">
              <meta name="viewport" content="width=device-width, initial-scale=1.0">
              <title>Test Case Editor</title>
              <style>
                  body {
                      padding: 10px;
                  }
              </style>
          </head>
          <body>
              <div id="root"></div>
          </body>
          <script src="${scriptUri}"></script>
          <script>
            window.Ui.renderUi("${language}");
          </script>
          </html>
      `;
  }
}

export function getLanguageFromUri(uri: vscode.Uri): string {
  const extension = path.extname(uri.path);
  const match = extension.match(/\.catala_(\w+)$/);
  if (match?.[1]) {
    return match[1];
  }
  throw new Error(`Unable to determine language from file name: ${uri}`);
}

export async function focusDiffInCustomEditor(
  uri: vscode.Uri,
  scope: string,
  results: TestRunResults
): Promise<boolean> {
  return TestCaseEditorProvider.focusDiffInCustomEditor(uri, scope, results);
}

export async function updateOpenCustomEditorWithResults(
  uri: vscode.Uri,
  scope: string,
  results: TestRunResults
): Promise<boolean> {
  return TestCaseEditorProvider.updateOpenCustomEditorWithResults(
    uri,
    scope,
    results
  );
}

/**
 * Find the tab for a custom document
 * @param uri The URI of the custom document
 * @returns The tab for the custom document
 * @throws Error if no tab is found for the given URI
 */
function findCustomDocumentTab(uri: vscode.Uri): vscode.Tab {
  const tab = vscode.window.tabGroups.all
    .flatMap((group) => group.tabs)
    .find(
      (tab) =>
        tab.input instanceof vscode.TabInputCustom &&
        tab.input.uri.toString() === uri.toString()
    );

  if (!tab) {
    throw new Error(`No tab found for custom document: ${uri.toString()}`);
  }

  return tab;
}

/**
 * Save a specific custom document
 * @param uri The URI of the document to save
 */
async function saveSpecificDocument(uri: vscode.Uri): Promise<void> {
  // Verify the tab exists first
  findCustomDocumentTab(uri);

  // Now save the active document (since we clicked on the run button
  // we assume that the active document is the right one)
  await vscode.commands.executeCommand('workbench.action.files.save');
}
