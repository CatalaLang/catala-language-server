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
          break;
        }
        case 'GuiEdit': {
          applyGuiEdit(typed_msg);
          break;
        }
        case 'TestRunRequest': {
          const shouldProceed = await promptSaveBeforeTest(document.uri);

          if (!shouldProceed) {
            // User cancelled
            postMessageToWebView({
              kind: 'TestRunResults',
              value: {
                scope: typed_msg.value.scope,
                reset_outputs: typed_msg.value.reset_outputs,
                results: { kind: 'Cancelled' },
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
        case 'SelectFileForNewTest': {
          const fileUri = await vscode.window.showOpenDialog({
            filters: {
              'Catala Files': ['catala_fr', 'catala_en', 'catala_pl'],
            },
          });

          if (fileUri?.[0]) {
            const selectedFile = fileUri[0].fsPath;
            const scopes = await getAvailableScopes(selectedFile);

            postMessageToWebView({
              kind: 'FileSelectedForNewTest',
              value: {
                filename: selectedFile,
                available_scopes: scopes,
              },
            });
          }
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
      changeSubscription.dispose();
    });
  }

  private static readonly viewType = 'catala.testCaseEditor';

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
 * Check if a custom document has unsaved changes.
 * Workaround using the tab API because vs code does not
 * expose the dirty state of a custom document even though
 * the indicator correctly displays it.
 * @param uri The URI of the custom document
 * @returns true if the document has unsaved changes, false otherwise
 * @throws Error if no tab is found for the given URI
 */
function isCustomDocumentDirty(uri: vscode.Uri): boolean {
  const tab = findCustomDocumentTab(uri);
  return tab.isDirty;
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

/**
 * Prompt user to save before running test if document is dirty
 * @param documentUri The URI of the document to check
 * @returns Promise<boolean> true if should proceed with test, false if cancelled
 */
async function promptSaveBeforeTest(documentUri: vscode.Uri): Promise<boolean> {
  if (!isCustomDocumentDirty(documentUri)) {
    return true; // No unsaved changes, proceed
  }

  const messages = getLocalizedMessages(vscode.env.language);

  const choice = await vscode.window.showWarningMessage(
    messages.unsavedChangesWarning,
    { modal: true },
    { title: messages.saveAndRun, action: 'SaveAndRun' },
    { title: messages.runWithoutSaving, action: 'RunWithoutSaving' }
  );

  switch (choice?.action) {
    case 'SaveAndRun':
      await saveSpecificDocument(documentUri);
      return true;

    case 'RunWithoutSaving': {
      const confirmed = await vscode.window.showWarningMessage(
        messages.runAgainstSavedContent,
        { modal: true },
        { title: messages.continue, action: 'Continue' }
      );
      return confirmed?.action === 'Continue';
    }
    default:
      return false;
  }
}
