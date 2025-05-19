import * as vscode from 'vscode';
import { logger } from './logger';
import { assertUnreachable } from './util';

import type { UpMessage } from './generated/test_case';
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
  atdToCatala,
  generate,
  getAvailableScopes,
} from './testCaseCompilerInterop';
import { renameIfNeeded } from './testCaseUtils';

// This class contains the 'backend' part of the test case editor that
// sets up the UI, provide initial data and exchanges messages with the
// web view whose entry point is in `uiEntryPoint.ts`
export class TestCaseEditorProvider implements vscode.CustomTextEditorProvider {
  private testQueue: PQueue;

  constructor(private readonly context: vscode.ExtensionContext) {
    this.testQueue = new PQueue({ concurrency: 1 });
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

  public async resolveCustomTextEditor(
    document: vscode.TextDocument,
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

    // listen for a 'ready' message from the web view, then send the initial
    // document (in parsed form)

    async function runTest(fileName: string, scope: string): Promise<void> {
      const results = runTestScope(fileName, scope);
      postMessageToWebView({
        kind: 'TestRunResults',
        value: results,
      });
    }

    const QUIESCENCE_DELAY_MS = 1500;

    // Debounce mechanism for GuiEdit messages
    let guiEditTimeout: NodeJS.Timeout | null = null;
    let latestGuiEditMessage: Extract<UpMessage, { kind: 'GuiEdit' }> | null =
      null;
    let isApplyingEdit = false;

    async function applyLatestEdit(lang: string): Promise<void> {
      if (!latestGuiEditMessage || isApplyingEdit) {
        return;
      }

      isApplyingEdit = true;

      try {
        // re-emit catala text file from ATD test definitions
        const newTextBuffer = atdToCatala(latestGuiEditMessage.value, lang);

        // produce edit
        const edit = new vscode.WorkspaceEdit();
        edit.replace(
          document.uri,
          new vscode.Range(0, 0, document.lineCount, 0),
          newTextBuffer
        );

        await vscode.workspace.applyEdit(edit);

        // Update the UI with the new state
        postMessageToWebView({
          kind: 'Update',
          value: parseTestFile(document.getText(), lang, document.uri.fsPath),
        });

        // Reset the latest message
        latestGuiEditMessage = null;
      } finally {
        isApplyingEdit = false;
      }
    }

    function applyGuiEdit(
      typed_msg: Extract<UpMessage, { kind: 'GuiEdit' }>,
      lang: string
    ): void {
      // Store the latest message
      latestGuiEditMessage = typed_msg;

      // Clear any existing timeout
      if (guiEditTimeout) {
        clearTimeout(guiEditTimeout);
      }

      // Set a new timeout for the quiescence period
      guiEditTimeout = setTimeout(() => {
        applyLatestEdit(lang);
      }, QUIESCENCE_DELAY_MS);
    }

    // Hook into the save event to immediately apply edits
    const willSaveTextDocumentSubscription =
      vscode.workspace.onWillSaveTextDocument((e) => {
        if (
          e.document.uri.toString() === document.uri.toString() &&
          latestGuiEditMessage
        ) {
          // Clear any pending timeout
          if (guiEditTimeout) {
            clearTimeout(guiEditTimeout);
            guiEditTimeout = null;
          }

          // Apply the edit immediately
          const lang = getLanguageFromFileName(e.document.fileName);
          // We need to wait for the edit to be applied before the save happens
          e.waitUntil(applyLatestEdit(lang));
        }
      });

    webviewPanel.webview.onDidReceiveMessage(async (message: unknown) => {
      const typed_msg = readUpMessage(message);
      const lang = getLanguageFromFileName(document.fileName);
      switch (typed_msg.kind) {
        case 'Ready': {
          const parseResults = parseTestFile(
            document.getText(),
            lang,
            document.uri.fsPath
          );
          logger.log(
            `Got ready message from webview, sending parsed document: \n ${JSON.stringify(parseResults)}`
          );
          postMessageToWebView({
            kind: 'Update',
            value: parseResults,
          });
          break;
        }
        case 'GuiEdit': {
          logger.log('Got edit from webview');
          applyGuiEdit(typed_msg, lang);
          break;
        }
        case 'CancelSourceUpdate': {
          logger.log('Source update cancelled');
          if (guiEditTimeout) {
            clearTimeout(guiEditTimeout);
            guiEditTimeout = null;
          }

          break;
        }
        case 'TestRunRequest': {
          const { scope } = typed_msg.value;
          this.testQueue.add(() => runTest(document.fileName, scope));
          break;
        }
        case 'TestGenerateRequest': {
          const { scope_under_test, filename } = typed_msg.value;
          const results = generate(scope_under_test, filename);
          if (results.kind === 'Results') {
            const newTest = results.value;

            const currentTests = parseTestFile(
              document.getText(),
              lang,
              document.uri.fsPath
            );
            if (currentTests.kind === 'Results') {
              newTest[0] = renameIfNeeded(currentTests.value, newTest[0]); //XXX kludge?
              const updatedTests = [...currentTests.value, newTest[0]];
              const newTextBuffer = atdToCatala(updatedTests, lang);
              const edit = new vscode.WorkspaceEdit();
              edit.replace(
                document.uri,
                new vscode.Range(0, 0, document.lineCount, 0),
                newTextBuffer
              );
              vscode.workspace.applyEdit(edit).then(() => {
                postMessageToWebView({
                  kind: 'Update',
                  value: parseTestFile(
                    document.getText(),
                    lang,
                    document.uri.fsPath
                  ),
                });
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

    const changeDocumentSubscription = vscode.workspace.onDidChangeTextDocument(
      (e) => {
        if (e.document.uri.toString() === document.uri.toString()) {
          const lang = getLanguageFromFileName(e.document.fileName);
          postMessageToWebView({
            kind: 'Update',
            value: parseTestFile(
              e.document.getText(),
              lang,
              document.uri.fsPath
            ),
          });
        }
      }
    );

    webviewPanel.onDidDispose(() => {
      changeDocumentSubscription.dispose();
      willSaveTextDocumentSubscription.dispose();
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

function getLanguageFromFileName(fileName: string): string {
  const extension = path.extname(fileName);
  const match = extension.match(/\.catala_(\w+)$/);
  if (match?.[1]) {
    return match[1];
  }
  throw new Error(`Unable to determine language from file name: ${fileName}`);
}
