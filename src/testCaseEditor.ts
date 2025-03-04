import * as vscode from 'vscode';
import { logger } from './logger';
import { assertUnreachable } from './util';

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
        case 'Edit': {
          logger.log('Got edit from webview');
          // re-emit catala text file from ATD test definitions
          const newTextBuffer = atdToCatala(typed_msg.value, lang);
          logger.log(`newTextBuffer:\n ${newTextBuffer}`);
          // produce edit
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
              ), //XXX concurrent edits?
            });
          });
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
    });
  }

  private static readonly viewType = 'catala.testCaseEditor';

  private getHtmlForWebview(webview: vscode.Webview): string {
    const scriptUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this.context.extensionUri, 'dist', 'ui.js')
    );

    const styleUri = webview.asWebviewUri(
      vscode.Uri.joinPath(this.context.extensionUri, 'src', 'style.css')
    );

    const codiconsUri = webview.asWebviewUri(
      vscode.Uri.joinPath(
        this.context.extensionUri,
        'node_modules',
        '@vscode/codicons',
        'dist',
        'codicon.css'
      )
    );

    return `
          <!DOCTYPE html>
          <html lang="en">
          <head>
              <meta charset="UTF-8">
              <meta name="viewport" content="width=device-width, initial-scale=1.0">
              <title>Test Case Editor</title>
              <link rel="stylesheet" type="text/css" href="${styleUri}">
              <link rel="stylesheet" type="text/css" href="${codiconsUri}">
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
            window.Ui.renderUi();
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
