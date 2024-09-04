import * as vscode from 'vscode';
import { logger } from './logger';
import { execFileSync } from 'child_process';
import type { DownMessage, UpMessage } from './messages';
import { assertUnreachable } from './util';
import type { TestList } from './generated/test_case';
import { readTestList, writeTestList } from './generated/test_case';

// This class contains the 'backend' part of the test case editor that
// sets up the UI, provide initial data and exchanges messages with the
// web view whose entry point is in `uiEntryPoint.ts`
export class TestCaseEditorProvider implements vscode.CustomTextEditorProvider {
  constructor(private readonly context: vscode.ExtensionContext) {}

  public static register(context: vscode.ExtensionContext): vscode.Disposable {
    const provider = new TestCaseEditorProvider(context);
    logger.log(`Registering ${TestCaseEditorProvider.viewType}`);
    const providerRegistration = vscode.window.registerCustomEditorProvider(
      TestCaseEditorProvider.viewType,
      provider
    );
    return providerRegistration;
  }

  public async resolveCustomTextEditor(
    document: vscode.TextDocument,
    webviewPanel: vscode.WebviewPanel,
    _token: vscode.CancellationToken
  ): Promise<void> {
    webviewPanel.webview.options = {
      enableScripts: true,
    };

    webviewPanel.webview.html = this.getHtmlForWebview(webviewPanel.webview);

    // This is just for typechecking purposes, as we want to restrict
    // cross-window messages to instances of DownMessage
    function postMessageToWebView(message: DownMessage): void {
      webviewPanel.webview.postMessage(message);
    }

    // listen for a 'ready' message from the web view, then send the initial
    // document (in parsed form)
    webviewPanel.webview.onDidReceiveMessage((message) => {
      // type coercion because cross-window message exchange is untyped
      // (see if typescript offers something better?)
      const msg = message as UpMessage;
      switch (msg.kind) {
        case 'ready': {
          const parseResults = parseTestFile(document.getText());
          logger.log(
            `Got ready message from webview, sending parsed document: \n ${JSON.stringify(parseResults)}`
          );
          postMessageToWebView({
            kind: 'update',
            parseResults,
          });
          break;
        }
        case 'edit': {
          logger.log('Got edit from webview');
          const jsonTests = JSON.parse(msg.tests);
          const atdTests = readTestList(jsonTests);

          // re-emit catala text file from ATD test definitions
          const newTextBuffer = atdToCatala(atdTests);
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
              kind: 'update',
              parseResults: parseTestFile(document.getText()), //XXX concurrent edits?
            });
          });
          break;
        }
        default:
          assertUnreachable(msg);
      }
    });

    const changeDocumentSubscription = vscode.workspace.onDidChangeTextDocument(
      (e) => {
        if (e.document.uri.toString() === document.uri.toString()) {
          postMessageToWebView({
            kind: 'update',
            parseResults: parseTestFile(document.getText()),
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

export type ParseResults =
  | { results: unknown /* JSON-able equivalent of test file */ }
  | { error: string };

function parseTestFile(content: string): ParseResults {
  // plugin dir should be relative to VS code extension package root obviously
  // -- TODO check
  // obviously 'examples' is not appropriate here too
  // --scope XXX should also be removed -- it's a leftover from the current CLI
  // TODO we could revisit this to make the parsing async
  // TODO we should also implement the language selection
  try {
    const results = execFileSync(
      'catala',
      [
        'testcase',
        '-',
        '--plugin-dir',
        './test-case-parser/_build/default',
        '--read',
        '-l',
        'en',
        '--scope',
        'XXX',
        '-I',
        './test-case-parser/examples',
      ],
      { input: content }
    );
    return { results: JSON.parse(results.toString()) };
  } catch (error) {
    return { error };
  }
}

function atdToCatala(tests: TestList): string {
  //XXX this probably needs better error handling
  const results = execFileSync(
    'catala',
    [
      'testcase',
      '-',
      '--plugin-dir',
      './test-case-parser/_build/default',
      '--write',
      '-l',
      'en',
      '--scope',
      'XXX',
      '-I',
      './test-case-parser/examples',
    ],
    { input: JSON.stringify(writeTestList(tests)) }
  );
  return results.toString();
}
