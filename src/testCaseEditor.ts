import * as vscode from 'vscode';
import { logger } from './logger';
import { execFileSync, type SpawnSyncReturns } from 'child_process';
import { assertUnreachable } from './util';
import type {
  ParseResults,
  TestList,
  TestRunResults,
} from './generated/test_case';
import {
  type DownMessage,
  readUpMessage,
  writeDownMessage,
  readTestList,
  writeTestList,
} from './generated/test_case';
import * as path from 'path';
import PQueue from 'p-queue';

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

    webviewPanel.webview.onDidReceiveMessage((message: unknown) => {
      const typed_msg = readUpMessage(message);
      const lang = getLanguageFromFileName(document.fileName);
      switch (typed_msg.kind) {
        case 'Ready': {
          const parseResults = parseTestFile(document.getText(), lang);
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
              value: parseTestFile(document.getText(), lang), //XXX concurrent edits?
            });
          });
          break;
        }
        case 'TestRunRequest': {
          const { scope } = typed_msg.value;
          this.testQueue.add(() => runTest(document.fileName, scope));
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
            value: parseTestFile(e.document.getText(), lang),
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

function parseTestFile(content: string, lang: string): ParseResults {
  // plugin dir should be relative to VS code extension package root obviously
  // -- TODO check
  // obviously 'examples' is not appropriate here too
  // --scope XXX should also be removed -- it's a leftover from the current CLI
  // TODO we could revisit this to make the parsing async
  try {
    const results = execFileSync(
      'catala',
      [
        'testcase',
        'read',
        '--plugin-dir',
        './test-case-parser/_build/default',
        '-l',
        lang,
        '-I',
        './test-case-parser/examples',
        '-',
      ],
      { input: content }
    );
    return {
      kind: 'Results',
      value: readTestList(JSON.parse(results.toString())),
    };
  } catch (error) {
    return {
      kind: 'Error',
      value: String((error as SpawnSyncReturns<string | Buffer>).stderr),
    };
  }
}

function atdToCatala(tests: TestList, lang: string): string {
  //XXX this probably needs better error handling
  try {
    const results = execFileSync(
      'catala',
      [
        'testcase',
        'write',
        '--plugin-dir',
        './test-case-parser/_build/default',
        '-l',
        lang,
      ],
      { input: JSON.stringify(writeTestList(tests)) }
    );
    return results.toString();
  } catch (error) {
    logger.log(`Error in atdToCatala: ${error}`);
    throw error;
  }
}

function runTestScope(filename: string, testScope: string): TestRunResults {
  /*
   * Notes:
   * - when parsing / generating tests, we operate on the current text buffer
   * in the editor through `stdin`. Here, we run the actual file on disk.
   * Should we produce an error if they are not identical? (i.e. the buffer
   * is dirty)?
   * - security: fileName should be provided by the editor, so it should be
   * trustworthy: check?
   * - Users should probably have a command that interrupts a running test
   * - Should tests have (configurable) timeouts? (when running interactively)
   * (note that not all these questions are related to the `runTestScope` function,
   * these could be handled externally as well)
   */
  const cmd = 'clerk';
  filename = path.isAbsolute(filename)
    ? path.relative(process.cwd(), filename)
    : filename;
  const args = ['run', filename, '--scope', testScope];
  logger.log(`Exec: ${cmd} ${args.join(' ')}`);
  try {
    execFileSync(cmd, args);
  } catch (error) {
    return {
      kind: 'Error',
      value: String((error as SpawnSyncReturns<string | Buffer>).stderr),
    };
  }
  return { kind: 'Ok' };
}

function getLanguageFromFileName(fileName: string): string {
  const extension = path.extname(fileName);
  const match = extension.match(/\.catala_(\w+)$/);
  if (match?.[1]) {
    return match[1];
  }
  throw new Error(`Unable to determine language from file name: ${fileName}`);
}
