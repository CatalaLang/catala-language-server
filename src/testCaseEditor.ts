import * as vscode from 'vscode';
import { logger } from './logger';
import { assertUnreachable } from './util';

import type { ParseResults, TestList, UpMessage } from './generated/test_case';
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

/**
 * Custom document.
 * The editor UI works with the AST (ATD structure, a TestList)
 * but the extension will serialize it to / parse it from a Catala text file.
 */
class CatalaTestCaseDocument
  extends vscode.Disposable
  implements vscode.CustomDocument
{
  private readonly _uri: vscode.Uri;
  private readonly _language: string;
  private _parseResults: ParseResults; //XXX not sure
  private _history: ParseResults[] = []; //TODO bound capacity? Edit coalescing?
  private _savedHistory: ParseResults[] = []; //TODO bound capacity? Edit coalescing?

  private readonly _onDidDispose = new vscode.EventEmitter<void>();
  public readonly onDidDispose = this._onDidDispose.event;

  static async create(
    uri: vscode.Uri,
    backupId: string | undefined
  ): Promise<CatalaTestCaseDocument> {
    const dataFile =
      typeof backupId === 'string' ? vscode.Uri.parse(backupId) : uri;
    const fileData = await CatalaTestCaseDocument.readFile(dataFile);
    return new CatalaTestCaseDocument(uri, fileData);
  }

  private static async readFile(uri: vscode.Uri): Promise<Uint8Array> {
    if (uri.scheme === 'untitled') {
      return new Uint8Array();
    }
    return new Uint8Array(await vscode.workspace.fs.readFile(uri));
  }

  // Fired when an edit is made, notify vs code
  // (which in turn manages the undo stack and dirty indicator...)
  // This does **not** require an explicit subscription in
  // our code (although we re-emit it from the editor,
  // which is the only thing that VS code knows about -- the custom
  // document model is unknown to VS code).
  //
  // Triggered from `setContents()`
  private readonly _onDidChange = new vscode.EventEmitter<
    vscode.CustomDocumentEditEvent<CatalaTestCaseDocument>
  >();
  public readonly onDidChange = this._onDidChange.event;

  // This event is used to trigger UI refreshes.
  // It is fired on GUI edits, undo and redo operations.
  // We subscribe to this event from `resolveCustomEditor`
  private readonly _onDidChangeDocument = new vscode.EventEmitter<
    vscode.CustomDocumentContentChangeEvent<CatalaTestCaseDocument>
  >();
  public readonly onDidChangeContent = this._onDidChangeDocument.event;

  public get uri(): vscode.Uri {
    return this._uri;
  }

  public get language(): string {
    return this._language;
  }

  //XXX not confident in this interface
  public get parseResults(): ParseResults {
    return this._parseResults;
  }

  async save(cancellation: vscode.CancellationToken): Promise<void> {
    await this.saveAs(this.uri, cancellation);
    this._savedHistory = Array.from(this._history);
  }

  async saveAs(
    targetResource: vscode.Uri,
    cancellation: vscode.CancellationToken
  ): Promise<void> {
    if (this._parseResults.kind !== 'Results') {
      throw new Error('Invalid testcase file, cannot save');
    }
    const catalaSource = atdToCatala(this._parseResults.value, this.language);
    const writeData = Buffer.from(catalaSource, 'utf-8');
    if (cancellation.isCancellationRequested) {
      return;
    }
    await vscode.workspace.fs.writeFile(targetResource, writeData);
  }

  async revert(_cancellation: vscode.CancellationToken): Promise<void> {
    const diskContent = await CatalaTestCaseDocument.readFile(this.uri);
    this._parseResults = parseContents(diskContent, this._uri, this._language);
    this._history = this._savedHistory;

    this._onDidChangeDocument.fire({
      document: this,
    });
  }

  async backup(
    destination: vscode.Uri,
    cancellation: vscode.CancellationToken
  ): Promise<vscode.CustomDocumentBackup> {
    await this.saveAs(destination, cancellation);

    return {
      id: destination.toString(),
      delete: async (): Promise<void> => {
        try {
          await vscode.workspace.fs.delete(destination);
        } catch {
          // noop
        }
      },
    };
  }

  /**
   * Called by VS Code when there are no more references to the document.
   * This happens when all editors for it have been closed.
   */
  dispose(): void {
    this._onDidDispose.fire();
    super.dispose();
  }

  // 'makeEdit' in sample
  public setContents(tests: TestList): void {
    this._parseResults = { kind: 'Results', value: tests };
    this._history.push(this._parseResults);

    this._onDidChange.fire({
      document: this,
      label: 'edit',
      undo: (): void => {
        const lastRev = this._history.pop();
        if (lastRev !== undefined) {
          this._parseResults = lastRev;
          this._onDidChangeDocument.fire({ document: this });
        }
      },
      redo: (): void => {
        this._history.push(this._parseResults);
        this._onDidChangeDocument.fire({ document: this });
      },
    });

    this._onDidChangeDocument.fire({ document: this });
  }

  private constructor(uri: vscode.Uri, initialContent: Uint8Array) {
    super(() => {}); //XXX -- the sample just seems to be able to call super()
    this._uri = uri;
    this._language = getLanguageFromUri(this._uri);

    this._parseResults = parseContents(
      initialContent,
      this._uri,
      this._language
    );
  }
}

function parseContents(
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

    async function runTest(fileName: string, scope: string): Promise<void> {
      const results = runTestScope(fileName, scope);
      postMessageToWebView({
        kind: 'TestRunResults',
        value: results,
      });
    }

    function applyGuiEdit(
      typed_msg: Extract<UpMessage, { kind: 'GuiEdit' }>
    ): void {
      document.setContents(typed_msg.value);
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
          const { scope } = typed_msg.value;
          this.testQueue.add(() => runTest(document.uri.fsPath, scope)); // assumes that the document is local (fsPath)
          break;
        }
        case 'TestGenerateRequest': {
          const { scope_under_test, filename } = typed_msg.value;
          const results = generate(scope_under_test, filename);
          if (results.kind === 'Results') {
            const newTest = results.value;

            const currentTests = document.parseResults;

            if (currentTests.kind === 'Results') {
              newTest[0] = renameIfNeeded(currentTests.value, newTest[0]); //XXX kludge?
              const updatedTests = [...currentTests.value, newTest[0]];

              // set new document state -- this will trigger an UI update
              document.setContents(updatedTests);
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

function getLanguageFromUri(uri: vscode.Uri): string {
  const extension = path.extname(uri.path);
  const match = extension.match(/\.catala_(\w+)$/);
  if (match?.[1]) {
    return match[1];
  }
  throw new Error(`Unable to determine language from file name: ${uri}`);
}
