import * as vscode from 'vscode';
import { logger } from './logger';
import { assertUnreachable } from '../shared/util';

import type {
  ParseResults,
  TestList,
  TestRunResults,
  UpMessage,
} from '../generated/catala_types';
import {
  type DownMessage,
  readUpMessage,
  writeDownMessage,
} from '../generated/catala_types';
import * as path from 'path';
import PQueue from 'p-queue';
import {
  runTestScope,
  parseTestFile,
  generate,
  getAvailableScopes,
} from '../test-case-editor/testCaseCompilerInterop';
import { renameIfNeeded } from '../test-case-editor/testCaseUtils';
import { CatalaTestCaseDocument } from '../shared/CatalaTestCaseDocument';
import { TraceEditorProvider } from './traceEditorProvider';
import { runTrace, readTraceFile } from '../trace-editor/traceRunner';
import { mkdtempSync, rmSync } from 'fs';
import { tmpdir } from 'os';
import type { TraceElement } from '../trace-editor/traceUtils';
import type { ResultController } from './testAndCoverage';
import { TestId } from './testAndCoverage';
import type { CheckExpected } from './lspRequests';
import { getCwd } from '../shared/util_client';

export function parseContents(
  content: Uint8Array,
  uri: vscode.Uri,
  language: string
): ParseResults {
  const documentText = new TextDecoder('utf-8').decode(content);
  return parseTestFile(documentText, uri.fsPath, language);
}

export async function testScopePicker(
  document: CatalaTestCaseDocument
): Promise<TestList> {
  try {
    const ws = vscode.workspace.getWorkspaceFolder(document.uri);
    const wsPath = ws?.uri.fsPath;
    const entries = (await vscode.commands.executeCommand(
      'catala.listTestableScopes',
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

    if (!picked) return [];

    let filename: string | undefined;
    let scopeUnderTest: string | undefined;

    if (picked === browseItem) {
      const fileUri = await vscode.window.showOpenDialog({
        filters: {
          'Catala Files': ['catala_fr', 'catala_en', 'catala_pl'],
        },
      });
      if (!fileUri?.[0]) return [];
      filename = fileUri[0].fsPath;

      const scopes = await getAvailableScopes(filename);
      const pickedScope = await vscode.window.showQuickPick(
        scopes.map((s) => ({ label: s.name })),
        {
          placeHolder: `Select a scope in ${path.basename(filename)}`,
        }
      );
      if (!pickedScope) return [];
      scopeUnderTest = pickedScope.label;
    } else {
      scopeUnderTest = picked.label;
      filename = picked.description;
    }

    if (!filename || !scopeUnderTest) return [];

    const results = generate(scopeUnderTest, filename, false, true);
    if (results.kind === 'Results') {
      const newTest = results.value;

      const currentTests = document.parseResults;
      if (currentTests.kind === 'Results') {
        newTest[0] = renameIfNeeded(currentTests.value, newTest[0]);
        const updatedTests = [...currentTests.value, newTest[0]];

        document.scheduleChange(updatedTests, false);

        return updatedTests;
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
  return [];
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

  constructor(
    private readonly context: vscode.ExtensionContext,
    /** dist-relative path to the emitted `codicon.css`. */
    private readonly codiconsCssPath: string,
    private resultController: ResultController,
    private readonly checkExpected: CheckExpected
  ) {
    this.testQueue = new PQueue({ concurrency: 1 });
    this.resultController = resultController;
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

  public static register(
    context: vscode.ExtensionContext,
    codiconsCssPath: string,
    resultController: ResultController,
    checkExpected: CheckExpected
  ): vscode.Disposable {
    const provider = new TestCaseEditorProvider(
      context,
      codiconsCssPath,
      resultController,
      checkExpected
    );
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
    const checkVariable = await this.checkExpected(
      getCwd(document.uri.fsPath) ?? path.dirname(document.uri.fsPath)
    );
    logger.log(`Check Variable: ${checkVariable}`);
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
      scope: string,
      traceFile?: string
    ): Promise<TestRunResults> {
      return runTestScope(fileName, scope, undefined, traceFile);
    }

    function applyGuiEdit(
      typed_msg: Extract<UpMessage, { kind: 'GuiEdit' }>
    ): void {
      document.scheduleChange(typed_msg.value[0], typed_msg.value[1]);
    }

    async function sendTrace(): Promise<void> {
      const parsed = document.parseResults;
      if (parsed.kind !== 'Results') {
        return;
      }
      for (const test of parsed.value) {
        const result = await runTrace(document.uri.fsPath, test.testing_scope);
        if (!result.ok) {
          logger.log(
            `Could not compute trace for scope ${test.testing_scope}: ${result.error}`
          );
          continue;
        }
        webviewPanel.webview.postMessage({
          kind: 'trace',
          scope: test.testing_scope,
          trace: result.trace,
        });
      }
    }

    webviewPanel.webview.onDidReceiveMessage(async (message: unknown) => {
      if (
        message !== null &&
        typeof message === 'object' &&
        (message as { kind?: unknown }).kind === 'openTraceEditor'
      ) {
        const scope = (message as { scope?: unknown }).scope;
        const scopeStr = typeof scope === 'string' ? scope : undefined;
        const trace = (message as { trace?: TraceElement[] }).trace;
        const parsed = document.parseResults;
        const test =
          scopeStr !== undefined && parsed.kind === 'Results'
            ? parsed.value.find((t) => t.testing_scope === scopeStr)
            : undefined;
        await TraceEditorProvider.openWith(document.uri, {
          scope: scopeStr,
          test,
          trace,
        });
        return;
      }
      const typed_msg = readUpMessage(message);
      switch (typed_msg.kind) {
        case 'Ready': {
          logger.log(`Got ready message from webview, sending parsed document`);
          postMessageToWebView({
            kind: 'Update',
            value: document.parseResults,
          });
          TestCaseEditorProvider.markReady(document.uri);
          void sendTrace();
          break;
        }
        case 'GuiEdit': {
          applyGuiEdit(typed_msg);
          break;
        }
        case 'TestRunRequest': {
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
            void vscode.commands.executeCommand(
              'catala.testcase.reportResult',
              document.uri,
              typed_msg.value.scope,
              { kind: 'Cancelled' }
            );
            return;
          }
          const { scope, reset_outputs, has_expected } = typed_msg.value;
          if (reset_outputs) {
            const confirmation = await vscode.window.showInformationMessage(
              vscode.l10n.t(
                'Replace expected outputs with test run results. Are you sure?'
              ),
              { modal: true },
              { title: vscode.l10n.t('Replace'), action: 'Reset' }
            );

            if (confirmation?.action !== 'Reset') {
              postMessageToWebView({
                kind: 'TestRunResults',
                value: {
                  scope,
                  reset_outputs,
                  results: { kind: 'Cancelled' },
                },
              });
              void vscode.commands.executeCommand(
                'catala.testcase.reportResult',
                document.uri,
                scope,
                { kind: 'Cancelled' }
              );
              return;
            }
          }
          // The trace is only produced when the test declares expected variables:
          // it is what the compiler checks them against, and what the webview
          // displays. A temporary directory keeps it out of the project.
          let traceDir: string | undefined;
          let traceFile: string | undefined;
          if (has_expected && checkVariable) {
            try {
              traceDir = mkdtempSync(path.join(tmpdir(), 'catala-test-trace-'));
              traceFile = path.join(traceDir, 'trace.json');
            } catch (err) {
              logger.log(
                `Could not create a temporary trace file, running without a trace: ${String(err)}`
              );
            }
          }

          const results = await this.testQueue.add(() =>
            runTest(document.uri.fsPath, scope, traceFile)
          );

          if (traceFile !== undefined) {
            const trace = readTraceFile(traceFile);
            if (trace.ok) {
              // Hand-written message, like `sendTrace` above: the webview
              // intercepts this kind before the strict ATD `readDownMessage`,
              // so it cannot go through `postMessageToWebView`.
              webviewPanel.webview.postMessage({
                kind: 'trace',
                scope,
                trace: trace.trace,
              });
            } else {
              logger.log(
                `Could not read the trace of scope ${scope}: ${trace.error}`
              );
            }
          }
          if (traceDir !== undefined) {
            rmSync(traceDir, { recursive: true, force: true });
          }

          // This run does not go through clerk, so nothing else would record
          // it: without this the General Tests view would keep showing the
          // outcome of the previous run for that scope.
          this.resultController.record(
            new TestId(document.uri, scope),
            scope,
            results
          );

          postMessageToWebView({
            kind: 'TestRunResults',
            value: { scope, reset_outputs, results },
          });
          void vscode.commands.executeCommand(
            'catala.testcase.reportResult',
            document.uri,
            scope,
            results
          );

          if (reset_outputs) {
            if (results.kind === 'Ok') {
              document.resetTestOutputs(scope, results.value.test_outputs);
            }
          }

          break;
        }
        case 'TestGenerateRequest': {
          const { scope_under_test, filename } = typed_msg.value;
          const results = generate(scope_under_test, filename, false, true);
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
          let result = await testScopePicker(document);
          if (result.length > 0) {
            postMessageToWebView({
              kind: 'Update',
              value: { kind: 'Results', value: result },
            });
          }
          break;
        }
        case 'ConfirmRequest': {
          const { id, action } = typed_msg.value;

          let prompt: string;
          let buttons: Array<{
            title: string;
            action: 'Delete' | 'RunAnyway' | 'Reset';
          }>;
          let successAction: 'Delete' | 'RunAnyway' | 'Reset';

          switch (action.kind) {
            case 'DeleteArrayElement':
              prompt = vscode.l10n.t('Delete this element?');
              buttons = [{ title: vscode.l10n.t('Delete'), action: 'Delete' }];
              successAction = 'Delete';
              break;
            case 'DeleteAssertion':
              prompt = vscode.l10n.t('Delete this assertion?');
              buttons = [{ title: vscode.l10n.t('Delete'), action: 'Delete' }];
              successAction = 'Delete';
              break;
            case 'RunTestWithUnsetValues':
              prompt = vscode.l10n.t(
                'This test contains one or more unset or invalid values. The run will likely fail. Do you want to run it anyway?'
              );
              buttons = [
                { title: vscode.l10n.t('Run anyway'), action: 'RunAnyway' },
              ];
              successAction = 'RunAnyway';
              break;
            case 'ResetContextVar':
              prompt = vscode.l10n.t(
                'Remove the override for this context variable? The current value will be lost.'
              );
              buttons = [{ title: vscode.l10n.t('Reset'), action: 'Reset' }];
              successAction = 'Reset';
              break;
            default:
              assertUnreachable(action as never);
          }

          const confirmation = await vscode.window.showWarningMessage(
            prompt,
            { modal: true },
            ...buttons
          );
          postMessageToWebView({
            kind: 'ConfirmResult',
            value: { id, confirmed: confirmation?.action === successAction },
          });
          break;
        }
        case 'SpecificTestRequest':
          throw new Error(`Unexpected SpecificTestRequest`);
        case 'OpenInTestEditor':
          throw new Error(`Unexpected OpenInTestEditor`);
        case 'Reload':
          throw new Error('Unexpected Reload');
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
    // vscode-elements' components look up this stylesheet by id to load the
    // Codicons font into their shadow DOM.
    const codiconsUri = webview.asWebviewUri(
      vscode.Uri.joinPath(
        this.context.extensionUri,
        'dist',
        this.codiconsCssPath
      )
    );

    const language = vscode.env.language;

    return `
          <!DOCTYPE html>
          <html lang="en">
          <head>
              <meta charset="UTF-8">
              <meta name="viewport" content="width=device-width, initial-scale=1.0">
              <title>Test Case Editor</title>
              <link href="${codiconsUri}" id="vscode-codicon-stylesheet" rel="stylesheet" />
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
