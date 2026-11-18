import * as vscode from 'vscode';
import type { ListedTest } from '../generated/catala_types';
import {
  readTestsUpMessage,
  writeTestsDownMessage,
  type TestsDownMessage,
} from '../generated/catala_types';
import type { CatalaEntrypoint } from './lspRequests';
import { logger } from './logger';
import { atdToCatala } from '../test-case-editor/testCaseCompilerInterop';
import { testScopePicker } from './testCaseEditorProvider';
import path from 'path';
import { CatalaTestCaseDocument } from '../shared/CatalaTestCaseDocument';
import PQueue from 'p-queue';
import type { ResultController, TestCatalog } from './testAndCoverage';
import { fileExists, runTestVscode, TestId } from './testAndCoverage';
import { getCwd } from '../shared/util_client';
import type { SettingsSource } from '../extension/settingsStore';
import { resolveLanguage } from '../shared/settings';

// Path of a test file relative to the workspace folder it belongs to, which is
// what the 'View Catala tests' panel displays: an absolute path is both too long
// for a table column and mostly noise, the interesting part being where the
// test sits in the project. The separator is normalised so that the web view
// need not care which platform the path was built on. Undefined when the file
// belongs to no workspace folder: there is then nothing to be relative to, and
// the panel falls back to the absolute path.
function relativeFilename(filename: string): string | undefined {
  const cwd = getCwd(filename);
  if (cwd == undefined) {
    return undefined;
  }
  return path.relative(cwd, filename).split(path.sep).join('/');
}

// This class contains the 'backend' part of the test case editor that
// sets up the UI, provide initial data and exchanges messages with the
// web view whose entry point is in `uiEntryPoint.ts`
export class TestMacroController {
  // Undefined until the panel is created, and reset to undefined when the
  // user closes it (a disposed panel can neither be revealed nor posted to).
  panel: vscode.WebviewPanel | undefined;
  tests: ListedTest[] = [];

  private following: vscode.Disposable = new vscode.Disposable(() => {});

  constructor(private readonly settings: SettingsSource) {}

  private testQueue: PQueue = new PQueue({ concurrency: 1 });

  // We want to restrict shell -> webview messages to instances
  // of TestsDownMessage
  postMessageToWebView(message: TestsDownMessage): void {
    this.panel?.webview.postMessage(writeTestsDownMessage(message));
  }

  // A test with no stored result after a run could not run: a failure.
  private postResult(
    test: ListedTest,
    reorder: boolean,
    resultController: ResultController
  ): void {
    const res = resultController.getResult(
      new TestId(vscode.Uri.file(test.filename), test.test.value.scope)
    );
    this.postMessageToWebView({
      kind: 'TestScopeResult',
      value: {
        entry: test.test,
        scope_success: res
          ? { success: res.success, date: res.date }
          : { success: false, date: new Date().toISOString() },
        test_id: test.test_id,
        reorder,
      },
    });
  }

  handleCatalaEntrypoint(
    entrypoints: CatalaEntrypoint[],
    resultController: ResultController
  ): void {
    for (let index = 0; index < entrypoints.length; index++) {
      const e = entrypoints[index];
      const filename = e.path;
      if (e.entrypoint.kind == 'Test') {
        let testId = new TestId(
          vscode.Uri.file(filename),
          e.entrypoint.value.value.scope
        );
        const res = resultController.getResult(testId);
        this.tests.push({
          test_id: testId.id,
          filename: filename,
          relative_filename: relativeFilename(filename),
          test: e.entrypoint.value,
          last_run: res && { success: res.success, date: res.date },
        });
      } else {
        throw new Error(`Unexpected test from ${path}`);
      }
    }
    this.postMessageToWebView({ kind: 'AllTests', value: this.tests });
  }

  // Reveal the panel if it is still alive, otherwise (re)create it.
  public show(
    context: vscode.ExtensionContext,
    resultController: ResultController,
    testController: vscode.TestController,
    catalog: TestCatalog,
    columnToShowIn: vscode.ViewColumn | undefined
  ): void {
    if (this.panel != undefined) {
      this.panel.reveal(columnToShowIn);
      return;
    }
    this.createWebView(context, resultController, testController, catalog);
  }

  public createWebView(
    context: vscode.ExtensionContext,
    resultController: ResultController,
    testController: vscode.TestController,
    catalog: TestCatalog
  ): void {
    const panel = vscode.window.createWebviewPanel(
      'viewAllTests',
      'View Catala tests',
      vscode.ViewColumn.One,
      {
        enableScripts: true,
        retainContextWhenHidden: true,
      }
    );
    this.panel = panel;
    panel.title = 'View Catala tests';
    panel.webview.html = this.getHtmlForWebview(panel, context);

    // Once the user closes the panel it is disposed for good: drop our
    // reference so the next invocation creates a fresh one instead of
    // revealing (or posting to) a disposed webview.
    panel.onDidDispose(() => {
      this.following.dispose();
      if (this.panel === panel) {
        this.panel = undefined;
        this.tests = [];
      }
    });

    panel.webview.onDidReceiveMessage(async (message: unknown) => {
      const typed_msg = readTestsUpMessage(message);
      switch (typed_msg.kind) {
        case 'Ready': {
          this.following.dispose();
          this.following = this.settings.follow(panel.webview);
          this.tests = [];
          // The panel's reload is the Test Explorer's refresh: one listing.
          await catalog.refresh();
          this.handleCatalaEntrypoint(catalog.entrypoints, resultController);
          break;
        }
        case 'RunTests': {
          if (this.tests.length == 0) {
            break;
          }
          let ids = typed_msg.value;
          // Reordering is worth it when a run refreshes several results at
          // once — the whole list (`[]`) or a filtered selection. A single
          // test must leave the order alone, or the list moves under the
          // pointer that just clicked it.
          const reorder = ids.length != 1;
          if (ids.length == 0) {
            await runTestVscode(catalog, testController, resultController, {
              kind: 'all',
            });
            for (const test of this.tests) {
              this.postResult(test, reorder, resultController);
            }
          } else {
            // Looked up by id, not position: the list may have been reloaded
            // since the panel sent the request. A test that is gone is
            // skipped rather than replaced by whichever one took its place.
            const byId = new Map(
              this.tests.map((test) => [test.test_id, test])
            );
            let testToRun = ids.flatMap((id) => {
              const test = byId.get(id);
              if (test == undefined) {
                logger.log(`Test to run not found, skipped: ${id}`);
                return [];
              }
              return [test];
            });
            if (testToRun.length == 0) {
              break;
            }
            await this.testQueue.add(async () => {
              // One run for all the tests, then their results one by one.
              await runTestVscode(catalog, testController, resultController, {
                kind: 'scope',
                tests: testToRun.map((test) => ({
                  filename: vscode.Uri.file(test.filename),
                  scope: test.test.value.scope,
                })),
              });
              for (const test of testToRun) {
                logger.log(`Test to run: ${JSON.stringify(test)}`);
                this.postResult(test, reorder, resultController);
              }
            });
          }
          break;
        }
        case 'OpenInTestEditor': {
          let uri: vscode.Uri = vscode.Uri.file(typed_msg.value);
          await vscode.commands.executeCommand(
            'vscode.openWith',
            uri,
            'catala.testCaseEditor'
          );
          break;
        }
        case 'OpenInTextEditor': {
          if (typed_msg.value) {
            let uri = vscode.Uri.file(typed_msg.value.value);
            vscode.commands.executeCommand('vscode.openWith', uri, 'default');
          }
          break;
        }
        case 'OpenTestScopePicker': {
          const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
          if (!workspaceFolder) {
            logger.log('No workspace folder open');
            break;
          }
          let uri: vscode.Uri | undefined;
          let defaultUri = vscode.Uri.joinPath(
            workspaceFolder.uri,
            'test.catala_fr'
          );
          // This loop only finishes if the fileName created by the user contains the
          // string "test". Otherwise, we can't open the Catala Test Case Editor properly
          // later.
          for (;;) {
            uri = await vscode.window.showSaveDialog({
              defaultUri,
              saveLabel: vscode.l10n.t('Create test file'),
              filters: {
                Catala: ['catala_fr', 'catala_en', 'catala_pl'],
              },
            });
            if (!uri) {
              // User cancelled the dialog.
              break;
            }
            const fileName = path.basename(uri.fsPath).toLowerCase();
            if (fileName.includes('test')) {
              // FileName includes test, the Catala Test Case Editor will be
              // properly displayed so we can exit the infinite loop.
              break;
            }
            await vscode.window.showErrorMessage(
              vscode.l10n.t('Invalid test file name'),
              {
                modal: true,
                detail: vscode.l10n.t(
                  'The test file name must contain the word "test".'
                ),
              }
            );
            // Re-open the dialog on the rejected file so the user can fix it.
            defaultUri = uri;
          }
          if (!uri) {
            // User cancelled the dialog.
            break;
          }
          // Read by CatalaTestCaseDocument.create: a new file has to exist first.
          let alreadyExist = await fileExists(uri);
          if (!alreadyExist) {
            await vscode.workspace.fs.writeFile(uri, new Uint8Array());
          }
          let document = await CatalaTestCaseDocument.create(uri, undefined);
          let result = await testScopePicker(document);
          if (result.length == 0) {
            // Picker cancelled: leave no empty file behind, but never touch a
            // file the user chose to add a test to.
            if (!alreadyExist) {
              await vscode.workspace.fs.delete(uri);
            }
            return;
          }
          let catalaSource = atdToCatala(result, document.language);
          await vscode.workspace.fs.writeFile(
            uri,
            Buffer.from(catalaSource, 'utf-8')
          );
          await vscode.commands.executeCommand(
            'vscode.openWith',
            uri,
            'catala.testCaseEditor'
          );
          break;
        }
      }
    });
  }

  private getHtmlForWebview(
    panel: vscode.WebviewPanel,
    context: vscode.ExtensionContext
  ): string {
    const scriptUri = panel.webview.asWebviewUri(
      vscode.Uri.joinPath(context.extensionUri, 'dist', 'ui.js')
    );

    const language = resolveLanguage(
      this.settings.get(),
      undefined,
      vscode.env.language
    );

    return `
            <!DOCTYPE html>
            <html lang="en">
            <head>
                <meta charset="UTF-8">
                <meta name="viewport" content="width=device-width, initial-scale=1.0">
                <title>Check all tests</title>
                <style>
                    body {
                        padding: 100px;
                    }
                </style>
            </head>
            <body>
                <div id="root"></div>
            </body>
            <script src="${scriptUri}"></script>
            <script>
              window.Ui.renderMacroTestsUi("${language}");
            </script>
            </html>
        `;
  }
}
