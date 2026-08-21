import * as vscode from 'vscode';
import type { TestDebugger } from '../generated/catala_types';
import {
  readUpMessage,
  writeDownMessage,
  type DownMessage,
} from '../generated/catala_types';
import { type LanguageClient } from 'vscode-languageclient/node';
import type { CatalaEntrypoint } from '../extension/lspRequests';
import { listEntrypoints } from '../extension/lspRequests';
import { logger } from '../extension/logger';
import { atdToCatala } from './testCaseCompilerInterop';
import { testScopePicker } from '../extension/testCaseEditorProvider';
import path from 'path';
import { CatalaTestCaseDocument } from '../shared/CatalaTestCaseDocument';
import PQueue from 'p-queue';
import type { ResultController } from '../extension/testAndCoverage';
import { runTestVscode, TestId, TestMap } from '../extension/testAndCoverage';
import { getCwd } from '../shared/util_client';

// Path of a test file relative to the workspace folder it belongs to, which is
// what the 'debug all tests' panel displays: an absolute path is both too long
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
  tests: TestDebugger[] = [];

  private testQueue: PQueue = new PQueue({ concurrency: 1 });

  // We want to restrict shell -> webview messages to instances
  // of DownMessage
  postMessageToWebView(message: DownMessage): void {
    this.panel?.webview.postMessage(writeDownMessage(message));
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
        let res = resultController.getResult(testId);
        if (res != undefined) {
          let testEntrypoint = {
            index,
            filename: filename,
            relative_filename: relativeFilename(filename),
            test: e.entrypoint.value,
            success:
              res.success && res.expected !== undefined
                ? res.expected.length == 0
                : true,
            date: res.date,
          };
          this.tests.push(testEntrypoint);
        } else {
          let testEntrypoint = {
            index,
            filename: filename,
            relative_filename: relativeFilename(filename),
            test: e.entrypoint.value,
          };
          this.tests.push(testEntrypoint);
        }
      } else {
        throw new Error(`Unexpected test from ${path}`);
      }
    }
    this.postMessageToWebView({ kind: 'AllTests', value: this.tests });
  }

  // Reveal the panel if it is still alive, otherwise (re)create it.
  public show(
    client: LanguageClient,
    context: vscode.ExtensionContext,
    catala_entry: Promise<CatalaEntrypoint[]>,
    resultController: ResultController,
    testController: vscode.TestController,
    columnToShowIn: vscode.ViewColumn | undefined
  ): void {
    if (this.panel != undefined) {
      this.panel.reveal(columnToShowIn);
      return;
    }
    this.createWebView(
      client,
      context,
      catala_entry,
      resultController,
      testController
    );
  }

  public createWebView(
    client: LanguageClient,
    context: vscode.ExtensionContext,
    catala_entry: Promise<CatalaEntrypoint[]>,
    resultController: ResultController,
    testController: vscode.TestController
  ): void {
    const panel = vscode.window.createWebviewPanel(
      'debugAllTests',
      'Catala debug tests',
      vscode.ViewColumn.One,
      {
        enableScripts: true,
        retainContextWhenHidden: true,
      }
    );
    this.panel = panel;
    panel.title = 'Catala debug tests';
    panel.webview.html = this.getHtmlForWebview(panel, context);

    // Once the user closes the panel it is disposed for good: drop our
    // reference so the next invocation creates a fresh one instead of
    // revealing (or posting to) a disposed webview.
    panel.onDidDispose(() => {
      if (this.panel === panel) {
        this.panel = undefined;
        this.tests = [];
      }
    });

    panel.webview.onDidReceiveMessage(async (message: unknown) => {
      const typed_msg = readUpMessage(message);
      switch (typed_msg.kind) {
        case 'Ready': {
          this.tests = [];
          const entrypoints = await catala_entry;
          this.handleCatalaEntrypoint(entrypoints, resultController);
          break;
        }
        case 'Reload': {
          this.tests = [];
          const entrypoints = await listEntrypoints(
            client,
            [{ kind: 'GUI' }, { kind: 'Test' }],
            undefined,
            false,
            true
          );
          for (let o of entrypoints) {
            logger.log(`Entrypoint: ${JSON.stringify(o)}`);
          }
          this.handleCatalaEntrypoint(entrypoints, resultController);
          break;
        }
        case 'SpecificTestRequest': {
          if (this.tests.length == 0) {
            break;
          }
          const cwd = getCwd(this.tests[0].filename);
          let ids = typed_msg.value;
          // Reordering is worth it when a run refreshes several results at
          // once — the whole list (`[]`) or a filtered selection. A single
          // test must leave the order alone, or the list moves under the
          // pointer that just clicked it.
          const order = ids.length != 1;
          if (ids.length == 0) {
            await runTestVscode(
              cwd!,
              new TestMap(),
              testController,
              resultController,
              { kind: 'all' }
            );
            for (let index = 0; index < this.tests.length; index++) {
              const testElt = this.tests[index];
              let testId = new TestId(
                vscode.Uri.file(testElt.filename),
                testElt.test.value.scope
              );
              let res = resultController.getResult(testId);
              if (res != undefined) {
                this.postMessageToWebView({
                  kind: 'TestScopeResult',
                  value: {
                    entry: testElt.test,
                    scope_success: {
                      success: res.success && res.expected.length == 0,
                      date: res.date,
                    },
                    index,
                    order,
                  },
                });
              } else {
                let date = new Date().toLocaleDateString('fr');
                this.postMessageToWebView({
                  kind: 'TestScopeResult',
                  value: {
                    entry: testElt.test,
                    scope_success: { success: false, date },
                    index,
                    order,
                  },
                });
              }
            }
          } else {
            let testToRun = ids.map(
              (index) => [index, this.tests[index]] as const
            );
            await this.testQueue.add(async () => {
              for (const [index, test] of testToRun) {
                await runTestVscode(
                  cwd!,
                  new TestMap(),
                  testController,
                  resultController,
                  {
                    kind: 'scope',
                    filename: test.filename,
                    scope: test.test.value.scope,
                  }
                );
                let testId = new TestId(
                  vscode.Uri.file(test.filename),
                  test.test.value.scope
                );
                let res = resultController.getResult(testId);
                if (res != undefined) {
                  this.postMessageToWebView({
                    kind: 'TestScopeResult',
                    value: {
                      entry: test.test,
                      scope_success: {
                        success: res.success && res.expected.length == 0,
                        date: res.date,
                      },
                      index,
                      order,
                    },
                  });
                } else {
                  let date = new Date().toLocaleDateString('fr');
                  this.postMessageToWebView({
                    kind: 'TestScopeResult',
                    value: {
                      entry: test.test,
                      scope_success: { success: false, date },
                      index,
                      order,
                    },
                  });
                }
              }
            });
          }
          break;
        }
        case 'OpenInTestEditor': {
          let uri: vscode.Uri = vscode.Uri.parse(typed_msg.value);
          await vscode.commands.executeCommand(
            'vscode.openWith',
            uri,
            'catala.testCaseEditor'
          );
          break;
        }
        case 'OpenInTextEditor': {
          if (typed_msg.value) {
            let uri = vscode.Uri.parse(typed_msg.value.value);
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
          await vscode.workspace.fs.writeFile(uri, Buffer.from('', 'utf-8'));
          let document = await CatalaTestCaseDocument.create(uri, undefined);
          let result = await testScopePicker(document);
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

    const language = vscode.env.language;

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
