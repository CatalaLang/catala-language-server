import * as vscode from 'vscode';
import { logger } from '../logger';
import { assertUnreachable } from '../util';
import type {
  ParseResults,
  Test,
  TestGenerateResults,
  TestRunResults,
} from '../generated/catala_types';
import {
  type DownMessage,
  readUpMessage,
  writeDownMessage,
  writeTestRunResults,
} from '../generated/catala_types';
import * as path from 'path';
import PQueue from 'p-queue';
import {
  runTestScope,
  parseTestFile,
  generate,
  getAvailableScopes,
  serializeInputs,
} from '../testCaseCompilerInterop';
import { renameIfNeeded } from '../testCaseUtils';
import { fail } from 'assert';
import { TestCaseEditorProvider } from '../testCaseEditor';


// This class contains the 'backend' part of the test case editor that
// sets up the UI, provide initial data and exchanges messages with the
// web view whose entry point is in `uiEntryPoint.ts`
export class ScopeInputController {

  context: vscode.ExtensionContext
  panel: vscode.WebviewPanel
  scope: string
  test: Test

  // We want to restrict shell -> webview messages to instances
  // of DownMessage
  postMessageToWebView(message: DownMessage): void {
    this.panel.webview.postMessage(writeDownMessage(message));
  }

  createWebview(context: vscode.ExtensionContext, file: string, scope: string) {
    this.context = context
    this.panel = vscode.window.createWebviewPanel('catala.scopeInputEditor', `Input for ${scope}`,
      vscode.ViewColumn.One,
      {
        enableScripts: true
      }
    )
    this.scope = scope;

    this.panel.webview.html = this.getHtmlForWebview()

    this.panel.webview.onDidReceiveMessage(async (message: unknown) => {
      const typed_msg = readUpMessage(message);
      logger.log(`RECEIVED MESSAGE: ${typed_msg.kind}`)
      logger.log(`PAYLOAD: ${JSON.stringify(typed_msg)}`)
      switch (typed_msg.kind) {
        case 'Ready':
          const generatedTest: TestGenerateResults = generate(scope, file, true, false)
          if (generatedTest.kind == "Error") {
            logger.log(`ERROR WHILE GENERATING SCOPE`);
            return;
          }
          const value: ParseResults = { kind: "Results", value: generatedTest.value }
          this.postMessageToWebView({
            kind: 'Update',
            value
          });
          this.test = generatedTest.value[0]
          break;
        case 'GuiEdit':
          if (typed_msg.value.length > 0 && typed_msg.value[0].length > 0) {
            this.test = typed_msg.value[0][0]
          }
          break;
        case 'TestRunRequest':
          if (typed_msg.value.in_shell) {
            const result = serializeInputs(this.test.test_inputs);
            if (result.kind == 'Ok')
              vscode.commands.executeCommand('catala.runScope', { uri: file, scope: this.scope, inputs: result.json })
            else
              throw new Error(`Error on test scope run with inputs: ${result.message}`);
          } else if (typed_msg.value.debug) {
            logger.log(`LAUNCH DEBUG`)
            const result = serializeInputs(this.test.test_inputs);
            if (result.kind == 'Ok')
              vscode.commands.executeCommand('catala.debugScope', { uri: file, scope: this.scope, inputs: result.json })
          } else {
            const results: TestRunResults = runTestScope(file, scope, this.test.test_inputs);
            this.postMessageToWebView({
              kind: 'TestRunResults',
              value: { scope, reset_outputs: false, results },
            })
          };
          break;
        case 'ConfirmRequest': {
          this.postMessageToWebView({
            kind: 'ConfirmResult',
            value: { id: typed_msg.value.id, confirmed: true },
          });
          break;
        }
        case 'OpenInTextEditor':
          throw new Error(`Trying to open text editor while in input scope mode`);
        case 'OpenTestScopePicker':
          throw new Error(`Trying to select scope while in input scope mode`);
        case 'TestGenerateRequest':
          throw new Error(`Trying to generate scope while in input scope mode`);
        default:
          assertUnreachable(typed_msg);
      }
    }
    )
  }

  getHtmlForWebview(): string {
    const scriptUri = this.panel.webview.asWebviewUri(
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
              window.Ui.renderInputScopeUi("${language}", "${this.scope}");
            </script>
            </html>
        `;
  }
}
