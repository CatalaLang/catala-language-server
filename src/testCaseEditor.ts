import * as vscode from 'vscode';

export class TestCaseEditorProvider implements vscode.CustomTextEditorProvider {
  constructor(private readonly context: vscode.ExtensionContext) {}

  public static register(context: vscode.ExtensionContext): vscode.Disposable {
    const provider = new TestCaseEditorProvider(context);
    console.log(`Registering ${TestCaseEditorProvider.viewType}`);
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

    webviewPanel.webview.html = this.getHtmlForWebview(
      webviewPanel.webview,
      document.getText()
    );

    const changeDocumentSubscription = vscode.workspace.onDidChangeTextDocument(
      (e) => {
        if (e.document.uri.toString() === document.uri.toString()) {
          webviewPanel.webview.postMessage({
            type: 'update',
            text: document.getText(),
          });
        }
      }
    );

    webviewPanel.onDidDispose(() => {
      changeDocumentSubscription.dispose();
    });
  }

  private static readonly viewType = 'catala.testCaseEditor';

  private getHtmlForWebview(webview: vscode.Webview, content: string): string {
    return `
          <!DOCTYPE html>
          <html lang="en">
          <head>
              <meta charset="UTF-8">
              <meta name="viewport" content="width=device-width, initial-scale=1.0">
              <title>Test Case Viewer</title>
              <style>
                  body {
                      border: thick double #FFFACD;
                      padding: 10px;
                  }
                  pre {
                      white-space: pre-wrap;
                      word-wrap: break-word;
                  }
              </style>
          </head>
          <body>
              <pre>${this.escapeHtml(content)}</pre>
              <script>
                  const vscode = acquireVsCodeApi();
                  window.addEventListener('message', event => {
                      const message = event.data;
                      switch (message.type) {
                          case 'update':
                              document.querySelector('pre').textContent = message.text;
                              break;
                      }
                  });
              </script>
          </body>
          </html>
      `;
  }

  private escapeHtml(unsafe: string): string {
    return unsafe
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }
}
