import * as vscode from 'vscode';

export class CatalaTestCaseEditorProvider implements vscode.CustomTextEditorProvider {

    constructor(
		private readonly context: vscode.ExtensionContext
	) { }

    public static register(context: vscode.ExtensionContext): vscode.Disposable {
		const provider = new CatalaTestCaseEditorProvider(context);
		const providerRegistration = vscode.window.registerCustomEditorProvider(CatalaTestCaseEditorProvider.viewType, provider);
		return providerRegistration;
	}

    public async resolveCustomTextEditor(
		document: vscode.TextDocument,
		webviewPanel: vscode.WebviewPanel,
		_token: vscode.CancellationToken
	): Promise<void> {

    }

	private static readonly viewType = 'catala.testCaseEditor';
}