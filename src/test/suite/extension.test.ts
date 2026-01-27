import * as assert from 'assert';

import * as vscode from 'vscode';
import { TestCaseEditorProvider } from '../../extension/testCaseEditorProvider';

suite('Extension Test Suite', () => {
  suiteTeardown(() => {
    vscode.window.showInformationMessage('All tests done!');
  });

  test('Extension registers successfully', () => {
    const context = {} as vscode.ExtensionContext; // Mock context
    const disposable = TestCaseEditorProvider.register(context);
    assert.ok(disposable);
    disposable.dispose();
  });
});
