import * as assert from 'assert';

import * as vscode from 'vscode';
import { TestCaseEditorProvider } from '../../extension/testCaseEditorProvider';
import { ResultController } from '../../extension/testAndCoverage';

/**
 * A memento that forgets everything. Registration only stores its
 * collaborators without reading them, so nothing here needs to persist.
 */
const dummyStorage: vscode.Memento = {
  keys: () => [],
  get: <T>(_key: string, defaultValue?: T): T | undefined => defaultValue,
  update: () => Promise.resolve(),
};

/** The provider only stores this collaborator; it is never called here. */
const checkExpected = (_clerk_toml_dir: string): Promise<boolean> =>
  Promise.resolve(false);

suite('Extension Test Suite', () => {
  suiteTeardown(() => {
    vscode.window.showInformationMessage('All tests done!');
  });

  test('Extension registers successfully', () => {
    const context = {} as vscode.ExtensionContext; // Mock context
    const resultController = new ResultController(dummyStorage, 'catala_en');
    // A real controller rather than a stub: the test host provides one for
    // free, and it has to be disposed either way.
    const disposable = TestCaseEditorProvider.register(
      context,
      'codicon.css',
      resultController,
      checkExpected
    );
    assert.ok(disposable);
    disposable.dispose();
  });
});
