/**
 * The extension as a user meets it: activation, then a test file opened in the
 * test case editor. What the page draws is out of reach; what it is sent is
 * checked through the same document model.
 */
import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { execSync } from 'child_process';
import * as vscode from 'vscode';
import { CatalaTestCaseDocument } from '../../shared/CatalaTestCaseDocument';

const fixtures = path.resolve(__dirname, '../../../tests/round_trip');

function within<T>(ms: number, what: string, p: Thenable<T>): Promise<T> {
  return Promise.race([
    Promise.resolve(p),
    new Promise<T>((_, reject) =>
      setTimeout(
        () => reject(new Error(`${what}: no answer after ${ms / 1000} s`)),
        ms
      )
    ),
  ]);
}

suite('Extension, end to end', function () {
  this.timeout(180_000);

  test('starts, and opens a test in the test case editor', async () => {
    const ext = vscode.extensions.getExtension('catalalang.catala');
    assert.ok(ext, 'extension not found');
    await within(30_000, 'activate()', ext.activate());

    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'catala-e2e-'));
    for (const f of [
      'clerk.toml',
      'optionals.catala_en',
      'test_optionals.catala_en',
    ]) {
      fs.copyFileSync(path.join(fixtures, f), path.join(dir, f));
    }
    execSync('clerk start', { cwd: dir, stdio: 'ignore' });
    const uri = vscode.Uri.file(path.join(dir, 'test_optionals.catala_en'));

    await within(
      60_000,
      'opening the test case editor',
      vscode.commands.executeCommand(
        'vscode.openWith',
        uri,
        'catala.testCaseEditor'
      )
    );
    const input = vscode.window.tabGroups.activeTabGroup.activeTab?.input;
    assert.ok(
      input instanceof vscode.TabInputCustom &&
        input.viewType === 'catala.testCaseEditor' &&
        input.uri.toString() === uri.toString(),
      'the active tab is not the test case editor on the test file'
    );

    const document = await CatalaTestCaseDocument.create(uri, undefined);
    assert.strictEqual(document.parseResults.kind, 'Results');
  });
});
