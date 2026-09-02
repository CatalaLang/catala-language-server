/**
 * The document model of a broken test, driven directly: no webview, no UI.
 * Runs in the extension host with the real `vscode.workspace.fs`, on a copy of
 * the round-trip fixtures with one scope input renamed underneath the tests.
 */
import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { execSync } from 'child_process';
import * as vscode from 'vscode';
import { CatalaTestCaseDocument } from '../../shared/CatalaTestCaseDocument';

const fixtures = path.resolve(__dirname, '../../../tests/round_trip');

/** A project whose tests no longer fit their scope. `broken` says how. */
function project(broken: 'field renamed' | 'module renamed'): string {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'catala-doc-'));
  for (const f of ['clerk.toml', 'test_optionals.catala_en']) {
    fs.copyFileSync(path.join(fixtures, f), path.join(dir, f));
  }
  const module = fs.readFileSync(
    path.join(fixtures, 'optionals.catala_en'),
    'utf8'
  );
  if (broken === 'field renamed') {
    fs.writeFileSync(
      path.join(dir, 'optionals.catala_en'),
      module.replace(/\bbase\b/g, 'amount')
    );
  } else {
    fs.writeFileSync(
      path.join(dir, 'benefits.catala_en'),
      module.replace('> Module Optionals', '> Module Benefits')
    );
  }
  execSync('clerk start', { cwd: dir, stdio: 'ignore' });
  return dir;
}

const token = new vscode.CancellationTokenSource().token;

suite('Broken test document', function () {
  this.timeout(120_000);

  test('opens on the rebuild, saves beside the original, replaces it', async () => {
    const dir = project('field renamed');
    const file = path.join(dir, 'test_optionals.catala_en');
    const uri = vscode.Uri.file(file);
    const original = fs.readFileSync(file);

    const doc = await CatalaTestCaseDocument.create(uri, undefined);
    assert.strictEqual(doc.parseResults.kind, 'BrokenTest');
    assert.strictEqual(doc.rebuilt?.length, 3);

    await doc.saveAs(uri, token);
    assert.ok(fs.existsSync(file + '.updated'), 'saved to the working copy');
    assert.ok(fs.readFileSync(file).equals(original), 'original untouched');

    await doc.replaceOriginal();
    assert.ok(!fs.existsSync(file + '.updated'), 'working copy removed');
    assert.strictEqual(doc.parseResults.kind, 'Results');
    const replaced = fs.readFileSync(file, 'utf8');
    assert.notStrictEqual(replaced, original.toString());
    assert.ok(replaced.includes('> Using Optionals'));
    assert.ok(!replaced.includes('.base equals'), 'the renamed field is gone');
    assert.ok(replaced.includes('.bonus equals'), 'unchanged fields carried');

    // ...and what was written reads as an ordinary, healthy test
    const reopened = await CatalaTestCaseDocument.create(uri, undefined);
    assert.strictEqual(reopened.parseResults.kind, 'Results');
  });

  test('discards the working copy and starts over', async () => {
    const dir = project('field renamed');
    const file = path.join(dir, 'test_optionals.catala_en');
    const uri = vscode.Uri.file(file);
    const original = fs.readFileSync(file);

    const doc = await CatalaTestCaseDocument.create(uri, undefined);
    await doc.saveAs(uri, token);
    assert.ok(fs.existsSync(file + '.updated'));

    await doc.discardWorkingCopy();
    assert.ok(!fs.existsSync(file + '.updated'), 'working copy removed');
    assert.strictEqual(doc.parseResults.kind, 'BrokenTest');
    assert.strictEqual(doc.rebuilt?.length, 3);
    assert.ok(fs.readFileSync(file).equals(original), 'original untouched');
  });

  test('a blocked rebuild has nothing to save, and saving does not fail', async () => {
    const dir = project('module renamed');
    const file = path.join(dir, 'test_optionals.catala_en');
    const uri = vscode.Uri.file(file);

    const doc = await CatalaTestCaseDocument.create(uri, undefined);
    assert.strictEqual(doc.parseResults.kind, 'BrokenTest');
    assert.strictEqual(doc.rebuilt?.length, 0);

    await doc.saveAs(uri, token);
    assert.ok(!fs.existsSync(file + '.updated'), 'nothing written');
    await assert.rejects(
      doc.replaceOriginal(),
      /could not be rebuilt/,
      'nothing to replace the original with'
    );
  });
});
