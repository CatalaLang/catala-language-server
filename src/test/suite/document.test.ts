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

const fixtures = path.resolve(
  __dirname,
  '../../../tests/cli/fixtures/round_trip'
);

/** A project whose tests no longer fit their scope. `broken` says how. */
function project(
  broken: 'field renamed' | 'module renamed' | 'nothing'
): string {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'catala-doc-'));
  for (const f of ['clerk.toml', 'test_optionals.catala_en']) {
    fs.copyFileSync(path.join(fixtures, f), path.join(dir, f));
  }
  const module = fs.readFileSync(
    path.join(fixtures, 'optionals.catala_en'),
    'utf8'
  );
  if (broken === 'nothing') {
    fs.writeFileSync(path.join(dir, 'optionals.catala_en'), module);
  } else if (broken === 'field renamed') {
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

/** A basket of two lines whose element type lost a field. */
function itemsProject(): string {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'catala-doc-'));
  for (const f of ['clerk.toml', 'test_items.catala_en']) {
    fs.copyFileSync(path.join(fixtures, f), path.join(dir, f));
  }
  const module = fs.readFileSync(
    path.join(fixtures, 'items.catala_en'),
    'utf8'
  );
  fs.writeFileSync(
    path.join(dir, 'items.catala_en'),
    module.replace('data taxed content boolean', 'data exempt content boolean')
  );
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
    assert.ok(fs.existsSync(file + '.repair'), 'saved to the working copy');
    assert.ok(fs.readFileSync(file).equals(original), 'original untouched');

    await doc.replaceOriginal();
    assert.ok(!fs.existsSync(file + '.repair'), 'working copy removed');
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
    assert.ok(fs.existsSync(file + '.repair'));

    await doc.discardWorkingCopy();
    assert.ok(!fs.existsSync(file + '.repair'), 'working copy removed');
    assert.strictEqual(doc.parseResults.kind, 'BrokenTest');
    assert.strictEqual(doc.rebuilt?.length, 3);
    assert.ok(fs.readFileSync(file).equals(original), 'original untouched');
  });

  test('re-committing identical content is not an undo stop', async () => {
    const dir = project('field renamed');
    const uri = vscode.Uri.file(path.join(dir, 'test_optionals.catala_en'));

    const doc = await CatalaTestCaseDocument.create(uri, undefined);
    const initial = doc.rebuilt;
    assert.ok(initial?.length === 3);

    let edits = 0;
    doc.onDidChange(() => (edits += 1));
    // The same content again, as a blur or normalisation pass would send it.
    doc.setRebuilt(structuredClone(initial), false);
    assert.strictEqual(edits, 0, 'a no-op change landed on the undo stack');
    // A real change still registers.
    doc.setRebuilt(initial.slice(1), false);
    assert.strictEqual(edits, 1);
  });

  test('an edit still in the batching window never lands after discard or undo', async () => {
    {
      const dir = project('field renamed');
      const uri = vscode.Uri.file(path.join(dir, 'test_optionals.catala_en'));

      const doc = await CatalaTestCaseDocument.create(uri, undefined);
      const initial = doc.rebuilt;
      assert.ok(initial?.length === 3);

      // A batched edit: its 350 ms timer is still pending when we discard.
      doc.setRebuilt(initial.slice(1), true);
      await doc.discardWorkingCopy();
      // Let a stray timer fire; it must not resurrect the discarded edit.
      await new Promise((r) => setTimeout(r, 500));

      const results = doc.parseResults;
      assert.ok(results.kind === 'BrokenTest');
      assert.strictEqual(
        results.value.tests.filter((t) => t.rebuilt !== undefined).length,
        3,
        'the discarded edit stayed discarded'
      );
    }
    {
      const dir = project('field renamed');
      const doc = await CatalaTestCaseDocument.create(
        vscode.Uri.file(path.join(dir, 'test_optionals.catala_en')),
        undefined
      );
      const initial = doc.rebuilt;
      assert.ok(initial?.length === 3);
      let lastEdit:
        | vscode.CustomDocumentEditEvent<CatalaTestCaseDocument>
        | undefined;
      doc.onDidChange((e) => (lastEdit = e));
      doc.setRebuilt(initial.slice(1), false);
      // Batched: its timer is pending when undo runs.
      doc.setRebuilt(initial.slice(2), true);
      assert.ok(lastEdit !== undefined);
      lastEdit.undo();
      await new Promise((r) => setTimeout(r, 500));
      assert.strictEqual(
        doc.rebuilt?.length,
        3,
        'the pending edit landed on top of the undo'
      );
    }
  });

  test('undo steps the rebuild back, and parseResults says so', async () => {
    const dir = project('field renamed');
    const uri = vscode.Uri.file(path.join(dir, 'test_optionals.catala_en'));

    const doc = await CatalaTestCaseDocument.create(uri, undefined);
    assert.strictEqual(doc.parseResults.kind, 'BrokenTest');
    const initial = doc.rebuilt;
    assert.ok(initial?.length === 3);

    // The edit VS Code would put on the undo stack.
    let lastEdit:
      | vscode.CustomDocumentEditEvent<CatalaTestCaseDocument>
      | undefined;
    doc.onDidChange((e) => (lastEdit = e));

    doc.setRebuilt(initial.slice(1), false);
    assert.strictEqual(doc.rebuilt?.length, 2);
    let results = doc.parseResults;
    assert.ok(results.kind === 'BrokenTest');
    assert.strictEqual(
      results.value.tests.filter((t) => t.rebuilt !== undefined).length,
      2,
      'parseResults reports the live rebuild'
    );

    assert.ok(lastEdit !== undefined);
    lastEdit.undo();
    results = doc.parseResults;
    assert.ok(results.kind === 'BrokenTest');
    assert.strictEqual(
      results.value.tests.filter((t) => t.rebuilt !== undefined).length,
      3,
      'after undo, parseResults reports the stepped-back rebuild'
    );

    lastEdit.redo();
    results = doc.parseResults;
    assert.ok(results.kind === 'BrokenTest');
    assert.strictEqual(
      results.value.tests.filter((t) => t.rebuilt !== undefined).length,
      2,
      'redo steps forward'
    );
  });

  test('undo is inert once the original was replaced', async () => {
    const dir = project('field renamed');
    const file = path.join(dir, 'test_optionals.catala_en');
    const doc = await CatalaTestCaseDocument.create(
      vscode.Uri.file(file),
      undefined
    );
    const initial = doc.rebuilt;
    assert.ok(initial?.length === 3);
    let lastEdit:
      | vscode.CustomDocumentEditEvent<CatalaTestCaseDocument>
      | undefined;
    doc.onDidChange((e) => (lastEdit = e));
    doc.setRebuilt(initial.slice(1), false);
    await doc.replaceOriginal();
    assert.strictEqual(doc.parseResults.kind, 'Results');

    assert.ok(lastEdit !== undefined);
    lastEdit.undo();
    assert.strictEqual(
      doc.parseResults.kind,
      'Results',
      'undo brought the broken test back'
    );
    assert.strictEqual(doc.rebuilt, undefined);
  });

  test('retarget stamps array items with uids', async () => {
    const dir = itemsProject();
    const file = path.join(dir, 'test_items.catala_en');
    const doc = await CatalaTestCaseDocument.create(
      vscode.Uri.file(file),
      undefined
    );
    const results = doc.parseResults;
    assert.ok(results.kind === 'BrokenTest');
    const lines = (t: {
      test_inputs: Map<string, { value?: { value: { value: unknown } } }>;
    }): { attrs: { kind: string }[] }[] => {
      const v = t.test_inputs.get('lines')?.value?.value.value as {
        kind: string;
        value: { attrs: { kind: string }[] }[];
      };
      assert.strictEqual(v.kind, 'Array');
      return v.value;
    };
    const stamped = (t: Parameters<typeof lines>[0]): boolean =>
      lines(t).every((item) => item.attrs.some((a) => a.kind === 'Uid'));
    // The wire as the rebuild command hands it over: no uids on the rebuilt copy.
    const unstamped = structuredClone(results);
    for (const pair of unstamped.value.tests) {
      if (pair.rebuilt) for (const item of lines(pair.rebuilt)) item.attrs = [];
    }
    doc.retarget(unstamped);
    const after = doc.parseResults;
    assert.ok(after.kind === 'BrokenTest');
    for (const pair of after.value.tests) {
      assert.ok(
        pair.rebuilt && stamped(pair.rebuilt),
        'rebuilt items lost their uids'
      );
    }
  });

  test('a working copy left over once the test reads again is discarded on open', async () => {
    const dir = project('nothing');
    const file = path.join(dir, 'test_optionals.catala_en');
    fs.writeFileSync(file + '.repair', 'left from an earlier repair');
    const doc = await CatalaTestCaseDocument.create(
      vscode.Uri.file(file),
      undefined
    );
    assert.strictEqual(doc.parseResults.kind, 'Results');
    assert.ok(!fs.existsSync(file + '.repair'), 'the leftover is still there');

    // A broken test's working copy is in use: kept.
    const broken = project('field renamed');
    const bfile = path.join(broken, 'test_optionals.catala_en');
    const bdoc = await CatalaTestCaseDocument.create(
      vscode.Uri.file(bfile),
      undefined
    );
    await bdoc.saveAs(vscode.Uri.file(bfile), token);
    await CatalaTestCaseDocument.create(vscode.Uri.file(bfile), undefined);
    assert.ok(
      fs.existsSync(bfile + '.repair'),
      'a working copy in use was discarded'
    );
  });

  test('a blocked rebuild has nothing to save, and saving does not fail', async () => {
    const dir = project('module renamed');
    const file = path.join(dir, 'test_optionals.catala_en');
    const uri = vscode.Uri.file(file);

    const doc = await CatalaTestCaseDocument.create(uri, undefined);
    assert.strictEqual(doc.parseResults.kind, 'BrokenTest');
    assert.strictEqual(doc.rebuilt?.length, 0);

    await doc.saveAs(uri, token);
    assert.ok(!fs.existsSync(file + '.repair'), 'nothing written');
    await assert.rejects(
      doc.replaceOriginal(),
      /could not be rebuilt/,
      'nothing to replace the original with'
    );
  });
});
