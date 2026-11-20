/**
 * Stopping the runs of the test overview panel: they go through the same
 * handler as the Test Explorer's, with the panel's cancellation token.
 */
import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { execSync } from 'child_process';
import * as vscode from 'vscode';
import {
  ResultController,
  runTestVscode,
  TestId,
  TestMap,
  type TestCatalog,
} from '../../extension/testAndCoverage';

const fixtures = path.resolve(__dirname, '../../../tests/round_trip');
const testFile = 'test_optionals.catala_en';
const scope = 'Grant_absent';

class MemoryMemento implements vscode.Memento {
  private readonly values = new Map<string, unknown>();
  keys(): readonly string[] {
    return [...this.values.keys()];
  }
  get<T>(key: string, defaultValue?: T): T | undefined {
    return (this.values.get(key) as T | undefined) ?? defaultValue;
  }
  update(key: string, value: unknown): Thenable<void> {
    if (value === undefined) this.values.delete(key);
    else this.values.set(key, value);
    return Promise.resolve();
  }
}

suite('Cancelled test runs', function () {
  this.timeout(120_000);

  const original = vscode.window.showErrorMessage;
  let errors: string[];

  setup(() => {
    errors = [];
    (vscode.window as { showErrorMessage: unknown }).showErrorMessage = (
      message: string
    ): Promise<undefined> => {
      errors.push(message);
      return Promise.resolve(undefined);
    };
  });

  teardown(() => {
    (vscode.window as { showErrorMessage: unknown }).showErrorMessage =
      original;
  });

  test('a cancelled run is no error and stores no result', async () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'catala-cancel-'));
    for (const f of ['clerk.toml', 'optionals.catala_en', testFile]) {
      fs.copyFileSync(path.join(fixtures, f), path.join(dir, f));
    }
    execSync('clerk start', { cwd: dir, stdio: 'pipe' });
    const file = vscode.Uri.file(path.join(dir, testFile));

    const controller = vscode.tests.createTestController(
      'catala-cancel-test',
      'Cancel test'
    );
    const testMap = new TestMap();
    const item = controller.createTestItem(
      new TestId(file, scope).id,
      scope,
      file
    );
    controller.items.add(item);
    testMap.set(new TestId(file, scope), item);
    const catalog: TestCatalog = {
      testMap,
      entrypoints: [],
      cwd: dir,
      refresh: () => Promise.resolve(),
    };
    const results = new ResultController(new MemoryMemento());
    const stop = new vscode.CancellationTokenSource();
    stop.cancel();

    await runTestVscode(
      catalog,
      controller,
      results,
      { kind: 'scope', tests: [{ filename: file, scope }] },
      stop.token
    );

    assert.deepStrictEqual(errors, []);
    assert.strictEqual(results.getResult(new TestId(file, scope)), undefined);
    controller.dispose();
  });
});
