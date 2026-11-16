/**
 * The cleaning of the stored test results: a test is run for real with clerk,
 * its result is stored as a run does, then the test goes away and the listing
 * of the workspace is taken again. The workspace state of the extension is out
 * of reach from here: an in-memory one stands in for it.
 */
import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { execSync } from 'child_process';
import * as vscode from 'vscode';
import {
  ResultController,
  TestId,
  type TestScopeMap,
} from '../../extension/testAndCoverage';

const fixtures = path.resolve(__dirname, '../../../tests/round_trip');

const testFile = 'test_optionals.catala_en';
const scopes = ['Grant_absent', 'Grant_present', 'Grant_hole'];

/** A `vscode.Memento` held in memory, as the workspace state is. */
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

/** A clerk project with the tests of the round-trip fixtures. */
function project(): string {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'catala-prune-'));
  for (const f of ['clerk.toml', 'optionals.catala_en', testFile]) {
    fs.copyFileSync(path.join(fixtures, f), path.join(dir, f));
  }
  execSync('clerk start', { cwd: dir, stdio: 'ignore' });
  return dir;
}

/** Runs the tests of the file with clerk and stores the results, as a run of
 * the Test Explorer does. */
function runTests(dir: string, results: ResultController): void {
  let output: string;
  try {
    output = execSync(`clerk test --json ${testFile}`, {
      cwd: dir,
      stdio: ['ignore', 'pipe', 'ignore'],
    }).toString();
  } catch (e) {
    // A failing test (`Grant_hole`) makes clerk exit with an error: its output
    // still holds every result.
    output = (e as { stdout: Buffer }).stdout.toString();
  }
  results.refresh({ results: JSON.parse(output), code: 0, err_msg: '' });
}

/** The listing of the workspace, as the server gives it. */
function listing(file: string, listed: string[]): TestScopeMap {
  return [
    {
      path: file,
      scopes: listed.map((scope) => ({
        label: scope,
        scope,
        kind: 'GUI' as const,
        range: new vscode.Range(0, 0, 0, 0),
      })),
    },
  ];
}

const stored = (results: ResultController, file: string): string[] =>
  scopes.filter(
    (scope) =>
      results.getResult(new TestId(vscode.Uri.file(file), scope)) !== undefined
  );

suite('Pruning of the stored test results', function () {
  this.timeout(120_000);

  test('the results of a deleted test file are dropped', async () => {
    const dir = project();
    const file = path.join(dir, testFile);
    const results = new ResultController(new MemoryMemento());

    runTests(dir, results);
    assert.deepStrictEqual(stored(results, file), scopes);

    fs.rmSync(file);
    await results.prune([]);
    assert.deepStrictEqual(stored(results, file), []);
  });

  test('the result of a scope removed from a listed file is dropped', async () => {
    const dir = project();
    const file = path.join(dir, testFile);
    const results = new ResultController(new MemoryMemento());

    runTests(dir, results);
    await results.prune(listing(file, ['Grant_absent', 'Grant_present']));
    assert.deepStrictEqual(stored(results, file), [
      'Grant_absent',
      'Grant_present',
    ]);
  });

  test('the results of a file still on the disk but not listed are kept', async () => {
    const dir = project();
    const file = path.join(dir, testFile);
    const results = new ResultController(new MemoryMemento());

    runTests(dir, results);
    // An empty listing: a file that does not compile, or a broken setup.
    await results.prune([]);
    assert.deepStrictEqual(stored(results, file), scopes);
  });
});
