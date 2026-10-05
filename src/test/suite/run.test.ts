/**
 * Running a test with an assertion still to fill, as the test case editor
 * does: the assertion is left out, so the run yields every output.
 */
import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { execSync } from 'child_process';
import * as vscode from 'vscode';
import type { TestRunResults } from '../../generated/catala_types';
import { CatalaTestCaseDocument } from '../../shared/CatalaTestCaseDocument';
import { withoutUnfilledAssertions } from '../../editors/unsetValidation';
import {
  runRebuiltTest,
  runSavedTest,
} from '../../test-case-editor/testCaseCompilerInterop';

const fixtures = path.resolve(__dirname, '../../../tests/round_trip');
const filled = 'assertion (calc.total = $1000.00)';
// What the editor writes for an assertion added but not filled yet
const unfilled = 'assertion (calc.total = impossible)';

async function project(
  assertion: string
): Promise<{ dir: string; file: string; doc: CatalaTestCaseDocument }> {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'catala-run-'));
  for (const f of ['clerk.toml', 'optionals.catala_en']) {
    fs.copyFileSync(path.join(fixtures, f), path.join(dir, f));
  }
  const file = path.join(dir, 'test_optionals.catala_en');
  fs.writeFileSync(
    file,
    fs
      .readFileSync(path.join(fixtures, 'test_optionals.catala_en'), 'utf8')
      .replace(filled, assertion)
  );
  execSync('clerk start', { cwd: dir, stdio: 'ignore' });
  const doc = await CatalaTestCaseDocument.create(
    vscode.Uri.file(file),
    undefined
  );
  return { dir, file, doc };
}

// The editor runs catala from the workspace folder; there is none here
function inDir<T>(dir: string, f: () => T): T {
  const cwd = process.cwd();
  process.chdir(dir);
  try {
    return f();
  } finally {
    process.chdir(cwd);
  }
}

function assertOutputs(results: TestRunResults): void {
  assert.strictEqual(results.kind, 'Ok', JSON.stringify(results));
  if (results.kind === 'Ok') assert.ok(results.value.test_outputs.has('total'));
}

suite('Running a test', function () {
  this.timeout(120_000);

  test('a saved test with an assertion still to fill yields its outputs', async () => {
    const { dir, file, doc } = await project(unfilled);
    assertOutputs(
      inDir(dir, () =>
        runSavedTest(doc.parseResults, file, 'Grant_absent', 'en')
      )
    );
  });

  test('a complete saved test still runs', async () => {
    const { dir, file, doc } = await project(filled);
    assertOutputs(
      inDir(dir, () =>
        runSavedTest(doc.parseResults, file, 'Grant_absent', 'en')
      )
    );
  });

  test('a working copy with an assertion still to fill yields its outputs', async () => {
    const { dir, file, doc } = await project(unfilled);
    const parsed = doc.parseResults;
    assert.strictEqual(parsed.kind, 'Results');
    if (parsed.kind !== 'Results') return;
    assertOutputs(
      inDir(dir, () =>
        runRebuiltTest(
          withoutUnfilledAssertions(parsed.value),
          'Grant_absent',
          'en',
          file
        )
      )
    );
  });
});
