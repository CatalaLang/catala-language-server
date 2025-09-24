import type { LanguageClient } from 'vscode-languageclient/node';
import * as vscode from 'vscode';
import type { ExecException } from 'child_process';
import { execFileSync } from 'child_process';
import { basename } from 'path';
import type { ClerkPosition } from './util_client';
import { clerkPath, getCwd, positionToLocation } from './util_client';

type ClerkTestResult = Array<{
  file: string;
  tests: {
    scopes: Array<{
      scope_name: string;
      success: boolean;
      time: number;
      errors: Array<{
        message: string;
        position: ClerkPosition;
      }>;
    }>;
    'inline-tests': Array<{ cmd: string; success: boolean }>;
  };
}>;

type TestScope = {
  name: string;
  range: vscode.Range;
};

type TestScopeMap = Array<{
  path: string;
  scopes: TestScope[];
}>;

function clerkRunTest(
  cwd: string,
  uri: string[],
  with_coverage?: boolean
): ClerkTestResult {
  try {
    const output = execFileSync(
      clerkPath,
      [
        'test',
        '--report-format',
        'json',
        '--quiet',
        with_coverage ? '--coverage' : '',
      ].concat(uri),
      { ...(cwd && { cwd }) }
    );
    return JSON.parse(output.toString()) as ClerkTestResult;
  } catch (e) {
    if (e.status && e.status === 1 && e.stdout) {
      return JSON.parse(e.stdout) as ClerkTestResult;
    }
    vscode.window.showErrorMessage(
      `Error running clerk test: ${(e as ExecException).message}`
    );
    console.error(e);
    throw e;
  }
}

function populateTestItems(
  ctrl: vscode.TestController,
  path: string,
  scopes: TestScope[]
): void {
  const uri = vscode.Uri.file(path);
  const test_item = ctrl.createTestItem(uri.path, basename(uri.path), uri);

  scopes.forEach((scope) => {
    const item = ctrl.createTestItem(
      `${uri.path}:${scope.name}`,
      scope.name,
      uri
    );
    item.range = scope.range;
    test_item.children.add(item);
  });
  ctrl.items.add(test_item);
}

export function getAllTestsToRun(
  ctrl: vscode.TestController,
  request: vscode.TestRunRequest
): readonly vscode.TestItem[] {
  if (request.include == undefined || request.include.length == 0) {
    const all_tests: vscode.TestItem[] = [];

    ctrl.items.forEach((item) => {
      all_tests.push(item);
      item.children.forEach((subitem) => all_tests.push(subitem));
    });

    return new Array(...all_tests);
  }

  return request.include;
}

export async function initTests(
  context: vscode.ExtensionContext,
  client: LanguageClient
): Promise<void> {
  const ctrl = vscode.tests.createTestController('testController', 'Bla bla');
  context.subscriptions.push(ctrl);

  const test_scopes_map: TestScopeMap = await client.sendRequest(
    'catala.getTestScopes'
  );
  const cwd = getCwd(test_scopes_map?.[0]?.path);

  test_scopes_map.forEach(({ path, scopes }) =>
    populateTestItems(ctrl, path, scopes)
  );

  const testRunHandler = async (
    request: vscode.TestRunRequest,
    _cancellation: vscode.CancellationToken,
    with_coverage?: boolean
  ): Promise<void> => {
    const run = ctrl.createTestRun(request);
    const testFiles =
      request.include
        ?.map(({ uri }) => uri?.path)
        ?.filter((p) => p !== undefined) ?? [];

    const testsToRun = getAllTestsToRun(ctrl, request);

    // Mark all tests as started before calling 'clerk test'
    testsToRun.forEach((test) => run.started(test));

    let test_results;
    try {
      test_results = clerkRunTest(cwd!, testFiles, with_coverage);
    } catch (e) {
      testsToRun.forEach((test) => run.errored(test, e));
      console.error('Error while processing test results:', e);
      vscode.window.showErrorMessage(
        'An error occurred while processing test results. Check the console for details.'
      );
      run.end();
      return;
    }

    test_results.forEach(({ file, tests }) => {
      tests.scopes.forEach((scope_test_result) => {
        // find the corresponding test item
        const test_id = `${file}:${scope_test_result.scope_name}`;
        const test_item = ctrl.items.get(file)?.children.get(test_id);
        if (test_item) {
          if (scope_test_result.success) {
            run.passed(test_item, scope_test_result.time);
          } else {
            const messages = scope_test_result.errors.map((error) => {
              const msg = new vscode.TestMessage(error.message);
              msg.location = positionToLocation(error.position);
              return msg;
            });
            run.failed(test_item, messages, scope_test_result.time);
          }
        }
      });
    });
    run.end();
  };

  const testCoverageHandler = async (
    _request: vscode.TestRunRequest,
    _cancellation: vscode.CancellationToken
  ): Promise<void> => {
    vscode.window.showInformationMessage(
      'Test coverage is not implemented yet.'
    );
  };

  ctrl.createRunProfile(
    'Run tests',
    vscode.TestRunProfileKind.Run,
    testRunHandler,
    true,
    undefined,
    false
  );

  ctrl.createRunProfile(
    'Run tests with coverage',
    vscode.TestRunProfileKind.Coverage,
    testCoverageHandler,
    true,
    undefined,
    false
  );
}
