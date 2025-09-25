import type { LanguageClient } from 'vscode-languageclient/node';
import * as vscode from 'vscode';
import type { ExecException } from 'child_process';
import { execFileSync } from 'child_process';
import { basename } from 'path';
import type { ClerkPosition } from './util_client';
import {
  clerkPath,
  getCwd,
  positionToLocation,
  positionToRange,
} from './util_client';

type ClerkScopeTestResult = {
  scope_name: string;
  success: boolean;
  time: number;
  errors: Array<{
    message: string;
    position: ClerkPosition;
  }>;
};

type ClerkTestResult = {
  file: string;
  tests: {
    scopes: ClerkScopeTestResult[];
    'inline-tests': Array<{ cmd: string; success: boolean }>;
  };
};

type ClerkCoverageEntry = {
  location: Omit<ClerkPosition, 'fname'>;
  reached_by: number;
};

type ClerkCoverageResult = Array<{
  filename: string;
  coverage_map: ClerkCoverageEntry[];
}>;

type ClerkTestAndCoverageResult = {
  'test-results': ClerkTestResult[];
  coverage: ClerkCoverageResult;
};

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
): ClerkTestAndCoverageResult {
  try {
    const args = ['test', '--report-format=json', '--quiet']
      .concat(with_coverage ? ['--code-coverage'] : [])
      .concat(uri);

    const output = execFileSync(clerkPath, args, {
      ...(cwd && { cwd }),
    });
    return JSON.parse(output.toString()) as ClerkTestAndCoverageResult;
  } catch (e) {
    if (e.status && e.status === 1 && e.stdout) {
      return JSON.parse(e.stdout) as ClerkTestAndCoverageResult;
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

class FileCoverageWithDetails extends vscode.FileCoverage {
  private details: vscode.FileCoverageDetail[];

  private constructor(
    fileCoverage: vscode.FileCoverage,
    details: readonly vscode.FileCoverageDetail[]
  ) {
    super(
      fileCoverage.uri,
      fileCoverage.statementCoverage,
      fileCoverage.branchCoverage,
      fileCoverage.declarationCoverage,
      fileCoverage.includesTests
    );
    this.details = [...details];
  }

  public static fromDetails(
    uri: vscode.Uri,
    details: readonly vscode.FileCoverageDetail[]
  ): FileCoverageWithDetails {
    const fileCoverage = vscode.FileCoverage.fromDetails(uri, details);
    return new FileCoverageWithDetails(fileCoverage, details);
  }

  public async getDetails(): Promise<vscode.FileCoverageDetail[]> {
    vscode.window.showInformationMessage(
      `Getting details: ${this.details.length}`
    );
    return this.details;
  }
}

function updateTestItemWithClerkResult(
  ctrl: vscode.TestController,
  run: vscode.TestRun,
  filePath: string,
  scopeTest: ClerkScopeTestResult
): void {
  // find the corresponding test item
  const test_id = `${filePath}:${scopeTest.scope_name}`;
  const test_item = ctrl.items.get(filePath)?.children.get(test_id);

  if (test_item) {
    if (scopeTest.success) {
      run.passed(test_item, scopeTest.time);
    } else {
      const messages = scopeTest.errors.map((error) => {
        const msg = new vscode.TestMessage(error.message);
        msg.location = positionToLocation(error.position);
        return msg;
      });
      run.failed(test_item, messages, scopeTest.time);
    }
  }
}

function getStatementCoveragesFromClerkResult(
  coverage_map: ClerkCoverageEntry[]
): vscode.StatementCoverage[] {
  return (
    coverage_map.reduce(
      (statements: vscode.StatementCoverage[], { location, reached_by }) => {
        statements.push(
          new vscode.StatementCoverage(reached_by, positionToRange(location))
        );
        return statements;
      },
      []
    ) ?? []
  );
}

export async function initTests(
  context: vscode.ExtensionContext,
  client: LanguageClient
): Promise<void> {
  const ctrl = vscode.tests.createTestController(
    'testController',
    'Test runner'
  );
  context.subscriptions.push(ctrl);

  let cwd: string | undefined;

  const updateTestScopes = async () => {
    const test_scopes_map: TestScopeMap = await client.sendRequest(
      'catala.getTestScopes'
    );
    ctrl.items.replace([]);

    test_scopes_map.forEach(({ path, scopes }) =>
      populateTestItems(ctrl, path, scopes)
    );

    cwd = getCwd(test_scopes_map?.[0]?.path);
  };

  updateTestScopes();

  ctrl.refreshHandler = (_token) => {
    updateTestScopes();
    vscode.window.showInformationMessage('Test scopes updated');
  };

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

    try {
      const results = clerkRunTest(cwd!, testFiles, with_coverage);

      results['test-results'].forEach(({ file, tests }) => {
        tests.scopes.forEach((scope_test_result) => {
          updateTestItemWithClerkResult(ctrl, run, file, scope_test_result);
        });
      });

      if (with_coverage) {
        results.coverage.forEach(({ filename, coverage_map }) => {
          const statements = getStatementCoveragesFromClerkResult(coverage_map);
          run.addCoverage(
            FileCoverageWithDetails.fromDetails(
              vscode.Uri.file(filename),
              statements
            )
          );
        });
      }
    } catch (e) {
      testsToRun.forEach((test) => run.errored(test, e));
      console.error('Error while processing test results:', e);
      vscode.window.showErrorMessage(
        'An error occurred while processing test results. Check the console for details.'
      );
      run.end();
      return;
    }

    run.end();
  };

  ctrl.createRunProfile(
    'Run tests',
    vscode.TestRunProfileKind.Run,
    testRunHandler,
    true,
    undefined,
    false
  );

  const profile = ctrl.createRunProfile(
    'Run tests with coverage',
    vscode.TestRunProfileKind.Coverage,
    (a, b) => testRunHandler(a, b, true),
    true,
    undefined,
    false
  );

  profile.loadDetailedCoverage = async (
    _testRun: vscode.TestRun,
    coverage: vscode.FileCoverage,
    _token: vscode.CancellationToken
  ) => {
    return coverage instanceof FileCoverageWithDetails
      ? await coverage.getDetails()
      : [];
  };
}
