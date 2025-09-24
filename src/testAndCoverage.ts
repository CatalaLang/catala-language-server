import type { LanguageClient } from 'vscode-languageclient/node';
import * as vscode from 'vscode';
import type { ExecException } from 'child_process';
import { execFileSync } from 'child_process';
import { basename } from 'path';
import type { ClerkPosition } from './util_client';
import { clerkPath, getCwd, positionToLocation } from './util_client';

type ClerkTestResult = {
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
};

type ClerkCoverageResult = Array<{
  filename: string;
  coverage_map: Array<{
    location: Omit<ClerkPosition, 'fname'>;
    reached_by: string[];
  }>;
}>;

type ClerkTestAndCoverageResult = {
  'test-results': ClerkTestResult[];
  coverage: ClerkCoverageResult;
};

type ClerkReachableStatement = Record<string, ClerkPosition[]>;
//   file: string;
//   positions: ClerkPosition[];
// };

type TestScope = {
  name: string;
  range: vscode.Range;
};

type TestScopeMap = Array<{
  path: string;
  scopes: TestScope[];
}>;

function clerkGetAllRachableStatements(cwd: string): ClerkReachableStatement {
  return {};
  // try {
  //   const output = execFileSync(
  //     clerkPath,
  //     ['reachable', '--report-format', 'json', '--quiet'],
  //     { ...(cwd && { cwd }) }
  //   );
  //   return JSON.parse(output.toString()) as ClerkReachableStatement[];
  // } catch (e) {
  //   vscode.window.showErrorMessage(
  //     `Error running clerk test: ${(e as ExecException).message}`
  //   );
  //   console.error(e);
  //   throw e;
  // }
}
function clerkRunTest(
  cwd: string,
  uri: string[],
  with_coverage?: boolean
): ClerkTestAndCoverageResult {
  try {
    const output = execFileSync(
      clerkPath,
      [
        'test',
        '--report-format',
        'json',
        '--quiet',
        with_coverage ? '--code-coverage' : '',
      ].concat(uri),
      { ...(cwd && { cwd }) }
    );
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

// class StatefulTestController {
//   private coverages: Record<string, FileCoverageWithDetails>;
//   private ctrl: vscode.TestController;
//
//   constructor(ctrl: vscode.TestController) {
//     this.ctrl = ctrl;
//   }
//
//   addCoverage(coverage: FileCoverageWithDetails) {
//     this.coverages[coverage.uri.path] = coverage
//     this.ctrl.
//   }
// }

export async function initTests(
  context: vscode.ExtensionContext,
  client: LanguageClient
): Promise<void> {
  const ctrl = vscode.tests.createTestController(
    'testController',
    'Test runner'
  );
  context.subscriptions.push(ctrl);

  let cwd: string | undefined; //= getCwd(test_scopes_map?.[0]?.path);

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
    vscode.window.showInformationMessage('Refreshing tests...');
    updateTestScopes();
  };

  let first = true;

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
    let coverage_results;
    // let all_reachable_statements: ClerkReachableStatement = {};

    try {
      // if (with_coverage) {
      //   // TODO: correctly manage all reachable statements
      //   all_reachable_statements = clerkGetAllRachableStatements(cwd!);
      // }
      const results = clerkRunTest(cwd!, testFiles, with_coverage);
      test_results = results['test-results'];
      coverage_results = results.coverage;

      test_results.forEach(({ file, tests }) => {
        console.log(`Processing results for file: ${file}`);
        tests.scopes.forEach((scope_test_result) => {
          console.log(`  Scope: ${scope_test_result.scope_name}`);
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

      if (with_coverage) {
        coverage_results.forEach(({ filename, coverage_map }) => {
          const { statements, test_files } = coverage_map.reduce(
            (
              acc: {
                statements: vscode.StatementCoverage[];
                test_files: string[];
              },
              { location, reached_by }
            ) => {
              // delete all_reachable_statements[filename];
              acc.statements.push(
                new vscode.StatementCoverage(
                  test_files.length,
                  new vscode.Range(
                    new vscode.Position(
                      location.start_lnum - 1,
                      location.start_cnum
                    ),
                    new vscode.Position(
                      location.end_lnum - 1,
                      location.end_cnum
                    )
                  )
                )
              );

              // acc.test_files.concat(test_files);
              return acc;
            },
            { statements: [], test_files: [] }
          ) ?? { statements: [], test_files: [] };

          const fileCoverage = vscode.FileCoverage.fromDetails(
            vscode.Uri.file(filename),
            statements
          );

          // FIXME: should implement [TestRunProfile.loadDetailedCoverageForTest] to be used
          // FIXME: maybe we need to remove doublons
          fileCoverage.includesTests = test_files
            .map((file) => ctrl.items.get(file))
            .filter((i) => i !== undefined);

          run.addCoverage(fileCoverage);
        });

        //   Object.entries(all_reachable_statements).forEach(
        //     ([filename, statements]) => {
        //       run.addCoverage(
        //         vscode.FileCoverage.fromDetails(
        //           vscode.Uri.file(filename),
        //           statements.map(
        //             (pos) =>
        //               new vscode.StatementCoverage(
        //                 0,
        //                 new vscode.Range(
        //                   new vscode.Position(pos.start_lnum - 1, pos.start_cnum),
        //                   new vscode.Position(pos.end_lnum - 1, pos.end_cnum)
        //                 )
        //               )
        //           )
        //         )
        //       );
        //     }
        //   );
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
    _token
  ) => {
    const details =
      coverage instanceof FileCoverageWithDetails
        ? await coverage.getDetails()
        : [];
    vscode.window.showInformationMessage(
      `Load details: ${coverage instanceof FileCoverageWithDetails} ${details.length}`
    );
    return details;
  };
}
