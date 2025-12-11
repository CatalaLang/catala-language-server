import type { LanguageClient } from 'vscode-languageclient/node';
import * as vscode from 'vscode';
import { spawn } from 'child_process';
import { sep } from 'path';
import { clerkPath, getCwd } from './util_client';
import type { CatalaEntrypoint } from './lspRequests';
import { listEntrypoints } from './lspRequests';

type ClerkLocation = {
  file: string;
  range: vscode.Range;
};

type ClerkScopeTestResult = {
  scope_name: string;
  success: boolean;
  errors: Array<{
    location: ClerkLocation;
    message: string;
  }>;
  time: number;
};

type ClerkTestResult = {
  file: string;
  tests: {
    scopes: ClerkScopeTestResult[];
    'inline-tests': Array<{ cmd: string; success: boolean }>;
  };
};

type ScopeIndex = number;

type ClerkScopeCoverage = {
  index: ScopeIndex;
  name: string;
  location: ClerkLocation;
};

type ClerkCoverageLocations = Array<{
  range: vscode.Range;
  reached_by: number[];
  subtree: ClerkCoverageLocations;
}>;

type ClerkFileCoverage = {
  file: string;
  tree: ClerkCoverageLocations;
};

type ClerkCoverageResult = {
  scopes: Array<ClerkScopeCoverage>;
  locations: Array<ClerkFileCoverage>;
};

type ClerkTestAndCoverageResult = {
  'test-results': ClerkTestResult[];
  coverage: ClerkCoverageResult;
};

type ClerkTestRunResult = {
  results: ClerkTestAndCoverageResult;
  code: number;
  err_msg: string;
};

type TestScope = {
  label: string;
  scope: string;
  kind: 'GUI' | 'Test';
  range: vscode.Range;
};

type TestScopeMap = Array<{
  path: string;
  scopes: TestScope[];
}>;

async function clerkRunTest(
  cwd: string,
  uri: string[],
  cancellation: vscode.CancellationToken,
  with_coverage?: boolean
): Promise<ClerkTestRunResult | Error> {
  const args = ['test', '--json', '--quiet']
    .concat(with_coverage ? ['--code-coverage'] : [])
    .concat(uri);
  return new Promise((resolve) => {
    const proc = spawn(clerkPath, args, {
      ...(cwd && { cwd }),
    });
    cancellation.onCancellationRequested((_) => {
      proc.kill(2);
      resolve(new Error('Clerk run canceled'));
    });
    let output = '';
    proc.stdout.on('data', (data) => {
      output += data;
    });
    let stderr = '';
    proc.stderr.on('data', (data) => {
      stderr += data;
    });
    proc.on('close', (code) => {
      const results = JSON.parse(
        output.toString()
      ) as ClerkTestAndCoverageResult;
      resolve({ results, code: code ?? 0, err_msg: stderr });
    });
  });
}

function lookupTestItem(
  ctrl: vscode.TestController,
  test_file: vscode.Uri,
  scope_name?: string
): vscode.TestItem | undefined {
  const cwd = getCwd(test_file.path);
  if (cwd) {
    let cur_uri = vscode.Uri.file(cwd);
    let file_path = test_file.path.replace(cur_uri.path + sep, '').split(sep);
    let cur_path = vscode.Uri.joinPath(cur_uri, file_path[0]);
    file_path = file_path.slice(1);
    let cur_item = ctrl.items.get(cur_path.path);
    while (cur_item && file_path.length > 0) {
      cur_path = vscode.Uri.joinPath(cur_path, file_path[0]);
      file_path = file_path.slice(1);
      cur_item = cur_item.children.get(cur_path.path);
    }
    if (scope_name)
      return cur_item?.children.get(`${cur_path.path}:${scope_name}`);
    else return cur_item;
  }
}

function populateTestHierarchy(
  ctrl: vscode.TestController,
  pred_item: vscode.TestItem,
  cwd: vscode.Uri,
  path: string[],
  scopes: TestScope[]
): void {
  if (path.length == 0) {
    scopes.forEach((scope) => {
      const item: vscode.TestItem = ctrl.createTestItem(
        `${cwd.path}:${scope.scope}`,
        scope.kind == 'GUI' ? '👁 ' + scope.label : scope.label,
        cwd
      );
      item.range = scope.range;
      pred_item.children.add(item);
    });
  } else {
    var cur_item: vscode.TestItem;
    const uri = vscode.Uri.joinPath(cwd, path[0]);
    const id = uri.path;
    if (pred_item?.children.get(id)) {
      cur_item = pred_item.children.get(id)!;
    } else {
      cur_item = ctrl.createTestItem(id, path[0], uri);
      pred_item?.children.add(cur_item);
    }
    populateTestHierarchy(ctrl, cur_item, uri, path.slice(1), scopes);
  }
}

function populateTestItems(
  ctrl: vscode.TestController,
  path: string,
  scopes: TestScope[]
): void {
  const uri = vscode.Uri.file(path);
  const cwd = getCwd(path);
  if (cwd) {
    const cwd_uri = vscode.Uri.file(cwd);
    const file_path = uri.path.replace(cwd_uri.path + sep, '').split(sep);
    const first_id_uri = vscode.Uri.joinPath(cwd_uri, file_path[0]);
    const item: vscode.TestItem =
      ctrl.items.get(first_id_uri.path) ??
      ctrl.createTestItem(first_id_uri.path, file_path[0], first_id_uri);
    populateTestHierarchy(ctrl, item, first_id_uri, file_path.slice(1), scopes);
    if (!ctrl.items.get(first_id_uri.path)) ctrl.items.add(item);
  }
}

function getAllTests(acc: vscode.TestItem[], testItem: vscode.TestItem): void {
  acc.push(testItem);
  testItem.children.forEach((i) => getAllTests(acc, i));
}

function getAllTestsToRun(
  ctrl: vscode.TestController,
  request: vscode.TestRunRequest
): readonly vscode.TestItem[] {
  if (request.include == undefined || request.include.length == 0) {
    const all_tests: vscode.TestItem[] = new Array();
    ctrl.items.forEach((i) => getAllTests(all_tests, i));
    return all_tests;
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
  const test_item = lookupTestItem(
    ctrl,
    vscode.Uri.file(filePath),
    scopeTest.scope_name == 'compilation' ? undefined : scopeTest.scope_name
  );
  if (test_item) {
    if (scopeTest.success) {
      run.passed(test_item, scopeTest.time);
    } else {
      const messages = scopeTest.errors.map((error) => {
        const msg = new vscode.TestMessage(error.message);
        msg.location = new vscode.Location(
          vscode.Uri.parse(error.location.file),
          error.location.range
        );
        return msg;
      });
      if (scopeTest.scope_name == 'compilation')
        test_item.children.forEach((item) =>
          run.failed(item, messages, scopeTest.time)
        );
      run.failed(test_item, messages, scopeTest.time);
    }
  }
}

function fold_tree(
  acc: vscode.StatementCoverage[],
  trees: ClerkCoverageLocations
): vscode.StatementCoverage[] {
  for (var { range, reached_by, subtree } of trees) {
    acc.push(new vscode.StatementCoverage(reached_by.length, range));
    fold_tree(acc, subtree);
  }
  return acc;
}

function testEntrypointsToTestScopeMap(
  entrypoints: Array<CatalaEntrypoint>
): TestScopeMap {
  let map: Map<string, TestScope[]> = new Map();
  entrypoints.forEach((e) => {
    if (e.entrypoint.kind == 'Test' && e.entrypoint.value.kind == 'Test') {
      const test: TestScope = {
        label: e.entrypoint.value.value.scope,
        scope: e.entrypoint.value.value.scope,
        kind: 'Test',
        range: e.range,
      };
      const prev_arr = map.get(e.path) ?? [];
      map.set(e.path, [...prev_arr, test]);
    } else if (
      e.entrypoint.kind == 'Test' &&
      e.entrypoint.value.kind == 'GUI'
    ) {
      const gui_test: TestScope = {
        label: e.entrypoint.value.value.title ?? e.entrypoint.value.value.scope,
        scope: e.entrypoint.value.value.scope,
        kind: 'GUI',
        range: e.range,
      };
      const prev_arr = map.get(e.path) ?? [];
      map.set(e.path, [...prev_arr, gui_test]);
    }
  });
  return Array.from(map).map((e) => {
    return { path: e[0], scopes: e[1] };
  });
}

export async function initTests(
  context: vscode.ExtensionContext,
  client: LanguageClient
): Promise<void> {
  const ctrl = vscode.tests.createTestController(
    'testController',
    'Catala Tests'
  );
  context.subscriptions.push(ctrl);
  let cwd: string | undefined;

  // Placeholder to display something while tests are retrieved
  ctrl.items.add(ctrl.createTestItem('loading', 'Loading tests...'));

  const updateTestScopes: () => Promise<void> = async () => {
    const entrypoints = await listEntrypoints(client, [
      { kind: 'GUI' },
      { kind: 'Test' },
    ]).finally(() => ctrl.items.replace([]));

    const test_scopes_map: TestScopeMap =
      testEntrypointsToTestScopeMap(entrypoints);
    test_scopes_map.forEach(({ path, scopes }) =>
      populateTestItems(ctrl, path, scopes)
    );
    cwd = getCwd(test_scopes_map?.[0]?.path);
  };

  updateTestScopes();

  ctrl.refreshHandler = async (_token) => await updateTestScopes();

  const testRunHandler = async (
    request: vscode.TestRunRequest,
    cancellation: vscode.CancellationToken,
    with_coverage?: boolean
  ): Promise<void> => {
    const run: vscode.TestRun = ctrl.createTestRun(request);
    const testFiles =
      request.include
        ?.map(({ uri }) => uri?.path)
        ?.filter((p) => p !== undefined) ?? [];
    const testsToRun = getAllTestsToRun(ctrl, request);
    // Mark all tests as started before calling 'clerk test'
    testsToRun.forEach((test) => run.started(test));
    try {
      const thread: Promise<ClerkTestRunResult | Error> = clerkRunTest(
        cwd!,
        testFiles,
        cancellation,
        with_coverage
      );
      cancellation.onCancellationRequested((_) => run.end());
      const clerk_test_result = await thread;
      if (clerk_test_result instanceof Error) throw clerk_test_result;
      const { results, code, err_msg } = clerk_test_result;
      if (code != 0 && err_msg != '')
        console.error(`Clerk exit code: ${code}, Output:\n{err_msg}`);
      results['test-results'].forEach(({ file, tests }) => {
        tests.scopes.forEach((scope_test_result) => {
          updateTestItemWithClerkResult(ctrl, run, file, scope_test_result);
        });
      });

      if (with_coverage) {
        results.coverage.locations.forEach(({ file, tree }) => {
          const statements = fold_tree([], tree);
          run.addCoverage(
            FileCoverageWithDetails.fromDetails(
              vscode.Uri.file(file),
              statements
            )
          );
        });
      }
    } catch (e) {
      testsToRun.forEach((test) => run.errored(test, e));
      console.error('Error while processing test results: ', e);
      vscode.window.showErrorMessage(
        'An error occurred while executing tests. Check the console logs for more details...'
      );
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
