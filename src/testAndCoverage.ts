import type { LanguageClient } from 'vscode-languageclient/node';
import * as vscode from 'vscode';
import { spawn } from 'child_process';
import { sep } from 'path';
import { clerkPath, getCwd } from './util_client';
import type { CatalaEntrypoint } from './lspRequests';
import { listEntrypoints } from './lspRequests';
import {
  focusDiffInCustomEditor,
  updateOpenCustomEditorWithResults,
} from './testCaseEditor';
import { runTestScope } from './testCaseCompilerInterop';
import type {
  Diff,
  RuntimeValue,
  SourcePosition,
  TestOutputs,
  TestRunResults,
} from './generated/catala_types';

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
  paths: string[],
  cancellation: vscode.CancellationToken,
  with_coverage?: boolean
): Promise<ClerkTestRunResult | Error> {
  const args = ['test', '--json', '--quiet']
    .concat(with_coverage ? ['--code-coverage'] : [])
    .concat(paths);
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
    proc.on('close', (code: number | null) => {
      try {
        const results = JSON.parse(
          output.toString()
        ) as ClerkTestAndCoverageResult;
        if (results?.['test-results']) {
          resolve({ results, code: code ?? 0, err_msg: stderr });
        } else {
          resolve(new Error('Clerk run failed:\n' + stderr));
        }
      } catch (e) {
        resolve(new Error('Clerk run failed:\n' + e + '\n' + stderr));
      }
    });
  });
}

function firstDiffLocation(
  diffs: Diff[],
  outputs: TestOutputs,
  file: vscode.Uri
): vscode.Location | undefined {
  if (!diffs.length) return;
  const first = diffs[0];
  const seg0 = first.path[0];
  if (seg0?.kind !== 'StructField') return;

  const field = seg0.value;
  const out = outputs.get(field);
  const pos: SourcePosition | undefined = out?.value?.pos;
  if (!pos) return;

  // SourcePosition is 1-based; VS Code expects 0-based
  const range = new vscode.Range(
    new vscode.Position(pos.start_line - 1, pos.start_column - 1),
    new vscode.Position(pos.end_line - 1, pos.end_column - 1)
  );
  return new vscode.Location(file, range);
}

function formatDiffs(diffs: Diff[]): string {
  const seg = (s: Diff['path'][number]): string => {
    switch (s.kind) {
      case 'StructField':
        return `.${s.value}`;
      case 'ListIndex':
        return `[${s.value}]`;
      case 'TupleIndex':
        return `(${s.value})`;
      case 'EnumPayload':
        return `<${s.value}>`;
    }
  };
  const pp = (rv: RuntimeValue): string => {
    switch (rv.value.kind) {
      case 'Bool':
      case 'Integer':
      case 'Decimal':
        return String(rv.value.value);
      case 'Money':
        return `$${(rv.value.value / 100).toFixed(2)}`;
      case 'Date': {
        const d = rv.value.value;
        return `|${d.year}-${String(d.month).padStart(2, '0')}-${String(d.day).padStart(2, '0')}|`;
      }
      case 'Duration':
        return JSON.stringify(rv.value.value);
      case 'Enum':
        return `${rv.value.value[0].enum_name}.${rv.value.value[1][0]}`;
      case 'Struct':
        return rv.value.value[0].struct_name;
      case 'Array':
        return `[${rv.value.value.map(pp).join(', ')}]`;
      default:
        return '<value>';
    }
  };
  return diffs
    .map(
      (d) =>
        `Diff at ${d.path.map(seg).join('')}:\n  expected: ${pp(
          d.expected
        )}\n  actual:   ${pp(d.actual)}`
    )
    .join('\n\n');
}

// Shared helper to apply results to a single TestItem and report to a TestRun
function applyResultsToTestItem(
  tr: vscode.TestRun,
  item: vscode.TestItem,
  file: vscode.Uri,
  results: TestRunResults
): void {
  if (results.kind === 'Ok') {
    const out = results.value;
    const diffs = out.diffs ?? [];
    const hasFailures = out.assert_failures || diffs.length > 0;
    if (hasFailures) {
      // Prefer focusing the custom editor and displaying diffs there when
      // run from controller; location still attached for Test Explorer
      const msg = new vscode.TestMessage(formatDiffs(diffs));
      const loc = firstDiffLocation(diffs, out.test_outputs, file);
      if (loc) msg.location = loc;
      tr.failed(item, msg);
    } else {
      tr.passed(item);
    }
  } else if (results.kind === 'Cancelled') {
    tr.skipped(item);
  } else {
    tr.errored(item, new vscode.TestMessage(results.value));
  }
}

async function processGUITest(
  run: vscode.TestRun,
  test: vscode.TestItem,
  file: string,
  scope: string
): Promise<void> {
  try {
    const uri = vscode.Uri.file(file);
    const res: TestRunResults = await runTestScope(file, scope);
    if (res.kind === 'Ok') {
      const out = res.value;
      const diffs = out.diffs ?? [];
      const hasFailures = out.assert_failures || diffs.length > 0;
      if (hasFailures) {
        // Prefer focusing the custom editor and displaying diffs there
        // TODO: SHOULD WE?
        await focusDiffInCustomEditor(uri, scope, res);
      } else {
        // Update GUI editor if open to clear stale diffs on success
        await updateOpenCustomEditorWithResults(uri, scope, res);
      }
    }
    applyResultsToTestItem(run, test, uri, res);
  } catch (e) {
    run.errored(test, new vscode.TestMessage(String(e)));
  }
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

const gui_test_tag: vscode.TestTag = new vscode.TestTag('GUI');
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
      item.tags = scope.kind == 'GUI' ? [gui_test_tag] : [];
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
  test_item: vscode.TestItem,
  run: vscode.TestRun,
  scopeTest: ClerkScopeTestResult
): void {
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
  const ctrl = vscode.tests.createTestController('catalaTests', 'Catala Tests');
  context.subscriptions.push(ctrl);
  let cwd: string | undefined;

  // Placeholder to display something while tests are retrieved
  ctrl.items.add(ctrl.createTestItem('loading', 'Loading tests...'));

  const updateTestScopes: () => Promise<void> = async () => {
    const entrypoints = await listEntrypoints(
      client,
      [{ kind: 'GUI' }, { kind: 'Test' }],
      undefined,
      false,
      true
    ).finally(() => ctrl.items.replace([]));

    const test_scopes_map: TestScopeMap =
      testEntrypointsToTestScopeMap(entrypoints);
    test_scopes_map.forEach(({ path, scopes }) =>
      populateTestItems(ctrl, path, scopes)
    );
    cwd = getCwd(test_scopes_map?.[0]?.path);
  };

  updateTestScopes();

  ctrl.refreshHandler = async (_token): Promise<void> =>
    await updateTestScopes();

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
      let test_gui_threads: Array<Promise<void>> = [];
      results['test-results'].forEach(({ file, tests }) => {
        tests.scopes.forEach((scope_test_result) => {
          const test_item = lookupTestItem(
            ctrl,
            vscode.Uri.file(file),
            scope_test_result.scope_name == 'compilation'
              ? undefined
              : scope_test_result.scope_name
          );
          if (test_item) {
            if (
              !with_coverage &&
              test_item.tags[0] === gui_test_tag &&
              scope_test_result.scope_name
            ) {
              const gui_thread: Promise<void> = processGUITest(
                run,
                test_item,
                file,
                scope_test_result.scope_name
              );
              test_gui_threads.push(gui_thread);
            } else {
              updateTestItemWithClerkResult(test_item, run, scope_test_result);
            }
          }
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
      });
      // Await GUI tests processing
      await Promise.all(test_gui_threads);
    } catch (e) {
      testsToRun.forEach((test) => run.errored(test, e));
      console.error('Error while processing test results: ', e);
      vscode.window.showErrorMessage(
        'Unexpected error while executing tests. Check the console logs for more details...'
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
  ): Promise<vscode.FileCoverageDetail[]> => {
    return coverage instanceof FileCoverageWithDetails
      ? await coverage.getDetails()
      : [];
  };

  // Update when button run is pressed in testcase ui
  // Bridge: allow the custom editor to report results back to the Test Explorer
  context.subscriptions.push(
    vscode.commands.registerCommand(
      'catala.testcase.reportResult',
      async (file: vscode.Uri, scope: string, results: TestRunResults) => {
        await updateTestScopes();
        // Find the test item for this file/scope
        let item: vscode.TestItem | undefined = lookupTestItem(
          ctrl,
          file,
          scope
        );
        if (!item) return;

        // Create a synthetic run to update status
        const req = new vscode.TestRunRequest([item]);
        const tr = ctrl.createTestRun(req);
        applyResultsToTestItem(tr, item, file, results);
        tr.end();
      }
    )
  );
}
