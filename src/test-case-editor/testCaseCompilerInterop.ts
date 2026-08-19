import { execFileSync, type SpawnSyncReturns } from 'child_process';
import type {
  ScopeDefList,
  ScopeTestResult,
  TestGenerateResults,
  TestInputs,
} from '../generated/catala_types';
import {
  readScopeDefList,
  readTestList,
  readTestRun,
  writeTestInputs,
  writeTestList,
  type ParseResults,
  type TestList,
  type TestRunResults,
} from '../generated/catala_types';
import { logger } from '../extension/logger';
import { window } from 'vscode';
import path from 'path';
import { clerkPath, catalaPath, getCwd, shellArg } from '../shared/util_client';

type ExecOptions = { input?: string; cwd?: string };
type ExecResult = { ok: true; output: string } | { ok: false; stderr: string };

function execBinary(
  bin: string,
  args: string[],
  opts: ExecOptions = {}
): ExecResult {
  logger.log(`Running ${bin} ${args.join(' ')}`);
  try {
    const useShell = process.platform === 'win32';
    const output = execFileSync(bin, useShell ? args.map(shellArg) : args, {
      encoding: 'utf8',
      shell: useShell,
      ...opts,
    });
    return { ok: true, output };
  } catch (error) {
    const stderr = (error as SpawnSyncReturns<Buffer | string>).stderr;
    return {
      ok: false,
      stderr: stderr
        ? stderr.toString()
        : error instanceof Error
          ? error.message
          : String(error),
    };
  }
}

export function parseTestFile(
  content: string,
  bufferPath: string,
  lang?: string,
  scope?: string
): ParseResults {
  const cwd = getCwd(bufferPath);
  const execResult = execBinary(
    catalaPath,
    [
      'testcase',
      'read',
      ...(lang ? ['-l', lang] : []),
      '--buffer-path',
      bufferPath,
      ...(scope ? ['--scope', scope] : []),
      '-',
    ],
    { input: content, ...(cwd && { cwd }) }
  );
  if (!execResult.ok) return { kind: 'ParseError', value: execResult.stderr };
  let parsed: unknown;
  try {
    parsed = JSON.parse(execResult.output);
  } catch (error) {
    logger.log(`JSON parse error in parseTestFile: ${error}`);
    return { kind: 'ParseError', value: `JSON parse error: ${String(error)}` };
  }
  let testList: TestList;
  try {
    testList = readTestList(parsed);
  } catch (error) {
    logger.log(`ATD read error in parseTestFile: ${error}`);
    return {
      kind: 'ParseError',
      value: `Schema error (catala LSP / extension version mismatch?): ${String(error)}`,
    };
  }
  if (content.trim() !== '' && testList.length === 0) {
    return { kind: 'EmptyTestListMismatch' };
  }
  return { kind: 'Results', value: testList };
}

export function atdToCatala(tests: TestList, lang: string): string {
  const result = execBinary(catalaPath, ['testcase', 'write', '-l', lang], {
    input: JSON.stringify(writeTestList(tests)),
  });
  if (!result.ok) {
    logger.log(`Error in atdToCatala: ${result.stderr}`);
    throw new Error(result.stderr);
  }
  return result.output;
}

// Outcome of running a scope test: either a successful `ScopeTestResult`,
// or a failure carrying an error message.
export type ScopeRunResult =
  | { kind: 'Success'; value: ScopeTestResult }
  | { kind: 'Failed'; value: string };

export function runTestScope(
  filename: string,
  testScope: string,
  inputs?: TestInputs,
  /**
   * Absolute path of the JSON file the trace should be written to. When
   * undefined the run is not instrumented at all: tracing changes the compiled
   * AST and costs interpretation time.
   */
  traceFile?: string
): TestRunResults {
  /*
   * Notes:
   * - security: fileName should be provided by the editor, so it should be
   * trustworthy: check?
   * - Users should probably have a command that interrupts a running test
   * - Should tests have (configurable) timeouts? (when running interactively)
   * (note that not all these questions are related to the `runTestScope` function,
   * these could be handled externally as well)
   */
  // Pass the input over stdin (`--input=-`), not inline: large generated inputs
  // (tens of KB) overflow the Windows command-line limit and fail with ENAMETOOLONG.
  const inputJson = inputs
    ? JSON.stringify(writeTestInputs(inputs))
    : undefined;
  const inputArgs = inputs ? ['--input=-'] : [];
  // Only instrument the run when the test has expected variables to check:
  // tracing changes the compiled AST and costs interpretation time.
  // NB: bare `--trace` defaults to writing the trace on stdout, where the JSON
  // result is read from; the plugin redirects it away, which is what makes this
  // safe.
  // Dependencies are built in a separate directory so that the instrumented
  // artifacts do not evict the plain ones from the main build dir.
  const clerkTraceArgs = traceFile
    ? [
        '--quiet',
        '--trace',
        traceFile,
        '--build-dir',
        '_build/_trace',
        '--ninja-output-file',
        '_build/_trace/clerk.ninja',
      ]
    : [];
  // The trace is produced by the clerk run below, not here: `testcase run` wraps
  // every evaluation in a dummy scope call, so the trace it could emit carries
  // only "<function>" as its root value. It is handed the file instead, so that
  // the expected variables are checked against the very trace the editor shows.
  const catalaTraceArgs = traceFile ? [`--check-trace=${traceFile}`] : [];
  const args = [
    'testcase',
    'run',
    '--scope',
    testScope,
    filename,
    ...inputArgs,
    ...catalaTraceArgs,
  ];
  const cwd = getCwd(filename);
  if (cwd) {
    const relFilename = path.relative(cwd, filename);
    // Two jobs at once: compile the dependencies the run below needs, and — when
    // a trace was asked for — produce it. `-c--no-fail-on-assert` matters in
    // both cases: a test whose expectations do not match must still get its
    // dependencies built and its trace written.
    const clerkResult = execBinary(
      clerkPath,
      [
        'run',
        ...clerkTraceArgs,
        '-c--no-fail-on-assert',
        relFilename,
        '--scope',
        testScope,
      ],
      {
        cwd,
      }
    );
    if (!clerkResult.ok) {
      window.showErrorMessage(clerkResult.stderr);
      return { kind: 'Error', value: clerkResult.stderr };
    }
  }
  // Here we *do* want to fail on asserts, as we catch failures through
  // the `register_lsp_error_notifier` hook.
  const execResult = execBinary(catalaPath, args, {
    ...(cwd && { cwd }),
    ...(inputJson !== undefined && { input: inputJson }),
  });
  if (!execResult.ok) {
    window.showErrorMessage(execResult.stderr);
    return { kind: 'Error', value: execResult.stderr };
  }
  let parsed: unknown;
  try {
    parsed = JSON.parse(execResult.output);
  } catch (error) {
    logger.log(`JSON parse error in runTestScope: ${error}`);
    const msg = `JSON parse error: ${String(error)}`;
    window.showErrorMessage(msg);
    return { kind: 'Error', value: msg };
  }
  try {
    const {
      test: { test_outputs },
      assert_failures,
      diffs,
      variable_failures,
    } = readTestRun(parsed);
    return {
      kind: 'Ok',
      value: {
        // TODO remove type TestRunOutput?
        test_outputs,
        assert_failures,
        diffs,
        variable_failures,
      },
    };
  } catch (error) {
    logger.log(`ATD read error in runTestScope: ${error}`);
    const msg = `Schema error (catala LSP / extension version mismatch?): ${String(error)}`;
    window.showErrorMessage(msg);
    return { kind: 'Error', value: msg };
  }
}

export function getAvailableScopes(filename: string): ScopeDefList {
  const execResult = execBinary(catalaPath, [
    'testcase',
    'list-scopes',
    filename,
  ]);
  if (!execResult.ok) {
    logger.log(`Execution error in getAvailableScopes: ${execResult.stderr}`);
    return [];
  }
  let parsed: unknown;
  try {
    parsed = JSON.parse(execResult.output);
  } catch (error) {
    logger.log(`JSON parse error in getAvailableScopes: ${error}`);
    return [];
  }
  try {
    return readScopeDefList(parsed);
  } catch (error) {
    logger.log(
      `ATD read error in getAvailableScopes (catala LSP / extension version mismatch?): ${error}`
    );
    return [];
  }
}

export function generate(
  scope: string,
  filename: string,
  default_values?: boolean,
  force_module?: boolean
): TestGenerateResults {
  const args = [
    'testcase',
    'generate',
    '--scope',
    scope,
    filename,
    ...(default_values ? ['--default-values'] : []),
    ...(force_module ? ['--enforce-module'] : []),
  ];
  const cwd = getCwd(filename);
  const execResult = execBinary(catalaPath, args, { ...(cwd && { cwd }) });
  if (!execResult.ok) return { kind: 'Error', value: execResult.stderr };
  let parsed: unknown;
  try {
    parsed = JSON.parse(execResult.output);
  } catch (error) {
    logger.log(`JSON parse error in generate: ${error}`);
    return { kind: 'Error', value: `JSON parse error: ${String(error)}` };
  }
  try {
    return { kind: 'Results', value: readTestList(parsed) };
  } catch (error) {
    logger.log(`ATD read error in generate: ${error}`);
    return {
      kind: 'Error',
      value: `Schema error (catala LSP / extension version mismatch?): ${String(error)}`,
    };
  }
}

export function serializeInputs(
  inputs: TestInputs
): { kind: 'Ok'; json: JSON } | { kind: 'Error'; message: string } {
  // JSON over stdin (--input=-): the Windows shell mangles it as an inline arg.
  const args = ['testcase', 'serialize-inputs', '--input=-'];
  const execResult = execBinary(catalaPath, args, {
    input: JSON.stringify(writeTestInputs(inputs)),
  });
  if (!execResult.ok) {
    window.showErrorMessage(execResult.stderr);
    return { kind: 'Error', message: execResult.stderr };
  }
  try {
    return { kind: 'Ok', json: JSON.parse(execResult.output) };
  } catch (error) {
    logger.log(`JSON parse error in serializeInputs: ${error}`);
    const msg = `JSON parse error: ${String(error)}`;
    window.showErrorMessage(msg);
    return { kind: 'Error', message: msg };
  }
}
