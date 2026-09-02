import { execFileSync, type SpawnSyncReturns } from 'child_process';
import type {
  ScopeDefList,
  TestGenerateResults,
  TestInputs,
} from '../generated/catala_types';
import {
  readRecovery,
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
import { Uri, window, workspace } from 'vscode';
import path from 'path';
import { clerkPath, catalaPath, shellArg } from '../shared/util_client';

function getCwd(bufferPath: string): string | undefined {
  return workspace.getWorkspaceFolder(Uri.file(bufferPath))?.uri?.fsPath;
}

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
    return {
      ok: true,
      output: execFileSync(bin, useShell ? args.map(shellArg) : args, {
        encoding: 'utf8',
        shell: useShell,
        ...opts,
      }),
    };
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

/**
 * Recover a test that no longer fits its scope. All the reasoning is in OCaml;
 * this only asks and parses. Returns the rebuild's own refusal as a parse
 * error, or null when there is nothing to show.
 */
function recoverBrokenTest(
  bufferPath: string,
  scope?: string
): ParseResults | null {
  const cwd = getCwd(bufferPath);
  const result = execBinary(
    catalaPath,
    ['testcase', 'rebuild', ...(scope ? ['--scope', scope] : []), bufferPath],
    { ...(cwd && { cwd }) }
  );
  if (!result.ok) {
    logger.log(`rebuild could not recover: ${result.stderr}`);
    // The refusal (e.g. mixed ownership) is more specific than the compiler's
    // first error.
    return result.stderr.trim() === ''
      ? null
      : { kind: 'ParseError', value: result.stderr };
  }
  try {
    const view = readRecovery(JSON.parse(result.output));
    if (view.original.length === 0) return null;
    return { kind: 'BrokenTest', value: view };
  } catch (error) {
    logger.log(`rebuild returned unreadable JSON: ${String(error)}`);
    return null;
  }
}

/** Rebuild against a scope the tester chose; the refusal's text on failure. */
export function retargetBrokenTest(
  bufferPath: string,
  scope: string
): Extract<ParseResults, { kind: 'BrokenTest' }> | string {
  const results = recoverBrokenTest(bufferPath, scope);
  if (results === null) return `Could not rebuild against ${scope}.`;
  if (results.kind !== 'BrokenTest') {
    return results.kind === 'ParseError'
      ? results.value
      : `Could not rebuild against ${scope}.`;
  }
  return results;
}

/** A run that did not happen: raised to the window like the ordinary path. */
function runFailed(message: string): TestRunResults {
  logger.log(`rebuilt test could not be run: ${message}`);
  window.showErrorMessage(message);
  return { kind: 'Error', value: message };
}

/**
 * Run a rebuilt test from stdin: the file on disk is the broken original, and
 * running must not imply saving the working copy.
 */
export function runRebuiltTest(
  tests: TestList,
  testingScope: string,
  lang: string,
  bufferPath: string
): TestRunResults {
  const cwd = getCwd(bufferPath);
  // A failure to run, not an exception: thrown, it escapes the message handler.
  let source: string;
  try {
    source = atdToCatala(tests, lang);
  } catch (error) {
    return runFailed(String(error));
  }
  const result = execBinary(
    catalaPath,
    // --buffer-path locates the project for a test fed on stdin.
    [
      'testcase',
      'run',
      '-l',
      lang,
      '-s',
      testingScope,
      '--buffer-path',
      bufferPath,
      '-',
    ],
    { input: source, ...(cwd && { cwd }) }
  );
  // Only empty output means the test could not run.
  const output = result.ok ? result.output : '';
  if (output.trim() === '') {
    return runFailed(result.ok ? 'The test could not be run.' : result.stderr);
  }
  try {
    const {
      test: { test_outputs },
      assert_failures,
      diffs,
    } = readTestRun(JSON.parse(output));
    return { kind: 'Ok', value: { test_outputs, assert_failures, diffs } };
  } catch (error) {
    return runFailed(String(error));
  }
}

export function parseTestFile(
  content: string,
  lang: string,
  bufferPath: string
): ParseResults {
  const cwd = getCwd(bufferPath);
  const execResult = execBinary(
    catalaPath,
    ['testcase', 'read', '-l', lang, '--buffer-path', bufferPath, '-'],
    { input: content, ...(cwd && { cwd }) }
  );
  if (!execResult.ok) {
    // Usually the scope moved underneath the test.
    return (
      recoverBrokenTest(bufferPath) ?? {
        kind: 'ParseError',
        value: execResult.stderr,
      }
    );
  }
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

export function runTestScope(
  filename: string,
  testScope: string,
  inputs?: TestInputs
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
  const args = [
    'testcase',
    'run',
    '--scope',
    testScope,
    filename,
    ...inputArgs,
  ];
  const cwd = getCwd(filename);
  if (cwd) {
    const relFilename = path.relative(cwd, filename);
    //compile dependencies (hack), do not fail on asserts
    const clerkResult = execBinary(
      clerkPath,
      ['run', '-c--no-fail-on-assert', relFilename],
      { cwd }
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
    } = readTestRun(parsed);
    return {
      kind: 'Ok',
      value: {
        // TODO remove type TestRunOutput?
        test_outputs,
        assert_failures,
        diffs,
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
