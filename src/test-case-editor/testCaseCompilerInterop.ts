import { spawn, type SpawnSyncReturns } from 'child_process';
import type {
  ReadFailure,
  ScopeDefList,
  TestGenerateResults,
  TestInputs,
} from '../generated/catala_types';
import {
  readReadFailure,
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
import {
  decideOpen,
  type ReadAnswer,
  type RebuildAnswer,
} from './openDecision';
import * as fs from 'fs';
import * as path from 'path';
import { window } from 'vscode';
import { catalaPath, clerkPath, getCwd, shellArg } from '../shared/util_client';

type ExecOptions = { input?: string; cwd?: string };
type ExecResult =
  | { ok: true; output: string }
  | { ok: false; output: string; stderr: string };

async function spawnBinary(
  bin: string,
  args: string[],
  opts: ExecOptions = {}
): Promise<ExecResult> {
  logger.log(`Running ${bin} ${args.join(' ')}`);
  const useShell = process.platform === 'win32';
  let th: Promise<ExecResult> = new Promise((resolve) => {
    try {
      const proc = spawn(bin, useShell ? args.map(shellArg) : args, {
        shell: useShell,
        // A resolved clerk reaches the plugin; a bare name would only shadow
        // the plugin's own lookup beside catala.
        env: {
          ...process.env,
          ...(path.isAbsolute(clerkPath) && { CATALA_CLERK_PATH: clerkPath }),
        },
        ...opts,
      });
      proc.stdout.setEncoding('utf-8');
      proc.stderr.setEncoding('utf-8');
      // Closed even when empty: `-` reads stdin to the end.
      if (opts.input !== undefined) {
        proc.stdin.setDefaultEncoding('utf-8');
        proc.stdin.write(opts.input);
      }
      proc.stdin.end();
      let output = '';
      proc.stdout.on('data', (data) => {
        output += data;
      });
      let stderr = '';
      proc.stderr.on('data', (data) => {
        stderr += data;
      });
      proc.on('error', (err) => {
        resolve({
          ok: false,
          output: output,
          stderr:
            stderr != ''
              ? stderr
              : err instanceof Error
                ? err.message
                : String(err),
        });
      });
      proc.on('close', (code: number | null) => {
        resolve({
          ok: code == 0,
          output: output,
          stderr: stderr,
        });
      });
    } catch (error) {
      const { stdout, stderr } = error as SpawnSyncReturns<Buffer | string>;
      resolve({
        ok: false,
        output: stdout ? stdout.toString() : '',
        stderr: stderr
          ? stderr.toString()
          : error instanceof Error
            ? error.message
            : String(error),
      });
    }
  });
  return th;
}

/** All the reasoning is in OCaml; this only asks and parses. */
export async function recoverBrokenTest(
  bufferPath: string,
  content: string,
  scope?: string
): Promise<RebuildAnswer> {
  const cwd = getCwd(bufferPath);
  const result = await spawnBinary(
    catalaPath,
    [
      'testcase',
      'rebuild',
      ...(scope ? ['--scope', scope] : []),
      '--buffer-path',
      bufferPath,
      '-',
    ],
    { input: content, ...(cwd && { cwd }) }
  );
  if (!result.ok) {
    logger.log(`rebuild could not recover: ${result.stderr}`);
    return result.stderr.trim() === ''
      ? { kind: 'silent' }
      : { kind: 'refused', message: result.stderr };
  }
  try {
    const view = readRecovery(JSON.parse(result.output));
    return view.tests.length === 0
      ? { kind: 'silent' }
      : { kind: 'recovered', view };
  } catch (error) {
    logger.log(`rebuild returned unreadable JSON: ${String(error)}`);
    return { kind: 'silent' };
  }
}

/** Rebuild against a scope the tester chose; the refusal's text on failure. */
export async function retargetBrokenTest(
  bufferPath: string,
  content: string,
  scope: string
): Promise<Extract<ParseResults, { kind: 'BrokenTest' }> | string> {
  const results = await recoverBrokenTest(bufferPath, content, scope);
  switch (results.kind) {
    case 'recovered':
      return { kind: 'BrokenTest', value: results.view };
    case 'refused':
      return results.message;
    case 'silent':
      return `Could not rebuild against ${scope}.`;
  }
}

/** A run that did not happen: raised to the window like the ordinary path. */
function runFailed(message: string): TestRunResults {
  logger.log(`rebuilt test could not be run: ${message}`);
  window.showErrorMessage(message);
  return { kind: 'Error', value: message };
}

/** From stdin: running must not imply saving the working copy. */
export async function runRebuiltTest(
  tests: TestList,
  testingScope: string,
  lang: string,
  bufferPath: string
): Promise<TestRunResults> {
  const cwd = getCwd(bufferPath);
  // A failure to run, not an exception: thrown, it escapes the message handler.
  let source: string;
  try {
    source = await atdToCatala(tests, lang);
  } catch (error) {
    return runFailed(String(error));
  }
  const result = await spawnBinary(
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

export async function parseTestFile(
  content: string,
  lang: string,
  bufferPath: string
): Promise<ParseResults> {
  const cwd = getCwd(bufferPath);
  const execResult = await spawnBinary(
    catalaPath,
    ['testcase', 'read', '-l', lang, '--buffer-path', bufferPath, '-'],
    { input: content, ...(cwd && { cwd }) }
  );
  return decideOpen({
    read: readAnswer(execResult),
    rebuild: () => recoverBrokenTest(bufferPath, content),
    hasDraft: (view) =>
      fs.existsSync(path.join(path.dirname(bufferPath), view.working_copy)),
    blank: content.trim() === '',
    ranFrom: cwd,
  });
}

function readAnswer(result: ExecResult): ReadAnswer {
  if (!result.ok) {
    let where: ReadFailure | undefined;
    try {
      where = readReadFailure(JSON.parse(result.output));
    } catch {
      where = undefined;
    }
    return { kind: 'failed', where, message: result.stderr };
  }
  let parsed: unknown;
  try {
    parsed = JSON.parse(result.output);
  } catch (error) {
    logger.log(`JSON parse error in parseTestFile: ${error}`);
    return {
      kind: 'unreadable',
      message: `JSON parse error: ${String(error)}`,
    };
  }
  try {
    return { kind: 'read', tests: readTestList(parsed) };
  } catch (error) {
    logger.log(`ATD read error in parseTestFile: ${error}`);
    return {
      kind: 'unreadable',
      message: `Schema error (catala LSP / extension version mismatch?): ${String(error)}`,
    };
  }
}

export async function atdToCatala(
  tests: TestList,
  lang: string
): Promise<string> {
  const result = await spawnBinary(
    catalaPath,
    ['testcase', 'write', '-l', lang],
    {
      input: JSON.stringify(writeTestList(tests)),
    }
  );
  if (!result.ok) {
    logger.log(`Error in atdToCatala: ${result.stderr}`);
    throw new Error(result.stderr);
  }
  return result.output;
}

export async function runTestScope(
  filename: string,
  testScope: string,
  inputs?: TestInputs
): Promise<TestRunResults> {
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
  // Runtime plugins are prepared by the testcase backend itself
  // (`prepare_runtime_plugins`); no clerk invocation is needed here.
  const cwd = getCwd(filename);
  // Here we *do* want to fail on asserts, as we catch failures through
  // the `register_lsp_error_notifier` hook.
  const execResult = await spawnBinary(catalaPath, args, {
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

export async function getAvailableScopes(
  filename: string
): Promise<ScopeDefList> {
  const execResult = await spawnBinary(catalaPath, [
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

export async function generate(
  scope: string,
  filename: string,
  default_values?: boolean,
  force_module?: boolean
): Promise<TestGenerateResults> {
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
  const execResult = await spawnBinary(catalaPath, args, {
    ...(cwd && { cwd }),
  });
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

export async function serializeInputs(
  inputs: TestInputs
): Promise<{ kind: 'Ok'; json: JSON } | { kind: 'Error'; message: string }> {
  // JSON over stdin (--input=-): the Windows shell mangles it as an inline arg.
  const args = ['testcase', 'serialize-inputs', '--input=-'];
  const execResult = await spawnBinary(catalaPath, args, {
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
