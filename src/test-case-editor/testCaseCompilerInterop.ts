import { execFileSync, type SpawnSyncReturns } from 'child_process';
import type {
  RunTestOutput,
  ScopeDefList,
  TestGenerateResults,
  TestInputs,
  TestRunResult,
} from '../generated/catala_types';
import {
  readScopeDefList,
  readTestList,
  readTestRun,
  writeTestInputs,
  writeTestList,
  type ParseResults,
  type TestList,
} from '../generated/catala_types';
import { logger } from '../extension/logger';
import { Uri, window, workspace } from 'vscode';
import path from 'path';
import { clerkPath, catalaPath } from '../shared/util_client';
import { generateTest, listEntrypoints, listScopes, readTest, runTest, writeTest, serializeInputs } from '../extension/lspRequests';
import { getClient } from '../extension';

function getCwd(bufferPath: string): string | undefined {
  return workspace.getWorkspaceFolder(Uri.parse(bufferPath))?.uri?.fsPath;
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
    return {
      ok: true,
      output: execFileSync(bin, args, { encoding: 'utf8', ...opts }),
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

export async function parseTestFile(
  content: string,
  lang: string,
  bufferPath: string
): Promise<ParseResults> {

  const testList = await readTest(lang, content, bufferPath)

  // const cwd = getCwd(bufferPath);
  // const execResult = execBinary(
  //   catalaPath,
  //   ['testcase', 'read', '-l', lang, '--buffer-path', bufferPath, '-'],
  //   { input: content, ...(cwd && { cwd }) }
  // );
  // if (!execResult.ok) return { kind: 'ParseError', value: execResult.stderr };
  // let parsed: unknown;
  // try {
  //   parsed = JSON.parse(execResult.output);
  // } catch (error) {
  //   logger.log(`JSON parse error in parseTestFile: ${error}`);
  //   return { kind: 'ParseError', value: `JSON parse error: ${String(error)}` };
  // }
  // let testList: TestList;
  // try {
  //   testList = readTestList(parsed);
  // } catch (error) {
  //   logger.log(`ATD read error in parseTestFile: ${error}`);
  //   return {
  //     kind: 'ParseError',
  //     value: `Schema error (catala LSP / extension version mismatch?): ${String(error)}`,
  //   };
  // }

  // TODO : CLEAN THIS UP

  if (testList) {
    if (content.trim() !== '' && testList.length === 0) {
      return { kind: 'EmptyTestListMismatch' };
    }
    return { kind: 'Results', value: testList };
  } else {
    return { kind: 'ParseError', value: "caca" };
  }
}

export async function atdToCatala(tests: TestList, lang: string): Promise<string> {
  const result = await writeTest(lang, tests)

  // const result = execBinary(catalaPath, ['testcase', 'write', '-l', lang], {
  //   input: JSON.stringify(writeTestList(tests)),
  // });
  if (!result) {
    logger.log(`Error in atdToCatala`);
    throw new Error();
  }
  return result;
}

export async function runTestScope(
  filename: string,
  testScope: string,
  inputs?: TestInputs
): Promise<TestRunResult> {
  /*
   * Notes:
   * - security: fileName should be provided by the editor, so it should be
   * trustworthy: check?
   * - Users should probably have a command that interrupts a running test
   * - Should tests have (configurable) timeouts? (when running interactively)
   * (note that not all these questions are related to the `runTestScope` function,
   * these could be handled externally as well)
   */
  // const inputArgs = inputs
  //   ? ['--input', JSON.stringify(writeTestInputs(inputs))]
  //   : [];

  // const args = [
  //   'testcase',
  //   'run',
  //   '--scope',
  //   testScope,
  //   filename,
  //   ...inputArgs,
  // ];
  // const cwd = getCwd(filename);
  // if (cwd) {
  //   const relFilename = path.relative(cwd, filename);
  //   //compile dependencies (hack), do not fail on asserts
  //   const clerkResult = execBinary(
  //     clerkPath,
  //     ['run', '-c--no-fail-on-assert', relFilename],
  //     { cwd }
  //   );
  //   if (!clerkResult.ok) {
  //     window.showErrorMessage(clerkResult.stderr);
  //     return { kind: 'Error', value: clerkResult.stderr };
  //   }
  // }
  // // Here we *do* want to fail on asserts, as we catch failures through
  // // the `register_lsp_error_notifier` hook.
  // const execResult = execBinary(catalaPath, args, { ...(cwd && { cwd }) });

  // TODO
  logger.log("before")
  const test_outputs: RunTestOutput | null = await runTest(filename, testScope, inputs);
  if (test_outputs){
    logger.log("good")
    return test_outputs
  }
  else {
    logger.log(":( ")
    return { kind: 'Error', value: 'LSP request \'runTest\' failed' }
  }
  // let parsed: unknown;
  // parsed = JSON.parse(result);

  // if (!execResult.ok) {
  //   window.showErrorMessage(execResult.stderr);
  //   return { kind: 'Error', value: execResult.stderr };
  // }
  // let parsed: unknown;
  // try {
  //   parsed = JSON.parse(execResult.output);
  // } catch (error) {
  //   logger.log(`JSON parse error in runTestScope: ${error}`);
  //   const msg = `JSON parse error: ${String(error)}`;
  //   window.showErrorMessage(msg);
  //   return { kind: 'Error', value: msg };
  // }
  // try {
  //   const {
  //     test: { test_outputs },
  //     assert_failures,
  //     diffs,
  //   } = readTestRun(parsed);
  //   return {
  //     kind: 'Ok',
  //     value: {
  //       // TODO remove type TestRunOutput?
  //       test_outputs,
  //       assert_failures,
  //       diffs,
  //     },
  //   };
  // } catch (error) {
  //   logger.log(`ATD read error in runTestScope: ${error}`);
  //   const msg = `Schema error (catala LSP / extension version mismatch?): ${String(error)}`;
  //   window.showErrorMessage(msg);
  //   return { kind: 'Error', value: msg };
  // }
}

export async function getAvailableScopes(filename: string): Promise<ScopeDefList> {
  const result = await listScopes(filename)
  if (result)
    return result
  else {
    logger.log('LSP request \'listScopes` failed')
    return []
  }
  // const execResult = execBinary(catalaPath, [
  //   'testcase',
  //   'list-scopes',
  //   filename,
  // ]);
  // if (!execResult.ok) {
  //   logger.log(`Execution error in getAvailableScopes: ${execResult.stderr}`);
  //   return [];
  // }
  // let parsed: unknown;
  // try {
  //   parsed = JSON.parse(execResult.output);
  // } catch (error) {
  //   logger.log(`JSON parse error in getAvailableScopes: ${error}`);
  //   return [];
  // }
  // try {
  //   return readScopeDefList(parsed);
  // } catch (error) {
  //   logger.log(
  //     `ATD read error in getAvailableScopes (catala LSP / extension version mismatch?): ${error}`
  //   );
  //   return [];
  // }
}

export async function generate(
  scope: string,
  filename: string,
  default_values?: boolean,
  force_module?: boolean
): Promise<TestGenerateResults> {

  const tests_list = await generateTest(filename, scope, default_values, force_module)
  if (tests_list)
    return { kind: 'Results', value: tests_list }
  else
    return { kind: 'Error', value: 'LSP request \'generateTest\' failed' }
  // const args = [
  //   'testcase',
  //   'generate',
  //   '--scope',
  //   scope,
  //   filename,
  //   ...(default_values ? ['--default-values'] : []),
  //   ...(force_module ? ['--enforce-module'] : []),
  // ];
  // const cwd = getCwd(filename);
  // const execResult = execBinary(catalaPath, args, { ...(cwd && { cwd }) });
  // if (!execResult.ok) return { kind: 'Error', value: execResult.stderr };
  // let parsed: unknown;
  // try {
  //   parsed = JSON.parse(execResult.output);
  // } catch (error) {
  //   logger.log(`JSON parse error in generate: ${error}`);
  //   return { kind: 'Error', value: `JSON parse error: ${String(error)}` };
  // }
  // try {
  //   return { kind: 'Results', value: readTestList(parsed) };
  // } catch (error) {
  //   logger.log(`ATD read error in generate: ${error}`);
  //   return {
  //     kind: 'Error',
  //     value: `Schema error (catala LSP / extension version mismatch?): ${String(error)}`,
  //   };
  // }
}

export async function serializeCatalaInputs(
  inputs: TestInputs
): Promise<{ kind: 'Ok'; json: JSON } | { kind: 'Error'; message: string }> {

  const result = await serializeInputs(inputs)
  if (result)
    return { kind: 'Ok', json: result }
  else {
    logger.log('LSP request \'listScopes` failed')
    return { kind: 'Error', message: 'LSP request \'serializeInputs\' failed' }
  }

  // const args = [
  //   'testcase',
  //   'serialize-inputs',
  //   '--input',
  //   JSON.stringify(writeTestInputs(inputs)),
  // ];
  // const execResult = execBinary(catalaPath, args);
  // if (!execResult.ok) {
  //   window.showErrorMessage(execResult.stderr);
  //   return { kind: 'Error', message: execResult.stderr };
  // }
  // try {
  //   return { kind: 'Ok', json: JSON.parse(execResult.output) };
  // } catch (error) {
  //   logger.log(`JSON parse error in serializeInputs: ${error}`);
  //   const msg = `JSON parse error: ${String(error)}`;
  //   window.showErrorMessage(msg);
  //   return { kind: 'Error', message: msg };
  // }
}
