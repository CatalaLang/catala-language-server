import * as vscode from 'vscode';
import { execFileSync, type SpawnSyncReturns } from 'child_process';
import type {
  ScopeDefList,
  TestGenerateResults,
} from './generated/catala_types';
import {
  readScopeDefList,
  readTestList,
  readTestRun,
  writeTestList,
  type ParseResults,
  type TestList,
  type TestRunResults,
} from './generated/catala_types';
import { logger } from './logger';
import { Uri, window, workspace } from 'vscode';
import path from 'path';
import fs from 'fs';

function getCwd(bufferPath: string): string | undefined {
  return workspace.getWorkspaceFolder(Uri.parse(bufferPath))?.uri?.fsPath;
}

function pathFromConfig(confId: string, defaultCmd: string): string {
  const confPath = vscode.workspace
    .getConfiguration('catala')
    .get<string>(confId);
  if (confPath === undefined || confPath === null || confPath.trim() === '')
    return defaultCmd;
  if (!fs.existsSync(confPath)) {
    vscode.window.showWarningMessage(
      `Could not find executable for ${confId} at ${confPath}, falling back to default`
    );
    return defaultCmd;
  }
  return confPath;
}

const catalaPath: string = pathFromConfig('catalaPath', 'catala');
logger.log(`catala command: ${catalaPath}`);

const clerkPath: string = pathFromConfig('clerkPath', 'clerk');
logger.log(`clerk command: ${clerkPath}`);

export function parseTestFile(
  content: string,
  lang: string,
  bufferPath: string
): ParseResults {
  const cwd = getCwd(bufferPath);

  // TODO we could revisit this to make the parsing async
  try {
    const results = execFileSync(
      catalaPath,
      ['testcase', 'read', '-l', lang, '--buffer-path', bufferPath, '-'],
      { input: content, ...(cwd && { cwd }) }
    );
    const testList = readTestList(JSON.parse(results.toString()));
    if (content.trim() !== '' && testList.length == 0) {
      return {
        kind: 'EmptyTestListMismatch',
      };
    }
    return {
      kind: 'Results',
      value: testList,
    };
  } catch (error) {
    return {
      kind: 'ParseError',
      value: String((error as SpawnSyncReturns<string | Buffer>).stderr),
    };
  }
}

export function atdToCatala(tests: TestList, lang: string): string {
  //XXX this probably needs better error handling
  try {
    const results = execFileSync(
      catalaPath,
      ['testcase', 'write', '-l', lang],
      {
        input: JSON.stringify(writeTestList(tests)),
      }
    );
    return results.toString();
  } catch (error) {
    logger.log(`Error in atdToCatala: ${error}`);
    throw error;
  }
}

export function runTestScope(
  filename: string,
  testScope: string
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

  const args = ['testcase', 'run', '--scope', testScope, filename];
  logger.log(`Exec: ${catalaPath} ${args.join(' ')}`);
  try {
    const cwd = getCwd(filename);
    if (cwd) {
      const relFilename = path.relative(cwd, filename);
      //compile dependencies (hack), do not fail on asserts
      execFileSync(clerkPath, ['run', '-c--no-fail-on-assert', relFilename], {
        cwd,
      });
    }
    // Here we *do* want to fail on asserts, as we catch failures through
    // the `register_lsp_error_notifier` hook.
    const result = execFileSync(catalaPath, args, { ...(cwd && { cwd }) });
    const testRun = readTestRun(JSON.parse(result.toString()));
    logger.log(`diffs: ${JSON.stringify(testRun.diffs)}`);
    return {
      kind: 'Ok',
      value: {
        // TODO remove type TestRunOutput?
        test_outputs: testRun.test.test_outputs,
        assert_failures: testRun.assert_failures,
        diffs: testRun.diffs,
      },
    };
  } catch (error) {
    const errorMsg = String(
      (error as SpawnSyncReturns<string | Buffer>).stderr
    );
    window.showErrorMessage(errorMsg);
    return {
      kind: 'Error',
      value: errorMsg,
    };
  }
}

export function getAvailableScopes(filename: string): ScopeDefList {
  try {
    const results = execFileSync(catalaPath, [
      'testcase',
      'list-scopes',
      filename,
    ]);
    return readScopeDefList(JSON.parse(results.toString()));
  } catch (error) {
    logger.log(`Error getting available scopes: ${error}`);
    return [];
  }
}

export function generate(scope: string, filename: string): TestGenerateResults {
  const cmd = catalaPath;
  const args = ['testcase', 'generate', '--scope', scope, filename];
  logger.log(`${cmd} ${args}`);
  try {
    const results = execFileSync(cmd, args);
    const test = readTestList(JSON.parse(results.toString()));
    return {
      kind: 'Results',
      value: test,
    };
  } catch (error) {
    return {
      kind: 'Error',
      value: String((error as SpawnSyncReturns<string | Buffer>).stderr),
    };
  }
}
