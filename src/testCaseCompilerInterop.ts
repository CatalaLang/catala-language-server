import { execFileSync, type SpawnSyncReturns } from 'child_process';
import type { ScopeDefList, TestGenerateResults } from './generated/test_case';
import {
  readScopeDefList,
  readTest,
  readTestList,
  writeTestList,
  type ParseResults,
  type TestList,
  type TestRunResults,
} from './generated/test_case';
import { logger } from './logger';
import path = require('path');
import { Uri, window, workspace } from 'vscode';

export function parseTestFile(
  content: string,
  lang: string,
  bufferPath: string
): ParseResults {
  const cwd = workspace.getWorkspaceFolder(Uri.parse(bufferPath))?.uri?.fsPath;

  // TODO we could revisit this to make the parsing async
  try {
    const results = execFileSync(
      'catala',
      [
        'testcase',
        'read',
        '-l',
        lang,
        '--debug',
        '--buffer-path',
        bufferPath,
        '-',
      ],
      { input: content, ...(cwd && { cwd }) }
    );
    return {
      kind: 'Results',
      value: readTestList(JSON.parse(results.toString())),
    };
  } catch (error) {
    return {
      kind: 'Error',
      value: String((error as SpawnSyncReturns<string | Buffer>).stderr),
    };
  }
}

export function atdToCatala(tests: TestList, lang: string): string {
  //XXX this probably needs better error handling
  try {
    const results = execFileSync('catala', ['testcase', 'write', '-l', lang], {
      input: JSON.stringify(writeTestList(tests)),
    });
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
   * - when parsing / generating tests, we operate on the current text buffer
   * in the editor through `stdin`. Here, we run the actual file on disk.
   * Should we produce an error if they are not identical? (i.e. the buffer
   * is dirty)?
   * - security: fileName should be provided by the editor, so it should be
   * trustworthy: check?
   * - Users should probably have a command that interrupts a running test
   * - Should tests have (configurable) timeouts? (when running interactively)
   * (note that not all these questions are related to the `runTestScope` function,
   * these could be handled externally as well)
   */

  const cmd = 'catala';
  filename = path.isAbsolute(filename)
    ? path.relative(process.cwd(), filename)
    : filename;

  const args = ['testcase', 'run', '--scope', testScope, filename];
  logger.log(`Exec: ${cmd} ${args.join(' ')}`);
  try {
    // HACK: use 'clerk run' as a preamble (does not output test result,
    // but builds any dependencies as a side-effect which we need currently
    // for the run plugin!)
    logger.log('before clerk run');
    execFileSync('clerk', [
      'run',
      filename,
      '-s',
      testScope,
      '-I',
      './test-case-parser/examples',
    ]);
    const result = execFileSync(cmd, args);
    logger.log(result.toString());
    const test = readTest(JSON.parse(result.toString()));
    return {
      kind: 'Ok',
      value: test.test_outputs,
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
    const results = execFileSync('catala', [
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
  const cmd = 'catala';
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
