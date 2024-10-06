import { execFileSync, type SpawnSyncReturns } from 'child_process';
import type { TestGenerateResults, TestInputs } from './generated/test_case';
import {
  readTest,
  readTestList,
  writeTestList,
  type ParseResults,
  type TestList,
  type TestRunResults,
} from './generated/test_case';
import { logger } from './logger';
import path = require('path');
import { getDefaultValue } from './defaults';

export function parseTestFile(content: string, lang: string): ParseResults {
  // TODO check behavior when packaging comes into play ('dune install')
  // TODO we should also delegate includes to clerk (remove 'examples')
  // TODO we could revisit this to make the parsing async
  try {
    const results = execFileSync(
      'catala',
      [
        'testcase',
        'read',
        '-l',
        lang,
        '-I',
        './test-case-parser/examples',
        '-',
      ],
      { input: content }
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
  const cmd = 'clerk';
  filename = path.isAbsolute(filename)
    ? path.relative(process.cwd(), filename)
    : filename;
  const args = ['run', filename, '--scope', testScope];
  logger.log(`Exec: ${cmd} ${args.join(' ')}`);
  try {
    execFileSync(cmd, args);
  } catch (error) {
    return {
      kind: 'Error',
      value: String((error as SpawnSyncReturns<string | Buffer>).stderr),
    };
  }
  return { kind: 'Ok' };
}

function withDefaultInputs(outputs: TestInputs): TestInputs {
  return new Map(
    Array.from(outputs, ([name, testIo]) => {
      if (testIo.value !== undefined) {
        throw Error(`Expected an undefined value: ${name}`);
      }
      return [
        name,
        { typ: testIo.typ, value: { value: getDefaultValue(testIo.typ) } },
      ];
    })
  );
}

export function generate(
  scope: string,
  filename: string,
  fillDefaultInputs = true
): TestGenerateResults {
  const cmd = 'catala';
  const args = ['testcase', 'generate', '--scope', scope, filename];
  try {
    const results = execFileSync(cmd, args);
    let test = readTest(JSON.parse(results.toString()));
    if (fillDefaultInputs) {
      test = { ...test, test_inputs: withDefaultInputs(test.test_inputs) };
    }
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
