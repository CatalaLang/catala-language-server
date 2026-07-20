import { useEffect, useState } from 'react';

import { type WebviewApi } from 'vscode-webview';

import {
  readTestsDownMessage,
  writeTestsUpMessage,
} from '../generated/catala_types';
import { setVsCodeApi } from '../shared/webviewApi';
import type { TestMacro } from './types';

export type Tests = {
  // `undefined` until the host has sent the list of tests
  tests: TestMacro[] | undefined;
  // A reload of the list of tests was asked for and has not come back yet
  reload: boolean;
  // Run a single test, by index
  onRun: (id: number) => void;
  // Run the given tests, or every test with 'all'
  runTests: (selection: 'all' | TestMacro[] | undefined) => void;
  // Ask the host for the list of tests again
  refresh: () => void;
};

/**
 * The list of tests, kept in sync with the extension host: it receives the
 * list and the run results, and sends the run and reload requests.
 */
export function useTests(vscode: WebviewApi<unknown>): Tests {
  const [tests, setTests] = useState<TestMacro[] | undefined>(undefined);
  const [reload, setReload] = useState<boolean>(false);

  useEffect(() => {
    setVsCodeApi(vscode);
  }, [vscode]);

  useEffect(() => {
    const handleMessage = (event: MessageEvent): void => {
      const message = readTestsDownMessage(event.data);
      switch (message.kind) {
        case 'AllTests': {
          setReload(false);
          let tests = message.value;
          let tsTests: TestMacro[] = [];
          let failures: TestMacro[] = [];
          for (let index = 0; index < tests.length; index++) {
            const test = tests[index];
            if (test.success !== undefined && !test.success) {
              failures.push(test);
            } else {
              tsTests.push(test);
            }
          }
          failures.push(...tsTests);
          setTests(failures);
          break;
        }
        case 'TestScopeResult': {
          const {
            entry,
            scope_success,
            index: runIndex,
            order,
          } = message.value;
          setTests((oldTests) => {
            if (oldTests === undefined) {
              return oldTests;
            }
            const failures: TestMacro[] = [];
            const others: TestMacro[] = [];
            for (const test of oldTests) {
              let updatedTest: TestMacro = test;
              if (test.index == runIndex) {
                // Spread rather than rebuilt field by field: everything the
                // run does not report is intrinsic to the test and must
                // survive it — `relative_filename`, in particular, or the
                // list view would fall back to the absolute path as soon as
                // a test has run.
                updatedTest = {
                  ...test,
                  test: entry,
                  success: scope_success.success,
                  date: scope_success.date,
                  loading: false,
                };
              }
              // Explicitly `false`: a test that was never run has an undefined
              // `success` and does not belong with the failures.
              if (order && updatedTest.success === false) {
                failures.push(updatedTest);
              } else {
                others.push(updatedTest);
              }
            }
            return [...failures, ...others];
          });
          break;
        }
        default:
          break;
      }
    };

    window.addEventListener('message', handleMessage);

    // Cleanup function to remove event listener
    return (): void => {
      window.removeEventListener('message', handleMessage);
    };
  }, []);

  const onRun = (id: number): void => {
    if (!tests) {
      return;
    }
    setTests((oldTests) =>
      oldTests?.map((test) =>
        test.index === id ? { ...test, loading: true } : test
      )
    );
    vscode.postMessage(
      writeTestsUpMessage({ kind: 'SpecificTestRequest', value: [id] })
    );
  };

  const runTests = (selection: 'all' | TestMacro[] | undefined): void => {
    if (tests) {
      if (selection === 'all') {
        setTests((oldTests) =>
          oldTests?.map((test) => {
            return { ...test, loading: true };
          })
        );
        vscode.postMessage(
          writeTestsUpMessage({
            kind: 'SpecificTestRequest',
            value: [],
          })
        );
      } else if (selection) {
        let indexes = selection.map(({ index }) => index);
        setTests((oldTests) => {
          return oldTests?.map((test) => {
            let loading = indexes.includes(test.index);
            return loading ? { ...test, loading: true } : test;
          });
        });
        vscode.postMessage(
          writeTestsUpMessage({
            kind: 'SpecificTestRequest',
            value: indexes,
          })
        );
      }
    }
  };

  const refresh = (): void => {
    setReload(true);
    vscode.postMessage(writeTestsUpMessage({ kind: 'Ready' }));
  };

  return { tests, reload, onRun, runTests, refresh };
}
