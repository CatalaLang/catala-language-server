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
  // Run a single test, by its `test_id`
  onRun: (testId: string) => void;
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
            if (test.last_run?.success === false) {
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
          const { entry, scope_success, test_id: runId, order } = message.value;
          setTests((oldTests) => {
            if (oldTests === undefined) {
              return oldTests;
            }
            const failures: TestMacro[] = [];
            const others: TestMacro[] = [];
            for (const test of oldTests) {
              let updatedTest: TestMacro = test;
              if (test.test_id == runId) {
                // Spread rather than rebuilt field by field: everything the
                // run does not report is intrinsic to the test and must
                // survive it — `relative_filename`, in particular, or the
                // list view would fall back to the absolute path as soon as
                // a test has run.
                updatedTest = {
                  ...test,
                  test: entry,
                  last_run: scope_success,
                  loading: false,
                };
              }
              // Explicitly `false`: a test that was never run has no
              // `last_run` and does not belong with the failures.
              if (order && updatedTest.last_run?.success === false) {
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

  const onRun = (testId: string): void => {
    if (!tests) {
      return;
    }
    setTests((oldTests) =>
      oldTests?.map((test) =>
        test.test_id === testId ? { ...test, loading: true } : test
      )
    );
    vscode.postMessage(
      writeTestsUpMessage({ kind: 'SpecificTestRequest', value: [testId] })
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
        let ids = selection.map(({ test_id }) => test_id);
        setTests((oldTests) => {
          return oldTests?.map((test) => {
            let loading = ids.includes(test.test_id);
            return loading ? { ...test, loading: true } : test;
          });
        });
        vscode.postMessage(
          writeTestsUpMessage({
            kind: 'SpecificTestRequest',
            value: ids,
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
