import { useEffect, useState, type ReactElement, useCallback } from 'react';
import {
  type ParseResults,
  type Test,
  type TestList,
  type TestRunResults,
  readDownMessage,
  writeUpMessage,
} from './generated/test_case';
import TestEditor from './TestEditor';
import { assertUnreachable } from './util';
import type { WebviewApi } from 'vscode-webview';

// Note:
//
// Test files are (for now) represented as a list of tests (instance
// of `TestList`) -- it's enough for now but will probably require
// a richer representation later to include markdown prose
// between tests?
//

type UIState =
  | { state: 'initializing' }
  | { state: 'error'; message: string }
  | { state: 'success'; tests: TestList };

type TestRunState = {
  [scope: string]: {
    status: 'running' | 'success' | 'error';
    results?: TestRunResults;
  };
};

type Props = { contents: UIState; vscode: WebviewApi<unknown> };

/** Editor for a collection of tests in a single file */
export default function TestFileEditor({
  contents,
  vscode,
}: Props): ReactElement {
  const [state, setState] = useState(contents);
  const [testRunState, setTestRunState] = useState<TestRunState>({});

  const onTestChange = useCallback(
    (newValue: Test): void => {
      if (state.state === 'success') {
        const idx = state.tests.findIndex(
          (tst) => tst.testing_scope === newValue.testing_scope
        );
        console.log(`test changed at index ${idx}`);
        const newTestState = [...state.tests];
        newTestState[idx] = newValue; //we can do away with this when array.with() becomes widely available
        console.log('old test state');
        console.log(state.tests);
        console.log('new test state');
        console.log(newTestState);

        vscode.postMessage(
          writeUpMessage({
            kind: 'Edit',
            value: newTestState,
          })
        );
      }
    },
    [state, vscode]
  );

  const onTestDelete = useCallback(
    (testScope: string): void => {
      if (state.state === 'success') {
        const newTestState = state.tests.filter(
          (test) => test.testing_scope !== testScope
        );
        console.log('Deleting test:', testScope);
        console.log('New test state:', newTestState);

        vscode.postMessage(
          writeUpMessage({
            kind: 'Edit',
            value: newTestState,
          })
        );
      }
    },
    [state, vscode]
  );

  const onTestRun = useCallback(
    (testScope: string): void => {
      setTestRunState((prev) => ({
        ...prev,
        [testScope]: { status: 'running' },
      }));
      vscode.postMessage(
        writeUpMessage({
          kind: 'TestRunRequest',
          value: {
            scope: testScope,
          },
        })
      );
    },
    [vscode]
  );

  useEffect(() => {
    const handleMessage = (event: MessageEvent): void => {
      const message = readDownMessage(event.data);
      switch (message.kind) {
        case 'Update':
          setState(parseResultsToUiState(message.value));
          break;
        case 'TestRunResults': {
          const results = message.value;
          setTestRunState((prev) => {
            const updatedScope = Object.keys(prev).find(
              (scope) => prev[scope].status === 'running'
            );
            if (updatedScope) {
              return {
                ...prev,
                [updatedScope]: {
                  status: results.kind === 'Ok' ? 'success' : 'error',
                  results,
                },
              };
            }
            return prev;
          });
          break;
        }
        default:
          assertUnreachable(message);
      }
    };

    window.addEventListener('message', handleMessage);

    // Cleanup function to remove event listener
    return (): void => {
      window.removeEventListener('message', handleMessage);
    };
  }, []);

  switch (state.state) {
    case 'error': {
      return (
        <div role="alert" className="test-editor-error">
          <h2>Oops! This should not have happened...</h2>
          <pre className="test-editor-error-message">{state.message}</pre>
          <button
            className="test-editor-open-text"
            onClick={() =>
              vscode.postMessage(writeUpMessage({ kind: 'OpenInTextEditor' }))
            }
          >
            <span className="codicon codicon-edit"></span>
            Open in Text Editor
          </button>
        </div>
      );
    }
    case 'initializing':
      return <strong>Initializing...</strong>;
    case 'success': {
      if (state.tests.length === 0) {
        return (
          <div className="test-editor-empty">
            <p className="test-editor-empty-message">
              No test cases found. Would you like to create your first test?
            </p>
            <button className="test-editor-run test-editor-add">
              <span className="codicon codicon-add"></span>
              Add a test
            </button>
          </div>
        );
      }
      return (
        <>
          {state.tests.map((test) => (
            <TestEditor
              test={test}
              key={test.testing_scope}
              onTestChange={onTestChange}
              onTestDelete={onTestDelete}
              onTestRun={onTestRun}
              runState={testRunState[test.testing_scope]}
            />
          ))}
        </>
      );
    }
    default:
      assertUnreachable(state);
  }
}

function parseResultsToUiState(tests: ParseResults): UIState {
  // XXX we could refactor type UIState for consistency
  // with ATD-produced types ?
  switch (tests.kind) {
    case 'Error':
      return { state: 'error', message: tests.value };
    case 'Results':
      return { state: 'success', tests: tests.value };
    default:
      assertUnreachable(tests);
  }
}
