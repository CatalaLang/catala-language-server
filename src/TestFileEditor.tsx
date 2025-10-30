import { useEffect, useState, type ReactElement, useCallback } from 'react';
import { FormattedMessage } from 'react-intl';
import {
  type ParseResults,
  type Test,
  type TestList,
  type TestRunResults,
  type PathSegment,
  type Diff,
  readDownMessage,
  writeUpMessage,
} from './generated/test_case';
import TestEditor from './TestEditor';
import { assertUnreachable } from './util';
import { pathEquals } from './diff/highlight';
import type { WebviewApi } from 'vscode-webview';

// Note:
//
// Test files are (for now) represented as a list of tests (instance
// of `TestList`) -- it's enough for now but will probably require
// a richer representation later to include markdown prose
// between tests?
//

// Note (2025-10-29):
//
// We changed our test file editor to
// show 0 or 1 test (vs an arbitrary number of tests before) ;
// for now the compiler-side definitions are unchanged (a TestFile
// is a possibly empty list of Test) and the TestFileEditor
// component will render N tests, but we removed the "add test"
// button (only the wizard for an empty file remains) which effectively
// limits test files to 1 test. It may be worth revisiting later?

type UIState =
  | { state: 'initializing' }
  | { state: 'error'; message: string }
  | { state: 'emptyTestListMismatch' }
  | { state: 'success'; tests: TestList };

export type TestRunStatus = 'running' | 'success' | 'error' | 'cancelled';

type TestRunState = {
  [scope: string]: {
    status: TestRunStatus;
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
    (newValue: Test, mayBeBatched: boolean): void => {
      if (state.state === 'success') {
        const idx = state.tests.findIndex(
          (tst) => tst.testing_scope === newValue.testing_scope
        );
        const newTestState = [...state.tests];
        newTestState[idx] = newValue; //we can do away with this when array.with() becomes widely available

        // optimistic update
        setState({ state: 'success', tests: newTestState });

        vscode.postMessage(
          writeUpMessage({
            kind: 'GuiEdit',
            value: [newTestState, mayBeBatched],
          })
        );
      }
    },
    [state, vscode, setTestRunState]
  );

  const onTestDelete = useCallback(
    (testScope: string): void => {
      if (state.state === 'success') {
        const newTestState = state.tests.filter(
          (test) => test.testing_scope !== testScope
        );
        console.log('Deleting test:', testScope);
        console.log('New test state:', newTestState);

        // optimistic update
        setState({ state: 'success', tests: newTestState });

        vscode.postMessage(
          writeUpMessage({
            kind: 'GuiEdit',
            value: [newTestState, false],
          })
        );
      }
    },
    [state, vscode]
  );

  const _onTestRun = (resetOutputs: boolean) => {
    return (testScope: string): void => {
      setTestRunState((prev) => ({
        ...prev,
        [testScope]: { status: 'running' },
      }));
      vscode.postMessage(
        writeUpMessage({
          kind: 'TestRunRequest',
          value: {
            scope: testScope,
            reset_outputs: resetOutputs,
          },
        })
      );
    };
  };

  // The 'run test' and 'reset test outputs' commands are
  // very similar, so we factor them
  const onTestRun = useCallback(_onTestRun(false), [vscode]);
  const onTestOutputsReset = useCallback(_onTestRun(true), [vscode]);

  const onDiffResolved = useCallback(
    (scope: string, path: PathSegment[]): void => {
      setTestRunState((prev) => {
        const entry = prev[scope];
        if (!entry?.results || entry.results.kind !== 'Ok') return prev;
        const currentDiffs = entry.results.value.diffs;
        const filtered: Diff[] = currentDiffs.filter(
          (d) => !pathEquals(d.path, path)
        );
        return {
          ...prev,
          [scope]: {
            ...entry,
            results: {
              kind: 'Ok',
              value: { ...entry.results.value, diffs: filtered },
            },
          },
        };
      });
    },
    []
  );

  const onAddNewTest = useCallback((): void => {
    vscode.postMessage(
      writeUpMessage({
        kind: 'OpenTestScopePicker',
      })
    );
  }, [vscode]);

  function _resultsToStatus(results: TestRunResults): TestRunStatus {
    if (results.kind === 'Ok') {
      return 'success';
    } else if (results.kind === 'Cancelled') {
      return 'cancelled';
    } else {
      return 'error';
    }
  }

  useEffect(() => {
    const handleMessage = (event: MessageEvent): void => {
      const message = readDownMessage(event.data);
      switch (message.kind) {
        case 'Update':
          setState(parseResultsToUiState(message.value));
          break;
        case 'TestRunResults': {
          const { scope, reset_outputs, results } = message.value;
          setTestRunState((prev) => {
            const next: TestRunState = {
              ...prev,
              [scope]: {
                status: _resultsToStatus(results),
                results,
              },
            };
            if (reset_outputs && results.kind === 'Ok') {
              delete next[scope];
            }
            return next;
          });
          break;
        }
        case 'FileSelectedForNewTest':
          // Legacy modal flow removed; ignore if received
          break;
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
      return <ParsingErrorWarning message={state.message} vscode={vscode} />;
    }
    case 'emptyTestListMismatch': {
      return <EmptyTestListMismatchWarning vscode={vscode} />;
    }
    case 'initializing':
      return (
        <strong>
          <FormattedMessage id="app.initializing" />
        </strong>
      );
    case 'success': {
      if (state.tests.length === 0) {
        return (
          <>
            <div className="test-editor-empty">
              <p className="test-editor-empty-message">
                <FormattedMessage id="testFile.noTests" />
              </p>
              <button className="test-editor-add-button" onClick={onAddNewTest}>
                <span className="codicon codicon-add"></span>
                <FormattedMessage id="testFile.addTest" />
              </button>
            </div>
          </>
        );
      }
      return (
        <div className="test-editor-container">
          {state.tests.map((test) => (
            <TestEditor
              test={test}
              key={test.testing_scope}
              onTestChange={onTestChange}
              onTestDelete={onTestDelete}
              onTestRun={onTestRun}
              onTestOutputsReset={onTestOutputsReset}
              runState={testRunState[test.testing_scope]}
              onDiffResolved={onDiffResolved}
            />
          ))}
        </div>
      );
    }
    default:
      assertUnreachable(state);
  }
}

function ParsingErrorWarning({
  message,
  vscode,
}: {
  message: string;
  vscode: WebviewApi<unknown>;
}): ReactElement {
  return (
    <div role="alert" className="test-editor-error">
      <h2>
        <FormattedMessage id="testFile.errorTitle" />
      </h2>
      <pre className="test-editor-error-message">{message}</pre>
      <button
        className="test-editor-open-text"
        onClick={() =>
          vscode.postMessage(writeUpMessage({ kind: 'OpenInTextEditor' }))
        }
      >
        <span className="codicon codicon-edit"></span>
        <FormattedMessage id="testFile.openTextEditor" />
      </button>
    </div>
  );
}

function EmptyTestListMismatchWarning({
  vscode,
}: {
  vscode: WebviewApi<unknown>;
}): ReactElement {
  return (
    <div className="test-editor-warning">
      <h2>
        <FormattedMessage id="testFile.formatIssueTitle" />
      </h2>
      <p>
        <FormattedMessage id="testFile.formatIssueText" />
      </p>
      <p>
        <FormattedMessage id="testFile.formatIssueFix" />
      </p>
      <div className="test-editor-warning-actions">
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
    </div>
  );
}

function parseResultsToUiState(tests: ParseResults): UIState {
  switch (tests.kind) {
    case 'ParseError':
      return { state: 'error', message: tests.value };
    case 'EmptyTestListMismatch':
      return { state: 'emptyTestListMismatch' };
    case 'Results':
      return { state: 'success', tests: tests.value };
    default:
      assertUnreachable(tests);
  }
}
