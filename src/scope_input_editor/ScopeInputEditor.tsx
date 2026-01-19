import type { ChangeEvent } from 'react';
import { type ReactElement, useCallback, useEffect, useRef, useState } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import {
  type Test,
  type TestInputs,
  type TestRunResults,
  type PathSegment,
  type RuntimeValue,
  TestList,
  writeUpMessage,
  Diff,
  readDownMessage,
  ParseResults,
  writeDownMessage,
} from '../generated/catala_types';
import TestInputsEditor from '../TestInputsEditor';
import TestOutputsEditor from '../TestOutputsEditor';
import { confirm, resolveConfirmResult } from '../messaging/confirm';
import { setVsCodeApi } from '../webviewApi';
import { WebviewApi } from 'vscode-webview';
import { isPathPrefix, pathEquals } from '../diff/highlight';
import { assertUnreachable } from '../util';
import { logger } from '../logger';
import ScopeOutputsEditor from './ScopeOutputsEditor';



type UIState =
  | { state: 'initializing' }
  | { state: 'error'; message: string }
  | { state: 'emptyTestListMismatch' }
  | { state: 'success'; tests: TestList };

export type TestRunStatus = 'stall' | 'running' | 'success' | 'error' | 'cancelled';

type TestRunState = {
  status: TestRunStatus;
  results?: TestRunResults;
};

type Props = { contents: UIState; vscode: WebviewApi<unknown>; scopename: string };


export default function ScopeInputEditor({
  contents,
  vscode,
  scopename,
}: Props): ReactElement {
  const [state, setState] = useState(contents);
  const [testRunState, setTestRunState] = useState<TestRunState>({ status: 'stall' });
  useEffect(() => {
    setVsCodeApi(vscode);
  }, [vscode]);

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

  const _onTestRun = (resetOutputs: boolean) => {
    return (testScope: string): void => {
      setTestRunState((prev) => ({ status: 'running' }));
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
          console.log("IN TESTRUNRESULTS");
          console.log(JSON.stringify(writeDownMessage(message)));
          const { scope, reset_outputs, results } = message.value;
          setTestRunState((prev) => {
            const next: TestRunState = {
              status: _resultsToStatus(results),
              results
            };
            console.log("bla");
            console.log(next)
            return next;
          });
          break;
        }
        case 'ConfirmResult': {
          resolveConfirmResult(message.value.id, message.value.confirmed);
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
      return (
        <div className="test-editor-container">
          {state.tests.map((test) => (
            <ScopeInputComponent
              title={scopename}
              test={test}
              key={test.testing_scope}
              onTestChange={onTestChange}
              onTestRun={onTestRun}
              runState={testRunState}
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


type Props2 = {
  title: string;
  test: Test;
  onTestChange(newValue: Test, mayBeBatched: boolean): void;
  onTestRun(testScope: string): void;
  runState?: {
    status: TestRunStatus;
    results?: TestRunResults;
    stale?: boolean;
  };
};

function ScopeInputComponent(props: Props2): ReactElement {
  const intl = useIntl();

  function onTestInputsChange(newValue: TestInputs): void {
    props.onTestChange(
      {
        ...props.test,
        test_inputs: newValue,
      },
      false
    );
  }

  const expectedSectionRef = useRef<HTMLDivElement>(null);
  // Scope for searching the first '.invalid-badge' or '.unset-badge' before running; used to scroll into view
  const unsetElementRef = useRef<HTMLDivElement>(null);
  const expectedAnchorId = `expected-${encodeURIComponent(props.test.testing_scope)}`;



  function containsUnsetInRuntime(rv: RuntimeValue): boolean {
    switch (rv.value.kind) {
      case 'Unset':
        return true;
      case 'Array':
        return rv.value.value.some(containsUnsetInRuntime);
      case 'Struct': {
        const map = rv.value.value[1];
        return Array.from(map.values()).some(containsUnsetInRuntime);
      }
      case 'Enum': {
        const payload = rv.value.value[1][1];
        return payload !== null && payload.value
          ? containsUnsetInRuntime(payload.value)
          : false;
      }
      default:
        return false;
    }
  }

  function hasUnsetInTest(test: Test): boolean {
    const inputsHas = Array.from(test.test_inputs.values()).some(
      (io) => io.value && containsUnsetInRuntime(io.value.value)
    );
    const outputsHas = Array.from(test.test_outputs.values()).some(
      (io) => io.value && containsUnsetInRuntime(io.value.value)
    );
    return inputsHas || outputsHas;
  }

  const scrollToFirstUnset = (): void => {
    setTimeout(() => {
      const container = unsetElementRef.current ?? document;
      const el = container.querySelector(
        '.invalid-badge, .unset-badge'
      ) as HTMLElement | null;
      if (el) {
        el.scrollIntoView({ behavior: 'smooth', block: 'center' });
        (el as HTMLElement)?.focus?.();
      }
    }, 0);
  };

  const runWithUnsetCheck = async (): Promise<void> => {
    if (hasUnsetInTest(props.test)) {
      scrollToFirstUnset();
      const confirmed = await confirm('RunTestWithUnsetValues');
      if (!confirmed) return;
    }
    props.onTestRun(props.test.tested_scope.name);
  };
  console.log("XXX comp render")
  console.log(props);
  return (
    <div className="test-editor" ref={unsetElementRef}>
      <h2> {props.title} </h2>
      <div className="test-editor-content">
        <div className="test-section">
          <h2 className="test-section-title heading-h2">
            <FormattedMessage id="testEditor.inputs" />
          </h2>
          <TestInputsEditor
            test_inputs={props.test.test_inputs}
            onTestInputsChange={onTestInputsChange}
          />
        </div>
        <div
          className="test-section"
          id={expectedAnchorId}
          ref={expectedSectionRef}
          tabIndex={-1}
        >
          <div className="test-result-header">
            <div className="test-result-action-bar"></div>
            <button
              className={`button-action-dvp ${props.runState?.status ?? ''}`}
              title={intl.formatMessage({ id: 'testEditor.run' })}
              onClick={runWithUnsetCheck}
              disabled={props.runState?.status === 'running'}
            >
              <span
                className={`codicon ${props.runState?.status === 'running' ? 'codicon-loading codicon-modifier-spin' : 'codicon-play'}`}
              ></span>{' '}
              Lancer le test
            </button>
            {props.runState && props.runState.results ? (
              <ScopeOutputsEditor
                test_run_output={props.runState.results}
              />) : <pre>Run to display results</pre>}
          </div>
        </div>
      </div>
    </div>
  );
}
