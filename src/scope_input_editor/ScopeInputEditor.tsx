import { type ReactElement, useCallback, useEffect, useRef, useState } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import {
  type Test,
  type TestInputs,
  type TestRunResults,
  type RuntimeValue,
  writeUpMessage,
  readDownMessage,
  ParseResults,
  TestRunOutput,
} from '../generated/catala_types';
import TestInputsEditor from '../TestInputsEditor';
import { confirm, resolveConfirmResult } from '../messaging/confirm';
import { setVsCodeApi } from '../webviewApi';
import { WebviewApi } from 'vscode-webview';
import { assertUnreachable } from '../util';
import { logger } from '../logger';
import ScopeOutputs from './ScopeOutputs';

type UIState =
  | { state: 'initializing' }
  | { state: 'error'; message: string }
  | { state: 'emptyTestListMismatch' }
  | { state: 'success'; test: Test };

export type TestRunState =
  | { status: 'stall' }
  | { status: 'running' }
  | { status: 'success'; results: TestRunOutput }
  | { status: 'error'; message: string }
  | { status: 'cancelled' }

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
    (newValue: Test): void => {
      if (state.state === 'success') {
        setState({ state: 'success', test: newValue });
        vscode.postMessage(
          writeUpMessage({
            kind: 'GuiEdit',
            value: [[newValue], false],
          })
        );
      }
    },
    [state, vscode, setTestRunState]
  );

  const _onTestRun =
    (args: { testScope: string, action: 'Run' | 'Terminal' | 'Debug' }): void => {
      if (args.action == 'Terminal') setTestRunState((prev) => ({ status: 'running' }));
      vscode.postMessage(
        writeUpMessage({
          kind: 'TestRunRequest',
          value: {
            scope: args.testScope,
            reset_outputs: false,
            in_shell: args.action == 'Terminal',
            debug: args.action == 'Debug',
          },
        })
      );
    };

  const onTestRun = useCallback(_onTestRun, [vscode]);

  function _resultsToState(results: TestRunResults): TestRunState {
    if (results.kind === 'Ok') {
      return { status: 'success', results: results.value };
    } else if (results.kind === 'Cancelled') {
      return { status: 'cancelled' };
    } else {
      return { status: 'error', message: results.value };
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
          setTestRunState((prev) => {
            const next: TestRunState = _resultsToState(message.value.results);
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
          <ScopeInputComponent
            title={scopename}
            test={state.test}
            key={state.test.testing_scope}
            onTestChange={onTestChange}
            onTestRun={onTestRun}
            runState={testRunState}
          />
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
      return { state: 'success', test: tests.value[0] };
    default:
      assertUnreachable(tests);
  }
}

type PropsInputComp = {
  title: string;
  test: Test;
  onTestChange(newValue: Test): void;
  onTestRun(args: { testScope: string, action: 'Run' | 'Terminal' | 'Debug' }): void;
  runState: TestRunState;
};

function ScopeInputComponent(props: PropsInputComp): ReactElement {
  const intl = useIntl();

  function onTestInputsChange(newValue: TestInputs): void {
    props.onTestChange(
      {
        ...props.test,
        test_inputs: newValue,
      }
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

  function hasUnsetInTestInputs(test: Test): boolean {
    const inputsHas = Array.from(test.test_inputs.values()).some(
      (io) => !io.value || !io.value.value || containsUnsetInRuntime(io.value.value)
    );
    return inputsHas;
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
    if (hasUnsetInTestInputs(props.test))
      scrollToFirstUnset()
    else
      props.onTestRun({ testScope: props.test.tested_scope.name, action: 'Run' });
  };

  const runInTermWithUnsetCheck = async (): Promise<void> => {
    if (hasUnsetInTestInputs(props.test))
      scrollToFirstUnset();
    else
      props.onTestRun({ testScope: props.test.tested_scope.name, action: 'Terminal' });
  };

  const debugWithUnsetCheck = async (): Promise<void> => {
    if (hasUnsetInTestInputs(props.test))
      scrollToFirstUnset()
    else
      props.onTestRun({ testScope: props.test.tested_scope.name, action: 'Debug' });
  };

  return (
    <div className="test-editor" ref={unsetElementRef}>
      <div className="test-editor-content">
        <div className="test-section">
          <h2 className="test-section-title heading-h2">
            {props.title} : <FormattedMessage id="testEditor.inputs" />
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
          <div className="test-result-action-bar">
            <button
              className={`button-action-dvp ${props.runState.status ?? ''}`}
              title={intl.formatMessage({ id: 'testEditor.run' })}
              onClick={runWithUnsetCheck}
              disabled={props.runState.status === 'running'}>
              <span
                className={`codicon ${props.runState?.status === 'running'
                  ? 'codicon-loading codicon-modifier-spin'
                  : 'codicon-play'}`}
              />
              <FormattedMessage id='testEditor.run' />
            </button>
            <button
              className={`button-action-dvp ${props.runState ?? ''}`}
              title={intl.formatMessage({ id: 'testEditor.shell_run' })}
              onClick={runInTermWithUnsetCheck}
              disabled={props.runState.status === 'running'}>
              <span className='codicon codicon-play' />
              <FormattedMessage id='testEditor.shell_run' />
            </button>
            <button
              className={`button-action-dvp ${props.runState ?? ''}`}
              title={intl.formatMessage({ id: 'testEditor.debug_run' })}
              onClick={debugWithUnsetCheck}
              disabled={props.runState.status === 'running'}>
              <span className='codicon codicon-play' />
              <FormattedMessage id='testEditor.debug_run' />
            </button>
          </div>
        </div>
        <div className="test-result">
          <ScopeOutputs test_run_output={props.runState} />
        </div>
      </div>
    </div>
  );
}
