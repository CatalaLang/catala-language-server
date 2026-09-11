import type { ChangeEvent } from 'react';
import { type ReactElement, useEffect, useRef } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import {
  type Option,
  type RuntimeValue,
  type Test,
  type TestInputs,
  type TestRunResults,
  type PathSegment,
  type VariableFailure,
} from '../generated/catala_types';
import TestInputsEditor from './TestInputsEditor';
import TestOutputsEditor from './TestOutputsEditor';
import ExpectedVariablesEditor from './ExpectedVariablesEditor';
import { type TestRunStatus } from './TestFileEditor';
import {
  type TraceElement,
  type TraceValue,
  traceValueToRuntime,
} from '../trace-editor/traceUtils';
import { confirm } from '../messaging/confirm';
import { getVsCodeApi } from '../shared/webviewApi';
import {
  hasUnsetInTest,
  scrollToFirstInvalidOrUnset,
} from '../editors/unsetValidation';

type Props = {
  test: Test;
  onTestChange(newValue: Test, mayBeBatched: boolean): void;
  onTestDelete(testScope: string): void;
  onTestRun(testScope: string, hasExpected: boolean): void;
  onTestOutputsReset(testScope: string, hasExpected: boolean): void;
  runState?: {
    status: TestRunStatus;
    results?: TestRunResults;
    stale?: boolean;
  };
  trace?: TraceElement[];
  /** When false, the trace is not run and the expected-variable catalog is hidden. */
  runTrace?: boolean;
  onDiffResolved(scope: string, path: PathSegment[]): void;
  onInvalidateDiffs(scope: string, pathPrefix: PathSegment[]): void;
};

// Editor for a single test case (child of TestFileEditor)
export default function TestEditor(props: Props): ReactElement {
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

  function onDescriptionChange(event: ChangeEvent<HTMLTextAreaElement>): void {
    props.onTestChange(
      {
        ...props.test,
        description: event.target.value,
      },
      true
    );
  }

  function onTitleChange(newTitle: string): void {
    props.onTestChange(
      {
        ...props.test,
        title: newTitle,
      },
      true
    );
  }

  function onVariablesChange(next: Map<string, TraceValue | null>): void {
    const variables: Map<string, Option<RuntimeValue>> = new Map();
    next.forEach((value, name) => {
      if (value === null) {
        variables.set(name, null);
      } else {
        const rv = traceValueToRuntime(value);
        if (rv !== undefined) {
          variables.set(name, { value: { value: rv, attrs: [] } });
        }
      }
    });
    props.onTestChange({ ...props.test, variables }, false);
  }

  // Mismatches on the auxiliary variables, as reported by the compiler for the
  // last run. Keyed by variable name, which is also the key of
  // `test.variables`. Empty until a run happened.
  const variableFailures: VariableFailure[] =
    props.runState?.results?.kind === 'Ok'
      ? props.runState.results.value.variable_failures
      : [];

  const expectedSectionRef = useRef<HTMLDivElement>(null);
  // Scope for searching the first '.value-editor.invalid' or '.value-editor.unset' before running; used to scroll into view
  const unsetElementRef = useRef<HTMLDivElement>(null);
  const expectedAnchorId = `expected-${encodeURIComponent(props.test.testing_scope)}`;

  // A failing run sends the user to what went wrong, and the scope results come
  // first: when an output is also incorrect the expected-values section takes
  // the focus, and the expected variables only get it when they are the sole
  // culprit.
  const focusVariableFailure =
    props.runState?.results?.kind === 'Ok' &&
    !props.runState.results.value.assert_failures &&
    variableFailures.length > 0;

  useEffect(() => {
    const runState = props.runState;
    const shouldFocus =
      !!runState &&
      runState.results?.kind === 'Ok' &&
      runState.results.value.assert_failures;

    if (shouldFocus) {
      setTimeout(() => {
        expectedSectionRef.current?.focus();
        expectedSectionRef.current?.scrollIntoView({
          behavior: 'smooth',
          block: 'start',
        });
      }, 0);
    }
  }, [props.runState]);

  // Whether the run needs to be traced: the trace is what the compiler checks
  // the expected variables against, so it is only worth producing when the
  // test declares some.
  const hasExpected = props.test.variables.size > 0;

  const scrollToFirstUnset = (): void => {
    scrollToFirstInvalidOrUnset(unsetElementRef.current ?? document, 0);
  };

  const runWithUnsetCheck = async (): Promise<void> => {
    if (hasUnsetInTest(props.test)) {
      scrollToFirstUnset();
      const confirmed = await confirm('RunTestWithUnsetValues');
      if (!confirmed) return;
    }
    props.onTestRun(props.test.testing_scope, hasExpected);
  };

  const openTraceEditor = (): void => {
    getVsCodeApi().postMessage({
      kind: 'openTraceEditor',
      scope: props.test.testing_scope,
      trace: props.trace,
    });
  };

  const resetWithUnsetCheck = async (): Promise<void> => {
    if (hasUnsetInTest(props.test)) {
      scrollToFirstUnset();
      const confirmed = await confirm('RunTestWithUnsetValues');
      if (!confirmed) return;
    }
    props.onTestOutputsReset(props.test.testing_scope, hasExpected);
  };

  return (
    <div className="test-editor" ref={unsetElementRef}>
      <div className="test-editor-breadcrumb body-b3">
        {props.test.testing_scope} ➛ {String(props.test.tested_scope.name)}
      </div>
      <div className="test-title-wrapper">
        <input
          type="text"
          className="test-title-input heading-h2"
          value={props.test.title}
          onChange={(e) => onTitleChange(e.target.value)}
          aria-label={intl.formatMessage({
            id: 'testEditor.title',
            defaultMessage: 'Title',
          })}
          placeholder={intl.formatMessage({
            id: 'testEditor.titlePlaceholder',
            defaultMessage: 'Test title...',
          })}
        />
        <span
          className="codicon codicon-edit test-title-edit-icon"
          aria-hidden="true"
        />
      </div>
      <div className="test-editor-content">
        <div className="test-section">
          <h2 className="test-section-title heading-h2">
            <FormattedMessage
              id="testEditor.description"
              defaultMessage="Description"
            />
          </h2>
          <div className="test-description-editor">
            <textarea
              value={props.test.description}
              onChange={onDescriptionChange}
              placeholder={intl.formatMessage({
                id: 'testEditor.descriptionPlaceholder',
              })}
              rows={10}
              className="test-description-textarea"
            />
          </div>
        </div>
        <div className="test-section">
          <h2 className="test-section-title heading-h2">
            <FormattedMessage id="testEditor.inputs" />
          </h2>
          <TestInputsEditor
            test_inputs={props.test.test_inputs}
            tested_scope={props.test.tested_scope}
            onTestInputsChange={onTestInputsChange}
          />
        </div>
        <ExpectedVariablesEditor
          test={props.test}
          trace={props.trace}
          runTrace={props.runTrace}
          failures={variableFailures}
          focusFailure={focusVariableFailure}
          onChange={onVariablesChange}
        />
        <div
          className="test-section"
          id={expectedAnchorId}
          ref={expectedSectionRef}
          tabIndex={-1}
        >
          <h2 className="test-section-title heading-h2">
            <FormattedMessage id="testEditor.expectedValues" />
          </h2>
          <div className="test-result-header">
            <div className="test-result-action-bar">
              <button
                className="reset-expected-values button-action-dvp body-b3"
                title={intl.formatMessage({ id: 'testEditor.resetExpected' })}
                onClick={resetWithUnsetCheck}
              >
                <span className="codicon codicon-refresh"></span>{' '}
                <FormattedMessage id="testEditor.resetExpectedButton" />
              </button>
              <button
                className={`button-action-dvp body-b3 ${props.runState?.status ?? ''}`}
                title={intl.formatMessage({ id: 'testEditor.runTest' })}
                onClick={runWithUnsetCheck}
                disabled={props.runState?.status === 'running'}
              >
                <span
                  className={`codicon ${props.runState?.status === 'running' ? 'codicon-loading codicon-modifier-spin' : 'codicon-play'}`}
                ></span>{' '}
                {intl.formatMessage({ id: 'testEditor.runTest' })}
              </button>
              <button
                className="button-action-dvp body-b3"
                title={intl.formatMessage({ id: 'testEditor.openTrace' })}
                onClick={openTraceEditor}
              >
                <span className="codicon codicon-graph"></span> Trace
              </button>
            </div>
            <div className="test-result">
              {props.runState?.status === 'success' &&
                props.runState?.results?.kind === 'Ok' &&
                !props.runState.results.value.assert_failures &&
                variableFailures.length === 0 && (
                  <p className="test-run-result test-run-success body-1">
                    <span className="codicon codicon-check-all"></span>
                    <FormattedMessage
                      id="testEditor.passed"
                      defaultMessage="Passed"
                    />
                  </p>
                )}
              {(props.runState?.status === 'error' ||
                (props.runState?.results?.kind === 'Ok' &&
                  (props.runState.results.value.assert_failures ||
                    variableFailures.length > 0))) && (
                <div className="test-result-information">
                  <p className="test-run-result test-run-error body-1">
                    <span className="codicon codicon-warning"></span>
                    <FormattedMessage
                      id="testEditor.failed"
                      defaultMessage="Failed"
                    />
                  </p>
                </div>
              )}
            </div>
            {props.runState?.stale && (
              <div className="test-result-information">
                <p className="body-3">
                  <span className="codicon codicon-history"></span>{' '}
                  <FormattedMessage
                    id="testEditor.diffsStale"
                    defaultMessage="Diffs are out of date. Re-run to refresh."
                  />
                </p>
              </div>
            )}
          </div>

          <TestOutputsEditor
            test={props.test}
            onTestChange={(test) => {
              props.onTestChange(test, false);
            }}
            diffs={
              props.runState?.results?.kind === 'Ok'
                ? props.runState.results.value.diffs
                : []
            }
            onDiffResolved={(path: PathSegment[]) =>
              props.onDiffResolved(props.test.testing_scope, path)
            }
            onInvalidateDiffs={(pathPrefix: PathSegment[]) =>
              props.onInvalidateDiffs(props.test.testing_scope, pathPrefix)
            }
          />
        </div>
      </div>
    </div>
  );
}
