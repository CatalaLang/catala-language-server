import type { ChangeEvent } from 'react';
import { type ReactElement, useState } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import {
  type Test,
  type TestInputs,
  type TestRunResults,
} from './generated/test_case';
import TestInputsEditor from './TestInputsEditor';
import TestOutputsEditor from './TestOutputsEditor';
import { type TestRunStatus } from './TestFileEditor';

type Props = {
  test: Test;
  onTestChange(newValue: Test, mayBeBatched: boolean): void;
  onTestDelete(testScope: string): void;
  onTestRun(testScope: string): void;
  onTestOutputsReset(testScope: string): void;
  runState?: {
    status: TestRunStatus;
    results?: TestRunResults;
  };
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

  const [isCollapsed, setIsCollapsed] = useState(false);

  return (
    <div className="test-editor">
      <div className="test-editor-bar">
        <button
          className="test-editor-collapse"
          title={isCollapsed ? 'Expand test case' : 'Collapse test case'}
          onClick={() => setIsCollapsed(!isCollapsed)}
        >
          <span
            className={`codicon ${isCollapsed ? 'codicon-unfold' : 'codicon-fold'}`}
          ></span>
        </button>
        <button
          className={`test-editor-run ${props.runState?.status ?? ''}`}
          title={intl.formatMessage({ id: 'testEditor.run' })}
          onClick={() => props.onTestRun(props.test.testing_scope)}
          disabled={props.runState?.status === 'running'}
        >
          <span
            className={`codicon ${props.runState?.status === 'running' ? 'codicon-loading codicon-modifier-spin' : 'codicon-play'}`}
          ></span>
        </button>
        <span className="test-editor-scope">
          <b>{props.test.testing_scope}</b> ➛{' '}
          {String(props.test.tested_scope.name)}
        </span>
        {props.runState?.status === 'success' &&
          props.runState?.results?.kind === 'Ok' &&
          !props.runState.results.value.assert_failures && (
            <span className="test-run-success">
              <FormattedMessage
                id="testEditor.passed"
                defaultMessage="Passed"
              />
            </span>
          )}
        {(props.runState?.status === 'error' ||
          (props.runState?.results?.kind === 'Ok' &&
            props.runState.results.value.assert_failures)) && (
          <span className="test-run-error">
            <FormattedMessage id="testEditor.failed" defaultMessage="Failed" />
          </span>
        )}
        <button
          className="test-editor-delete"
          title="Delete test"
          onClick={() => props.onTestDelete(props.test.testing_scope)}
        >
          <span className="codicon codicon-trash"></span>
        </button>
      </div>
      <div
        className="test-editor-content"
        style={{ display: isCollapsed ? 'none' : 'block' }}
      >
        <div className="test-section">
          <h2 className="test-section-title">
            <FormattedMessage
              id="testEditor.description"
              defaultMessage="Description"
            />
          </h2>
          <div className="test-description-editor">
            <textarea
              value={props.test.description}
              onChange={onDescriptionChange}
              onBlur={onDescriptionChange}
              placeholder={intl.formatMessage({
                id: 'testEditor.descriptionPlaceholder',
              })}
              rows={3}
              className="test-description-textarea"
            />
          </div>
        </div>
        <div className="test-section">
          <h2 className="test-section-title">
            <FormattedMessage id="testEditor.inputs" />
          </h2>
          <TestInputsEditor
            test_inputs={props.test.test_inputs}
            onTestInputsChange={onTestInputsChange}
          />
        </div>
        <div className="test-section">
          <div className="test-section-header">
            <h2 className="test-section-title">
              <FormattedMessage id="testEditor.expectedValues" />
              <button
                className="reset-expected-values"
                title={intl.formatMessage({ id: 'testEditor.resetExpected' })}
                onClick={() => {
                  props.onTestOutputsReset(props.test.testing_scope);
                }}
              >
                <span className="codicon codicon-refresh"></span>
              </button>
            </h2>
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
            actualOutputs={
              props.runState?.results?.kind === 'Ok'
                ? props.runState.results.value.test_outputs
                : undefined
            }
          />
        </div>
      </div>
    </div>
  );
}
