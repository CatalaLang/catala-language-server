import { type ReactElement, useState } from 'react';
import { FormattedMessage } from 'react-intl';
import {
  type Test,
  type TestInputs,
  type TestRunResults,
} from './generated/test_case';
import TestInputsEditor from './TestInputsEditor';
import TestOutputsEditor from './TestOutputsEditor';
import Results from './Results';
import { select } from './testCaseUtils';

type Props = {
  test: Test;
  onTestChange(newValue: Test): void;
  onTestDelete(testScope: string): void;
  onTestRun(testScope: string): void;
  runState?: {
    status: 'running' | 'success' | 'error';
    results?: TestRunResults;
  };
};

// Editor for a single test case (child of TestFileEditor)
export default function TestEditor(props: Props): ReactElement {
  function onTestInputsChange(newValue: TestInputs): void {
    props.onTestChange({
      ...props.test,
      test_inputs: newValue,
    });
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
          title="Run test"
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
        {props.runState?.status === 'success' && (
          <span className="test-run-success">Passed</span>
        )}
        {props.runState?.status === 'error' && (
          <span className="test-run-error">Failed</span>
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
                title="Reset Expected Values"
              >
                <span className="codicon codicon-refresh"></span>
              </button>
            </h2>
          </div>
          <TestOutputsEditor
            test={props.test}
            onTestChange={props.onTestChange}
          />
        </div>
        {props.runState?.status === 'success' &&
          props.runState?.results &&
          props.runState?.results?.kind === 'Ok' && (
            <div className="test-section">
              <div className="test-section-header">
                <h2 className="test-section-title">
                  <FormattedMessage id="testEditor.results" />
                </h2>
                <Results
                  {...select(
                    props.test.test_outputs,
                    props.runState.results.value
                  )}
                />
              </div>
            </div>
          )}
      </div>
    </div>
  );
}
