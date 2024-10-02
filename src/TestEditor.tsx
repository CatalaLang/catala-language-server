import { type ReactElement } from 'react';
import type { Test, TestInputs, TestRunResults } from './generated/test_case';
import TestInputsEditor from './TestInputsEditor';
import TestOutputsEditor from './TestOutputsEditor';

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

  return (
    <div className="test-editor">
      <div className="test-editor-bar">
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
      <div className="test-editor-content">
        <div className="test-section">
          <h2 className="test-section-title">Inputs</h2>
          <TestInputsEditor
            test_inputs={props.test.test_inputs}
            onTestInputsChange={onTestInputsChange}
          />
        </div>
        <div className="test-section">
          <div className="test-section-header">
            <h2 className="test-section-title">
              Outputs/Expected values
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
      </div>
    </div>
  );
}
