import { type ReactElement } from 'react';
import type { Test, TestInputs } from './generated/test_case';
import TestInputsEditor from './TestInputsEditor';
import TestOutputsEditor from './TestOutputsEditor';

type Props = {
  test: Test;
  onTestChange(newValue: Test): void;
  onTestDelete(testScope: string): void;
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
          className="test-editor-run"
          title="Run test"
          onClick={() => {
            /* TODO: Implement test run logic */
          }}
        >
          <span className="codicon codicon-play"></span>
        </button>
        <span className="test-editor-scope">
          <b>{props.test.testing_scope}</b> ➛{' '}
          {String(props.test.tested_scope.name)}
        </span>
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
          <h2 className="test-section-title">Outputs/Assertions</h2>
          <TestOutputsEditor
            test={props.test}
            onTestChange={props.onTestChange}
          />
        </div>
      </div>
    </div>
  );
}
