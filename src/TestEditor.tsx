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
        <span className="test-editor-scope">
          <b>{props.test.testing_scope}</b> ➛{' '}
          {String(props.test.tested_scope.name)}
        </span>
        <button
          className="test-editor-delete"
          title="Delete test"
          onClick={() => props.onTestDelete(props.test.testing_scope)}
        >
          🗑️
        </button>
      </div>
      <div className="test-editor-content">
        <TestInputsEditor
          test_inputs={props.test.test_inputs}
          onTestInputsChange={onTestInputsChange}
        />
        <TestOutputsEditor />
      </div>
    </div>
  );
}
