import { type ReactElement } from 'react';
import type { TestInputs, TestIo } from './generated/test_case';
import ValueEditor from './ValueEditors';

type Props = {
  test_inputs: TestInputs;
  onTestInputsChange(newValue: TestInputs): void;
};

export default function TestInputsEditor(props: Props): ReactElement {
  return (
    <>
      <div className="test-inputs data-card">
        {Array.from(props.test_inputs, ([inputName, testIo]) => {
          function onTestInputChange(newValue: TestIo): void {
            props.onTestInputsChange(
              new Map([...props.test_inputs, [inputName, newValue]])
            );
          }

          return (
            <>
              <label>{inputName}</label>
              <ValueEditor testIO={testIo} onValueChange={onTestInputChange} />
            </>
          );
        })}
      </div>
    </>
  );
}
