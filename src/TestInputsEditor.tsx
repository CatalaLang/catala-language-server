import { type ReactElement, useEffect } from 'react';
import type { TestInputs, TestIo } from './generated/test_case';
import ValueEditor from './ValueEditors';
import { clearValidationErrors } from './validation';

type Props = {
  test_inputs: TestInputs;
  onTestInputsChange(newValue: TestInputs): void;
};

// An editor for test inputs. Inputs are named and typed, and
// may be optional (provided as context) or mandatory.
//
// This component aggregates all inputs for a single test.
export default function TestInputsEditor(props: Props): ReactElement {
  // Clear validation errors when component unmounts
  useEffect(() => {
    return (): void => {
      // Clean up validation errors for this component
      Array.from(props.test_inputs.keys()).forEach((inputName) => {
        clearValidationErrors(`input_${inputName}`);
      });
    };
  }, []);
  // TODO: discuss behavior w.r.t mandatory and optional inputs
  // and implement an UI that reflects those (e.g. reset an optional
  // input to its default value and mark it as unedited, provide user
  // feedback if mandatory inputs are omitted...)

  return (
    <>
      <table className="test-inputs-table">
        <tbody>
          {Array.from(props.test_inputs, ([inputName, testIo]) => {
            function onTestInputChange(newValue: TestIo): void {
              props.onTestInputsChange(
                new Map([...props.test_inputs, [inputName, newValue]])
              );
            }

            return (
              <tr key={inputName}>
                <td>
                  <strong className="identifier">{inputName}</strong>
                </td>
                <td>
                  <ValueEditor
                    testIO={testIo}
                    onValueChange={onTestInputChange}
                    validationId={`input_${inputName}`}
                  />
                </td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </>
  );
}
