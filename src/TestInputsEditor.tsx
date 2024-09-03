import type { ReactElement } from 'react';
import type { TestInputs, TestIo } from './generated/test_case';
import ValueEditor from './ValueEditors';

type Props = {
  test_inputs: TestInputs;
  onTestInputsChange(newValue: TestInputs): void;
};

// An editor for test inputs. Inputs are named and typed, and
// may be optional (provided as context) or mandatory.
//
// This component aggregates all inputs for a single test.
export default function TestInputsEditor(props: Props): ReactElement {
  // TODO: discuss behavior w.r.t mandatory and optional inputs
  // and implement an UI that reflects those (e.g. reset an optional
  // input to its default value and mark it as unedited, provide user
  // feedback if mandatory inputs are omitted...)

  return (
    <>
      <strong>Inputs</strong>
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
                  <strong>{inputName}</strong>
                </td>
                <td>
                  <ValueEditor
                    testIO={testIo}
                    onValueChange={onTestInputChange}
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
