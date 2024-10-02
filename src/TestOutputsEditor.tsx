import type { ReactElement } from 'react';
import type { Test, TestIo } from './generated/test_case';
import ValueEditor from './ValueEditors';
import { getDefaultValue } from './defaults';

type Props = {
  test: Test;
  onTestChange(newValue: Test): void;
};

/* An editor for test outputs. Outputs are named and typed, and
   are *always* optional -- each defined output corresponds to
   an assertion, and each test contains zero or more assertions. 

   Two data sources are used for producing this component:
    - `tested_scope.outputs` is the set of all *possible* assertions
    (names and types, no value)
    - `test_outputs` (which may be better named `test_assertions`?) is the set of
    checked assertions; the names in `test_outputs` is/should be a subset of `tested_scope.outputs`
    - `test_outputs` is a named set of TestIo ; As far as I can tell, values should be mandatory
      (although for records we should be able to test partial values, and for tuples and arrays, 
       I do not know what the 'right' answer is).

   This component aggregates all outputs (assertions)
   for a single test and offers an interface for CRUD operations
   on assertions.

   We will also need a 'reset assertions to current output value'
   which will run the test and fill assertions values with the scope output.
*/

export default function TestOutputsEditor({
  test,
  onTestChange: onTestAssertsChange,
}: Props): ReactElement {
  const { test_outputs, tested_scope } = test;

  function onAssertAdd(outputName: string): void {
    const outputType = tested_scope.outputs.get(outputName);
    if (outputType) {
      const defaultValue = getDefaultValue(outputType);
      onTestAssertsChange({
        ...test,
        test_outputs: new Map(test_outputs).set(outputName, {
          typ: outputType,
          value: { value: defaultValue },
        }),
      });
    }
  }

  function onAssertValueChange(outputName: string, newValue: TestIo): void {
    onTestAssertsChange({
      ...test,
      test_outputs: new Map(test_outputs.set(outputName, newValue)),
    });
  }

  return (
    <div className="test-outputs-editor">
      <table className="test-outputs-table">
        <tbody>
          {Array.from(tested_scope.outputs, ([outputName, _outputType]) => {
            const outputData = test_outputs.get(outputName);
            return (
              <tr key={outputName}>
                <td>
                  <strong className="identifier">{outputName}</strong>
                </td>
                <td>
                  {outputData?.value ? (
                    <ValueEditor
                      testIO={outputData}
                      onValueChange={(newValue) =>
                        onAssertValueChange(outputName, newValue)
                      }
                    />
                  ) : (
                    <button
                      className="test-editor-run"
                      onClick={() => onAssertAdd(outputName)}
                    >
                      <span className="codicon codicon-add"></span>
                      Add expected value
                    </button>
                  )}
                </td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}
