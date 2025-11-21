import { type ReactElement } from 'react';
import { FormattedMessage } from 'react-intl';
import type { Test, TestIo, Diff, PathSegment } from './generated/test_case';
import AssertionValueEditor from './AssertionValueEditor';
import { getDefaultValue } from './defaults';

type Props = {
  test: Test;
  onTestChange(newValue: Test): void;
  diffs?: Diff[];
  onDiffResolved?: (path: PathSegment[]) => void;
  onInvalidateDiffs?: (pathPrefix: PathSegment[]) => void;
};

/* An editor for test outputs. Outputs are named and typed, and
   are *always* optional -- each defined output corresponds to
   an assertion, and each test contains zero or more assertions. 

   Two data sources are used for producing this component:
    - `tested_scope.outputs` is the set of all *possible* assertions
    (names and types, no value)
    - `test_outputs` (which may be better named `test_assertions`?) is the set of
    checked assertions; the names in `test_outputs` is/should be a subset of `tested_scope.outputs`
    - `test_outputs` is a named set of TestIo ; Values are defined if an assertion is present,
    and undefined otherwise.

   This component aggregates all outputs (assertions)
   for a single test and offers an interface for CRUD operations
   on assertions.
*/

export default function TestOutputsEditor({
  test,
  onTestChange: onTestAssertsChange,
  diffs = [],
  onDiffResolved,
  onInvalidateDiffs,
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
      test_outputs: new Map(test_outputs).set(outputName, newValue),
    });
  }

  function onAssertDelete(outputName: string): void {
    const newTestOutputs = new Map(test_outputs);
    newTestOutputs.delete(outputName);
    onTestAssertsChange({
      ...test,
      test_outputs: newTestOutputs,
    });
  }

  return (
    <div className="test-outputs-editor">
      <div className="test-outputs data-card">
        {Array.from(tested_scope.outputs, ([outputName, _outputType]) => {
          const outputData = test_outputs.get(outputName);

          // NOTE: Diffs are absolute from the test outputs root; keep absolute (no slicing).

          return (
            <div key={outputName} className="test-output-row">
              <label>{outputName}</label>
              {outputData?.value ? (
                <AssertionValueEditor
                  testIO={outputData}
                  onValueChange={(newValue) =>
                    onAssertValueChange(outputName, newValue)
                  }
                  onAssertionDeletion={() => onAssertDelete(outputName)}
                  diffs={diffs}
                  currentPath={[{ kind: 'StructField', value: outputName }]}
                  onDiffResolved={onDiffResolved}
                  onInvalidateDiffs={onInvalidateDiffs}
                />
              ) : (
                <div className="assertion-editor">
                  <button
                    className="button-action-dvp"
                    onClick={() => onAssertAdd(outputName)}
                  >
                    <span className="codicon codicon-add"></span>
                    <FormattedMessage id="testOutputs.addExpectedValue" />
                  </button>
                </div>
              )}
            </div>
          );
        })}
      </div>
    </div>
  );
}
