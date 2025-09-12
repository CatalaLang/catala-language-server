import { type ReactElement } from 'react';
import { FormattedMessage } from 'react-intl';
import type {
  Test,
  TestIo,
  Diff,
  PathSegment,
  TestOutputs,
} from './generated/test_case';
import AssertionValueEditor from './AssertionValueEditor';
import ValueEditor from './editors/ValueEditors';
import { getDefaultValue } from './defaults';
import { isElemental } from './util';
import { createHighlightOnlyHook, isPathPrefix } from './diff/highlight';

type Props = {
  test: Test;
  onTestChange(newValue: Test): void;
  diffs?: Diff[];
  actualOutputs?: TestOutputs;
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
  actualOutputs,
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

          return (
            <div key={outputName} className="test-output-row">
              <label>{outputName}</label>
              {outputData?.value ? (
                (() => {
                  const outputPath: PathSegment[] = [
                    { kind: 'StructField' as const, value: outputName },
                  ];
                  const subtreeDiffs = (diffs ?? []).filter((d) =>
                    isPathPrefix(outputPath, d.path)
                  );
                  const isComplex = subtreeDiffs.some((d) => !isElemental(d));

                  if (isComplex && actualOutputs) {
                    const rightHook = createHighlightOnlyHook(subtreeDiffs);

                    return (
                      <div className="two-pane-diff">
                        <div
                          className="pane-expected"
                          style={{ minWidth: '25%', maxWidth: '75%' }}
                        >
                          <AssertionValueEditor
                            testIO={outputData}
                            onValueChange={(newValue) =>
                              onAssertValueChange(outputName, newValue)
                            }
                            onAssertionDeletion={() =>
                              onAssertDelete(outputName)
                            }
                            diffs={subtreeDiffs}
                            currentPath={outputPath}
                            highlightDiffs={false}
                          />
                        </div>
                        <div
                          className="pane-divider"
                          role="separator"
                          aria-orientation="vertical"
                        />
                        <div className="pane-actual">
                          {(() => {
                            const actualIo = actualOutputs.get(outputName);
                            if (!actualIo) return null;
                            return (
                              <ValueEditor
                                testIO={actualIo}
                                onValueChange={() => {
                                  /* read-only */
                                }}
                                editorHook={rightHook}
                                currentPath={outputPath}
                                diffs={subtreeDiffs}
                                editable={false}
                              />
                            );
                          })()}
                        </div>
                      </div>
                    );
                  }

                  return (
                    <AssertionValueEditor
                      testIO={outputData}
                      onValueChange={(newValue) =>
                        onAssertValueChange(outputName, newValue)
                      }
                      onAssertionDeletion={() => onAssertDelete(outputName)}
                      diffs={diffs}
                      currentPath={outputPath}
                    />
                  );
                })()
              ) : (
                <button
                  className="test-editor-run"
                  onClick={() => onAssertAdd(outputName)}
                >
                  <span className="codicon codicon-add"></span>
                  <FormattedMessage id="testOutputs.addExpectedValue" />
                </button>
              )}
            </div>
          );
        })}
      </div>
    </div>
  );
}
