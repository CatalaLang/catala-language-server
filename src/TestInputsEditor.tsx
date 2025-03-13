import { type ReactElement, useState } from 'react';
import type { TestInputs, TestIo } from './generated/test_case';
import ValueEditor from './ValueEditors';
import CollapsibleRow from './CollapsibleRow';
import { isCollapsible } from './ValueEditors';

type Props = {
  test_inputs: TestInputs;
  onTestInputsChange(newValue: TestInputs): void;
};

export default function TestInputsEditor(props: Props): ReactElement {
  return (
    <>
      <table className="test-inputs-table">
        <tbody>
          {Array.from(props.test_inputs, ([inputName, testIo]) => {
            const [isFolded, setIsFolded] = useState(false);

            function onTestInputChange(newValue: TestIo): void {
              props.onTestInputsChange(
                new Map([...props.test_inputs, [inputName, newValue]])
              );
            }

            return (
              <CollapsibleRow
                key={inputName}
                label={inputName}
                isCollapsed={isCollapsible(testIo.typ) ? isFolded : undefined}
                onToggleCollapse={
                  isCollapsible(testIo.typ)
                    ? () => setIsFolded(!isFolded)
                    : undefined
                }
              >
                <ValueEditor
                  testIO={testIo}
                  onValueChange={onTestInputChange}
                />
              </CollapsibleRow>
            );
          })}
        </tbody>
      </table>
    </>
  );
}
