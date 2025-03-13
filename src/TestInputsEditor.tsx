import { type ReactElement, useState } from 'react';
import type { TestInputs, TestIo } from './generated/test_case';
import ValueEditor from './ValueEditors';
import Row from './Row';
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
              <Row
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
              </Row>
            );
          })}
        </tbody>
      </table>
    </>
  );
}
