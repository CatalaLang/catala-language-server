import { type ReactElement } from 'react';
import type { TestInputs, TestIo } from './generated/test_case';
import ValueEditor from './editors/ValueEditors';
import { CompositeEditor, type EditorItem } from './editors/CompositeEditor';

type Props = {
  test_inputs: TestInputs;
  onTestInputsChange(newValue: TestInputs): void;
};

export default function TestInputsEditor(props: Props): ReactElement {
  // Create editor items for CompositeEditor
  const editorItems: EditorItem[] = Array.from(props.test_inputs.entries()).map(
    ([inputName, testIo]) => {
      function onTestInputChange(newValue: TestIo): void {
        props.onTestInputsChange(
          new Map([...props.test_inputs, [inputName, newValue]])
        );
      }

      return {
        key: inputName,
        label: inputName,
        type: testIo.typ,
        editor: (
          <ValueEditor testIO={testIo} onValueChange={onTestInputChange} />
        ),
      };
    }
  );

  return (
    <div className="test-inputs data-card">
      <CompositeEditor items={editorItems} />
    </div>
  );
}
