import type { TestIo } from './generated/test_case';
import ValueEditor from './ValueEditors';

type Props = {
  testIO: TestIo;
  onValueChange: (newValue: TestIo) => void;
  onAssertionDeletion: () => void;
};

export default function AssertionValueEditor({
  testIO,
  onValueChange,
  onAssertionDeletion,
}: Props): React.ReactElement {
  return (
    <div className="assertion-value-editor">
      <div className="assertion-value-content">
        <ValueEditor testIO={testIO} onValueChange={onValueChange} />
      </div>
      <button
        className="assertion-delete"
        title="Delete assertion"
        onClick={onAssertionDeletion}
      >
        <span className="codicon codicon-trash"></span>
      </button>
    </div>
  );
}