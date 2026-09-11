import { type ReactElement, type ReactNode, useState } from 'react';
import { useIntl } from 'react-intl';
import type {
  PathSegment,
  TestInputs,
  TestIo,
  ScopeDef,
} from '../generated/catala_types';
import ValueEditor, {
  getDefaultValue,
  createRuntimeValue,
} from '../editors/ValueEditors';
import { CompositeEditor, type EditorItem } from '../editors/CompositeEditor';
import { countUnsetIn } from '../editors/unsetValidation';
import { confirm } from '../messaging/confirm';

type InputFieldProps = {
  inputName: string;
  testIo: TestIo;
  isContext: boolean;
  readOnly?: boolean;
  onTestInputChange(newValue: TestIo): void;
  editorHook?: (editor: ReactElement, path: PathSegment[]) => ReactElement;
};

function InputField({
  inputName,
  testIo,
  isContext,
  readOnly,
  onTestInputChange,
  editorHook,
}: InputFieldProps): ReactElement {
  const intl = useIntl();
  // false iff the context var is NotOverridden (using scope-computed default)
  const [overriding, setOverriding] = useState(
    () => testIo.value?.value.value.kind !== 'NotOverridden'
  );

  function startOverride(): void {
    setOverriding(true);
    onTestInputChange({
      ...testIo,
      value: { value: getDefaultValue(testIo.typ), pos: undefined },
    });
  }

  async function resetToDefault(): Promise<void> {
    const hasData = testIo.value?.value.value.kind !== 'NotOverridden';
    if (hasData) {
      const confirmed = await confirm('ResetContextVar');
      if (!confirmed) return;
    }
    setOverriding(false);
    onTestInputChange({
      ...testIo,
      value: {
        value: createRuntimeValue({ kind: 'NotOverridden' }),
        pos: undefined,
      },
    });
  }

  if (!overriding) {
    return (
      <div className="context-var-placeholder">
        <span className="context-var-default-text">
          {intl.formatMessage({ id: 'testEditor.usingComputedDefault' })}
        </span>
        {!readOnly && (
          <button className="button-action-dvp body-b3" onClick={startOverride}>
            {intl.formatMessage({ id: 'testEditor.override' })}
          </button>
        )}
      </div>
    );
  }

  return (
    <div className={isContext ? 'context-var-editor' : undefined}>
      <ValueEditor
        testIO={testIo}
        onValueChange={onTestInputChange}
        editable={!readOnly}
        editorHook={editorHook}
        currentPath={[{ kind: 'StructField', value: inputName }]}
        diffs={[]}
      />
      {isContext && !readOnly && (
        <button
          className="context-var-reset-btn"
          onClick={resetToDefault}
          title={intl.formatMessage({ id: 'testEditor.resetToDefault' })}
        >
          <span className="codicon codicon-trash"></span>
        </button>
      )}
    </div>
  );
}

type Props = {
  test_inputs: TestInputs;
  tested_scope: ScopeDef;
  onTestInputsChange(newValue: TestInputs): void;
  readOnly?: boolean;
  /** Rendered next to an input's name (the recovery view's carry marks). */
  labelExtra?: (inputName: string) => ReactNode;
  /** Decorates any nested editor by path, as the diff highlighter does. */
  editorHook?: (editor: ReactElement, path: PathSegment[]) => ReactElement;
};

export default function TestInputsEditor(props: Props): ReactElement {
  const intl = useIntl();

  const editorItems: EditorItem[] = Array.from(props.test_inputs.entries()).map(
    ([inputName, testIo]) => {
      const isContext =
        props.tested_scope.inputs.get(inputName)?.is_context ?? false;

      function onTestInputChange(newValue: TestIo): void {
        props.onTestInputsChange(
          new Map([...props.test_inputs, [inputName, newValue]])
        );
      }

      const label = (
        <>
          {isContext ? (
            <span className="context-var-label">
              <span
                className="context-var-badge"
                title={intl.formatMessage({ id: 'testEditor.contextVarTitle' })}
                aria-hidden="true"
              >
                C
              </span>
              {inputName}
            </span>
          ) : (
            inputName
          )}
          {props.labelExtra?.(inputName)}
        </>
      );

      const rawValue = testIo.value?.value.value;
      const count =
        testIo.typ.kind === 'TArray' && rawValue?.kind === 'Array'
          ? rawValue.value.length
          : undefined;

      return {
        key: inputName,
        label,
        type: testIo.typ,
        count,
        unfilled: countUnsetIn(testIo.value?.value, testIo.typ),
        editor: (
          <InputField
            inputName={inputName}
            testIo={testIo}
            isContext={isContext}
            readOnly={props.readOnly}
            onTestInputChange={onTestInputChange}
            editorHook={props.editorHook}
          />
        ),
      };
    }
  );

  return (
    <div className="test-inputs data-card">
      <CompositeEditor
        items={editorItems}
        atomicElements={true}
        currentPath={[]}
      />
    </div>
  );
}
