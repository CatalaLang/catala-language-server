import { type ReactElement, useState } from 'react';
import { useIntl } from 'react-intl';
import type { TestInputs, TestIo, ScopeDef } from '../generated/catala_types';
import ValueEditor, {
  getDefaultValue,
  createRuntimeValue,
} from '../editors/ValueEditors';
import { CompositeEditor, type EditorItem } from '../editors/CompositeEditor';
import { confirm } from '../messaging/confirm';

type Props = {
  test_inputs: TestInputs;
  tested_scope: ScopeDef;
  onTestInputsChange(newValue: TestInputs): void;
};

export default function TestInputsEditor(props: Props): ReactElement {
  const intl = useIntl();

  // Context vars that already have a non-Unset value start in override mode
  const [overriding, setOverriding] = useState<Set<string>>(
    () =>
      new Set(
        Array.from(props.test_inputs.entries())
          .filter(([name, io]) => {
            const si = props.tested_scope.inputs.get(name);
            return si?.is_context && io.value?.value.value.kind !== 'Unset';
          })
          .map(([name]) => name)
      )
  );

  const editorItems: EditorItem[] = Array.from(props.test_inputs.entries()).map(
    ([inputName, testIo]) => {
      const isContext =
        props.tested_scope.inputs.get(inputName)?.is_context ?? false;
      const showPlaceholder = isContext && !overriding.has(inputName);

      function onTestInputChange(newValue: TestIo): void {
        props.onTestInputsChange(
          new Map([...props.test_inputs, [inputName, newValue]])
        );
      }

      function startOverride(): void {
        setOverriding((prev) => new Set([...prev, inputName]));
        onTestInputChange({
          ...testIo,
          value: { value: getDefaultValue(testIo.typ), pos: undefined },
        });
      }

      async function resetToDefault(): Promise<void> {
        const hasData = testIo.value?.value.value.kind !== 'Unset';
        if (hasData) {
          const confirmed = await confirm('ResetContextVar');
          if (!confirmed) return;
        }
        setOverriding((prev) => {
          const next = new Set(prev);
          next.delete(inputName);
          return next;
        });
        onTestInputChange({
          ...testIo,
          value: {
            value: createRuntimeValue({ kind: 'Unset' }),
            pos: undefined,
          },
        });
      }

      const label = isContext ? (
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
      );

      const editor = showPlaceholder ? (
        <div className="context-var-placeholder">
          <span className="context-var-default-text">
            {intl.formatMessage({
              id: 'testEditor.usingComputedDefault',
              defaultMessage: 'Using computed default',
            })}
          </span>
          <button className="button-action-dvp body-b3" onClick={startOverride}>
            {intl.formatMessage({
              id: 'testEditor.override',
              defaultMessage: 'Override',
            })}
          </button>
        </div>
      ) : (
        <div className={isContext ? 'context-var-editor' : undefined}>
          <ValueEditor
            testIO={testIo}
            onValueChange={onTestInputChange}
            currentPath={[{ kind: 'StructField', value: inputName }]}
            diffs={[]}
          />
          {isContext && (
            <button
              className="context-var-reset-btn"
              onClick={resetToDefault}
              title={intl.formatMessage({
                id: 'testEditor.resetToDefault',
                defaultMessage: 'Reset to computed default',
              })}
            >
              <span className="codicon codicon-trash"></span>
            </button>
          )}
        </div>
      );

      return {
        key: inputName,
        label,
        type: testIo.typ,
        editor,
      };
    }
  );

  return (
    <div className="test-inputs data-card">
      <CompositeEditor items={editorItems} atomicElements={true} />
    </div>
  );
}
