// Editors for a single value type (grouped with a factory function)

import type { ReactElement } from 'react';
import type { TestIo } from './generated/test_case';
import { assertUnreachable } from './util';

type Props = {
  testIO: TestIo;
  onValueChange(newValue: TestIo): void;
};

export default function ValueEditor(props: Props): ReactElement {
  const typ = props.testIO.typ;
  switch (typ.kind) {
    case 'TInt':
      // hmmm... much "value" nesting and use of type coercion, we might do better
      return (
        <IntEditor
          value={props.testIO.value?.value.value as number}
          onValueChange={(newValue: number) => {
            props.onValueChange({
              typ,
              value: {
                value: {
                  kind: 'Integer',
                  value: newValue,
                },
              },
            });
          }}
        />
      );
    case 'TBool':
      return (
        <BoolEditor
          value={props.testIO.value?.value.value as boolean}
          onValueChange={(newValue: boolean) => {
            props.onValueChange({
              typ,
              value: {
                value: {
                  kind: 'Bool',
                  value: newValue,
                },
              },
            });
          }}
        />
      );
    case 'TRat':
    case 'TMoney':
    case 'TDate':
    case 'TDuration':
    case 'TTuple':
    case 'TStruct':
    case 'TEnum':
    case 'TOption':
    case 'TArray':
      return <i>Unimplemented Editor</i>;
    default:
      assertUnreachable(typ);
  }
}

type IntEditorProps = {
  value?: number; //initial value, may not exist
  onValueChange(newValue: number /* int */): void;
};

function IntEditor(props: IntEditorProps): ReactElement {
  return (
    <input
      type="number"
      step="1"
      value={props?.value}
      onChange={(evt) => {
        props.onValueChange(Number(evt.target.value));
      }}
    ></input>
  );
}

type BoolEditorProps = {
  value?: boolean; // initial value, may not exist
  onValueChange(newValue: boolean): void;
};

function BoolEditor(props: BoolEditorProps): ReactElement {
  return (
    <select
      value={props.value?.toString() ?? ''}
      onChange={(evt) => {
        props.onValueChange(evt.target.value === 'true');
      }}
    >
      <option value="false">false</option>
      <option value="true">true</option>
    </select>
  );
}
