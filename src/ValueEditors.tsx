// Editors for a single value type (grouped with a factory function)

import { type ReactElement, useState, useEffect } from 'react';
import type {
  TestIo,
  StructDeclaration,
  RuntimeValue,
} from './generated/test_case';
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
    case 'TStruct':
      return (
        <StructEditor
          structDeclaration={typ.value}
          value={
            props.testIO.value?.value.value as [
              StructDeclaration,
              Map<string, RuntimeValue>,
            ]
          }
          onValueChange={(newValue) => {
            props.onValueChange({
              typ,
              value: {
                value: {
                  kind: 'Struct',
                  value: newValue,
                },
              },
            });
          }}
        />
      );
    case 'TRat':
      return (
        <RatEditor
          value={props.testIO.value?.value.value as number}
          onValueChange={(newValue: number) => {
            props.onValueChange({
              typ,
              value: {
                value: {
                  kind: 'Decimal',
                  value: newValue,
                },
              },
            });
          }}
        />
      );
    case 'TMoney':
    case 'TDate':
      return (
        <DateEditor
          value={
            props.testIO.value?.value.value as {
              year: number;
              month: number;
              day: number;
            }
          }
          onValueChange={(newValue: {
            year: number;
            month: number;
            day: number;
          }) => {
            props.onValueChange({
              typ,
              value: {
                value: {
                  kind: 'Date',
                  value: newValue,
                },
              },
            });
          }}
        />
      );
    case 'TDuration':
    case 'TTuple':
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
    <div className="value-editor">
      <input
        type="number"
        step="1"
        value={props?.value}
        onChange={(evt) => {
          props.onValueChange(Number(evt.target.value));
        }}
      />
    </div>
  );
}

type DateEditorProps = {
  value?: { year: number; month: number; day: number };
  onValueChange(newValue: { year: number; month: number; day: number }): void;
};

function DateEditor(props: DateEditorProps): ReactElement {
  const formatDate = (date: {
    year: number;
    month: number;
    day: number;
  }): string => {
    return `${date.year}-${String(date.month).padStart(2, '0')}-${String(date.day).padStart(2, '0')}`;
  };

  const [internalValue, setInternalValue] = useState(
    props.value ? formatDate(props.value) : ''
  );

  const parseDate = (
    dateString: string
  ): { year: number; month: number; day: number } | null => {
    const [year, month, day] = dateString.split('-').map(Number);
    if (isNaN(year) || isNaN(month) || isNaN(day)) {
      return null;
    }
    return { year, month, day };
  };

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    setInternalValue(event.target.value);
  };

  const handleBlur = () => {
    validateAndUpdate();
  };

  const handleKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key === 'Enter') {
      validateAndUpdate();
    }
  };

  const validateAndUpdate = () => {
    const newDate = parseDate(internalValue);
    if (newDate) {
      props.onValueChange(newDate);
    } else {
      // If invalid, revert to the last valid value
      setInternalValue(props.value ? formatDate(props.value) : '');
    }
  };

  useEffect(() => {
    if (props.value) {
      setInternalValue(formatDate(props.value));
    }
  }, [props.value]);

  return (
    <div className="value-editor">
      <input
        type="date"
        value={internalValue}
        onChange={handleChange}
        onBlur={handleBlur}
        onKeyDown={handleKeyDown}
      />
    </div>
  );
}

type RatEditorProps = {
  value?: number; // initial value, may not exist
  onValueChange(newValue: number): void;
};

function RatEditor(props: RatEditorProps): ReactElement {
  return (
    <div className="value-editor">
      <input
        type="number"
        value={props.value}
        onChange={(evt) => {
          props.onValueChange(Number(evt.target.value));
        }}
      />
    </div>
  );
}

type BoolEditorProps = {
  value?: boolean; // initial value, may not exist
  onValueChange(newValue: boolean): void;
};

function BoolEditor(props: BoolEditorProps): ReactElement {
  return (
    <div className="value-editor">
      <select
        value={props.value?.toString() ?? ''}
        onChange={(evt) => {
          props.onValueChange(evt.target.value === 'true');
        }}
      >
        <option value="false">false</option>
        <option value="true">true</option>
      </select>
    </div>
  );
}

type StructEditorProps = {
  structDeclaration: StructDeclaration;
  value?: [StructDeclaration, Map<string, RuntimeValue>];
  onValueChange(newValue: [StructDeclaration, Map<string, RuntimeValue>]): void;
};

function StructEditor(props: StructEditorProps): ReactElement {
  const { structDeclaration, value, onValueChange } = props;
  const fields = structDeclaration.fields;

  const handleFieldChange = (fieldName: string, fieldValue: RuntimeValue) => {
    const newMap = new Map(value?.[1] || []);
    newMap.set(fieldName, fieldValue);
    onValueChange([structDeclaration, newMap]);
  };

  return (
    <div className="struct-editor">
      <table>
        <thead>
          <tr>
            <th colSpan={2} className="struct-name">
              {structDeclaration.struct_name}
            </th>
          </tr>
        </thead>
        <tbody>
          {Array.from(fields.entries()).map(([fieldName, fieldType]) => (
            <tr key={fieldName}>
              <td className="field-name identifier">{fieldName}</td>
              <td>
                <ValueEditor
                  testIO={{
                    typ: fieldType,
                    value: value?.[1].get(fieldName)
                      ? { value: value[1].get(fieldName)! }
                      : undefined,
                  }}
                  onValueChange={(newValue) =>
                    handleFieldChange(fieldName, newValue.value!.value)
                  }
                />
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
