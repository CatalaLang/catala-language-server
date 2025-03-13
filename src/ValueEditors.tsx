// Editors for a single value type (grouped with a factory function)

import { type ReactElement, useState, useEffect } from 'react';
import Row from './Row';
import type {
  Option,
  TestIo,
  StructDeclaration,
  RuntimeValue,
  Duration,
  EnumDeclaration,
  Typ,
} from './generated/test_case';
import { assertUnreachable } from './util';
import { getDefaultValue } from './defaults';

type Props = {
  testIO: TestIo;
  onValueChange(newValue: TestIo): void;
};

export function isCollapsible(typ: Typ): boolean {
  return typ.kind === 'TStruct' || typ.kind === 'TArray';
}

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
      return (
        <DurationEditor
          value={props.testIO.value?.value.value as Duration}
          onValueChange={(newValue: Duration) => {
            props.onValueChange({
              typ,
              value: {
                value: {
                  kind: 'Duration',
                  value: newValue,
                },
              },
            });
          }}
        />
      );
    case 'TMoney':
      return (
        <MoneyEditor
          value={props.testIO.value?.value.value as number}
          onValueChange={(newValue: number) => {
            props.onValueChange({
              typ,
              value: {
                value: {
                  kind: 'Money',
                  value: newValue,
                },
              },
            });
          }}
        />
      );
    case 'TEnum':
      return (
        <EnumEditor
          enumDeclaration={typ.value}
          onValueChange={(
            newCtor: string,
            newValue: Option<RuntimeValue>
          ): void => {
            props.onValueChange({
              typ,
              value: {
                value: {
                  kind: 'Enum',
                  value: [typ.value, [newCtor, newValue]],
                },
              },
            });
          }}
        />
      );
    case 'TArray':
      return (
        <ArrayEditor
          elementType={typ.value}
          value={props.testIO.value?.value.value as RuntimeValue[]}
          onValueChange={(newValue: RuntimeValue[]) => {
            props.onValueChange({
              typ,
              value: {
                value: {
                  kind: 'Array',
                  value: newValue,
                },
              },
            });
          }}
        />
      );
    case 'TTuple':
    case 'TOption':
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

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>): void => {
    setInternalValue(event.target.value);
  };

  const handleBlur = (): void => {
    validateAndUpdate();
  };

  const handleKeyDown = (
    event: React.KeyboardEvent<HTMLInputElement>
  ): void => {
    if (event.key === 'Enter') {
      validateAndUpdate();
    }
  };

  const validateAndUpdate = (): void => {
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

type DurationEditorProps = {
  value?: Duration;
  onValueChange(newValue: Duration): void;
};

function DurationEditor(props: DurationEditorProps): ReactElement {
  const [years, setYears] = useState(props.value?.years ?? 0);
  const [months, setMonths] = useState(props.value?.months ?? 0);
  const [days, setDays] = useState(props.value?.days ?? 0);

  useEffect(() => {
    if (props.value) {
      setYears(props.value.years);
      setMonths(props.value.months);
      setDays(props.value.days);
    }
  }, [props.value]);

  const handleChange = (
    field: 'years' | 'months' | 'days',
    value: number
  ): void => {
    const newValue = Math.max(0, value); // Ensure non-negative values
    switch (field) {
      case 'years':
        setYears(newValue);
        break;
      case 'months':
        setMonths(newValue);
        break;
      case 'days':
        setDays(newValue);
        break;
    }
    props.onValueChange({ years, months, days, [field]: newValue });
  };

  return (
    <div className="value-editor duration-editor">
      <div className="duration-fields">
        <label>
          Years:
          <input
            type="number"
            min="0"
            value={years}
            onChange={(e) => handleChange('years', parseInt(e.target.value))}
          />
        </label>
        <label>
          Months:
          <input
            type="number"
            min="0"
            value={months}
            onChange={(e) => handleChange('months', parseInt(e.target.value))}
          />
        </label>
        <label>
          Days:
          <input
            type="number"
            min="0"
            value={days}
            onChange={(e) => handleChange('days', parseInt(e.target.value))}
          />
        </label>
      </div>
    </div>
  );
}

type MoneyEditorProps = {
  value?: number; // initial value in cents, may not exist
  onValueChange(newValue: number): void;
};

function MoneyEditor(props: MoneyEditorProps): ReactElement {
  const centsToDisplayValue = (cents: number | undefined): string =>
    cents !== undefined ? (cents / 100).toFixed(2) : '';

  const [displayValue, setDisplayValue] = useState(
    centsToDisplayValue(props.value)
  );

  useEffect(() => {
    setDisplayValue(centsToDisplayValue(props.value));
  }, [props.value]);

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>): void => {
    setDisplayValue(event.target.value);
  };

  const handleBlur = (): void => {
    const numericValue = parseFloat(displayValue);
    if (!isNaN(numericValue)) {
      const centsValue = Math.round(numericValue * 100);
      if (centsValue >= 0) {
        props.onValueChange(centsValue);
        setDisplayValue(centsToDisplayValue(centsValue));
      } else {
        // Reset to the last valid value or empty string
        setDisplayValue(centsToDisplayValue(props.value));
      }
    } else {
      // Reset to the last valid value or empty string
      setDisplayValue(centsToDisplayValue(props.value));
    }
  };

  return (
    <div className="value-editor money-editor">
      <div className="money-input-container">
        <input
          type="text"
          value={displayValue}
          onChange={handleChange}
          onBlur={handleBlur}
          placeholder="0.00"
        />
      </div>
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

  const handleFieldChange = (
    fieldName: string,
    fieldValue: RuntimeValue
  ): void => {
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
            <Row key={fieldName} label={fieldName} className="field-name">
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
            </Row>
          ))}
        </tbody>
      </table>
    </div>
  );
}

type EnumEditorProps = {
  enumDeclaration: EnumDeclaration;
  value?: [EnumDeclaration, [string, Option<RuntimeValue>]];
  onValueChange(ctor: string, value: Option<RuntimeValue>): void;
};

type ArrayEditorProps = {
  elementType: Typ;
  value?: RuntimeValue[];
  onValueChange(newValue: RuntimeValue[]): void;
};

function ArrayEditor(props: ArrayEditorProps): ReactElement {
  const { elementType, value = [], onValueChange } = props;

  const handleAdd = (): void => {
    const newValue: RuntimeValue = getDefaultValue(elementType);
    onValueChange([...value, newValue]);
  };

  const handleUpdate = (index: number, newValue: RuntimeValue) => {
    const updatedArray = [...value];
    updatedArray[index] = newValue;
    onValueChange(updatedArray);
  };

  const handleDelete = (index: number): void => {
    const newArray = value.filter((_, i) => i !== index);
    onValueChange(newArray);
  };

  const handleMove = (fromIndex: number, toIndex: number): void => {
    const newArray = [...value];
    const [movedItem] = newArray.splice(fromIndex, 1);
    newArray.splice(toIndex, 0, movedItem);
    onValueChange(newArray);
  };

  return (
    <div className="array-editor">
      <div className="array-items">
        {value.map((item, index) => (
          <div key={index} className="array-item">
            <div className="array-item-controls">
              <button
                className="array-move-up"
                onClick={() => handleMove(index, index - 1)}
                disabled={index === 0}
              >
                ↑
              </button>
              <button
                className="array-move-down"
                onClick={() => handleMove(index, index + 1)}
                disabled={index === value.length - 1}
              >
                ↓
              </button>
              <button
                className="array-delete"
                onClick={() => handleDelete(index)}
              >
                ×
              </button>
            </div>
            <ValueEditor
              testIO={{
                typ: elementType,
                value: { value: item },
              }}
              onValueChange={(newValue) =>
                handleUpdate(index, newValue.value!.value)
              }
            />
          </div>
        ))}
      </div>
      <button className="array-add" onClick={handleAdd}>
        <span className="codicon codicon-add"></span>
        Add Item
      </button>
    </div>
  );
}

function runtimeValueToTestIo(typ: Typ, value: Option<RuntimeValue>): TestIo {
  return {
    typ: typ,
    ...(value != undefined && {
      value: value,
    }),
  };
}

function EnumEditor(props: EnumEditorProps): ReactElement {
  // Note that in Catala, an enum should always have at least 1 constructor
  // so dereferencing the first array element is valid
  const [currentCtor, setCurrentCtor] = useState(
    Array.from(props.enumDeclaration.constructors.keys())[0]
  );

  return (
    <div className="value-editor">
      <select
        onChange={(evt: React.ChangeEvent<HTMLSelectElement>): void => {
          const newCtor = evt.target.value;
          setCurrentCtor(newCtor);
          if (props.enumDeclaration.constructors.get(newCtor) === null) {
            props.onValueChange(newCtor, null);
          }
        }}
      >
        {Array.from(props.enumDeclaration.constructors.keys()).map(
          (optionName) => (
            <option key={optionName}>{optionName}</option>
          )
        )}
      </select>
      {props.enumDeclaration.constructors.get(currentCtor) && (
        <ValueEditor
          testIO={runtimeValueToTestIo(
            props.enumDeclaration.constructors.get(currentCtor)!.value,
            props.value?.[1][1] ?? null //TODO make this more legible?
          )}
          onValueChange={(newValue: TestIo): void => {
            props.onValueChange(currentCtor, newValue.value ?? null);
          }}
        />
      )}
    </div>
  );
}
