// Editors for a single value type (grouped with a factory function)

import { type ReactElement, useState, useEffect } from 'react';
import { useDebounceValidation } from './ValidationUtils';
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
  const initialValue = props.value !== undefined ? String(props.value) : '';
  const [inputValue, handleInputChange] = useDebounceValidation<number>(
    initialValue,
    props.onValueChange,
    Number,
    300
  );

  // Handle input changes from the UI
  const handleChange = (evt: React.ChangeEvent<HTMLInputElement>) => {
    const newValue = evt.target.value;

    // Check if it's a valid integer
    // Allow empty string and minus sign during typing
    const isValidInteger = /^-?\d+$/.test(newValue);

    // Pass to our debounced handler
    handleInputChange(newValue, isValidInteger);
  };

  // Determine if input is valid
  const isValid = /^-?\d+$/.test(inputValue);

  return (
    <div className="value-editor">
      <input
        type="text"
        inputMode="numeric"
        pattern="-?[0-9]*"
        value={inputValue}
        onChange={handleChange}
        className={isValid ? '' : 'invalid'}
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
  const initialValue = props.value !== undefined ? String(props.value) : '';
  const [inputValue, handleInputChange] = useDebounceValidation<number>(
    initialValue,
    props.onValueChange,
    Number,
    300
  );

  // Handle input changes from the UI
  const handleChange = (evt: React.ChangeEvent<HTMLInputElement>) => {
    const newValue = evt.target.value;

    // Check if it's a valid decimal number
    // Allow empty string, minus sign, and partial decimal inputs during typing
    const isValidDecimal = /^-?\d*\.?\d*$/.test(newValue);

    // Only consider complete numbers as valid for updating the value
    const isCompleteNumber = /^-?\d+(\.\d+)?$/.test(newValue);

    // Pass to our debounced handler
    handleInputChange(newValue, isValidDecimal && isCompleteNumber);
  };

  // Determine if input is valid
  const isValid = /^-?\d+(\.\d+)?$/.test(inputValue);

  return (
    <div className="value-editor">
      <input
        type="text"
        inputMode="decimal"
        pattern="-?\d+(\.\d+)?"
        value={inputValue}
        onChange={handleChange}
        className={isValid ? '' : 'invalid'}
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

function isPrefix(partial: string, complete: string): boolean {
  // Check if partial is a prefix of complete, considering decimal places
  const partialParts = partial.split('.');
  const completeParts = complete.split('.');

  // Compare dollar parts
  if (!completeParts[0].startsWith(partialParts[0])) return false;

  // If partial has decimal, check cents prefix
  if (partialParts[1] !== undefined) {
    const partialCents = partialParts[1];
    const completeCents = completeParts[1] || '00';
    return completeCents.startsWith(partialCents);
  }

  return true;
}

function validateMoneyInput(input: string): { valid: boolean; cents: number } {
  // Default to 0 when empty
  if (input === '') return { valid: true, cents: 0 };

  // Allow numbers with optional decimal point
  const moneyPattern = /^\d*\.?\d*$/;
  if (!moneyPattern.test(input)) return { valid: false, cents: 0 };

  // Split into dollars and cents
  const [dollars, cents] = input.split('.');

  // Calculate total cents
  const dollarsCents = parseInt(dollars || '0', 10) * 100;
  const partialCents = parseInt((cents || '00').padEnd(2, '0').slice(0, 2), 10);

  return {
    valid: true,
    cents: dollarsCents + partialCents,
  };
}

function MoneyEditor(props: MoneyEditorProps): ReactElement {
  const [inputValue, setInputValue] = useState(
    props.value ? (props.value / 100).toFixed(2) : ''
  );
  const [lastValidValue, setLastValidValue] = useState(props.value || 0);

  // Handle external value changes (e.g., from parent)
  useEffect(() => {
    const formattedValue = (props.value || 0) / 100;
    const formattedString = formattedValue.toFixed(2);

    // Only update if the current input isn't a valid prefix
    if (!isPrefix(inputValue, formattedString)) {
      setInputValue(formattedString);
    }
  }, [props.value]);

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const newInput = e.target.value;
    const { valid, cents } = validateMoneyInput(newInput);

    setInputValue(newInput);

    if (valid) {
      setLastValidValue(cents);
      props.onValueChange(cents); // Always propagate a valid number (minimum 0)
    }
  };

  const handleBlur = () => {
    // On blur, format the value properly only if input isn't already valid
    const formatted = (lastValidValue / 100).toFixed(2);
    if (!isPrefix(inputValue, formatted)) {
      setInputValue(formatted);
    }
  };

  return (
    <div className="value-editor money-editor">
      <div className="money-input-container">
        <input
          type="text"
          inputMode="decimal"
          pattern="\d*\.?\d*"
          value={inputValue}
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
