// Editors for a single value type (grouped with a factory function)

import { type ReactElement, useState, useEffect, useRef, useId } from 'react';
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
import type { ValidationError } from './validation';
import {
  generateValidationId,
  registerValidationErrors,
  clearValidationErrors,
  validateNumeric,
  validateDate,
  validateMoney,
} from './validation';

type Props = {
  testIO: TestIo;
  onValueChange(newValue: TestIo): void;
  validationId?: string; // Optional ID for validation tracking
};

// Component to display validation errors
function ValidationErrorDisplay({
  errors,
}: {
  errors: ValidationError[];
}): ReactElement | null {
  if (errors.length === 0) return null;

  return (
    <div className="validation-errors">
      {errors.map((error, index) => (
        <div
          key={index}
          className={`validation-error validation-${error.severity}`}
        >
          {error.message}
        </div>
      ))}
    </div>
  );
}

export default function ValueEditor(props: Props): ReactElement {
  const typ = props.testIO.typ;
  const validationId =
    props.validationId ?? generateValidationId(`value_${typ.kind}`);
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
          validationId={`${validationId}_int`}
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
          validationId={`${validationId}_bool`}
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
          validationId={`${validationId}_struct`}
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
          validationId={`${validationId}_rat`}
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
          validationId={`${validationId}_date`}
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
          validationId={`${validationId}_duration`}
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
          validationId={`${validationId}_money`}
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
          validationId={`${validationId}_enum`}
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
          validationId={`${validationId}_array`}
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
  validationId?: string;
};

function IntEditor(props: IntEditorProps): ReactElement {
  const defaultId = useId();
  const validationId = props.validationId ?? `int_${defaultId}`;
  const [inputValue, setInputValue] = useState(props.value?.toString() ?? '');
  const [errors, setErrors] = useState<ValidationError[]>([]);
  const inputRef = useRef<HTMLInputElement>(null);

  // Update internal state when props change
  useEffect(() => {
    if (props.value !== undefined) {
      setInputValue(props.value.toString());
    }
  }, [props.value]);

  const validate = (value: string): ValidationError[] => {
    const result = validateNumeric(value, { isInteger: true });
    return result.errors;
  };

  const handleChange = (evt: React.ChangeEvent<HTMLInputElement>): void => {
    const newValue = evt.target.value;
    setInputValue(newValue);

    // Clear errors during typing for better UX
    if (errors.length > 0) {
      setErrors([]);
      clearValidationErrors(validationId);
    }
  };

  const handleBlur = (): void => {
    const validationErrors = validate(inputValue);
    setErrors(validationErrors);
    registerValidationErrors(validationId, validationErrors);

    // If valid, update the value
    if (validationErrors.length === 0 && inputValue.trim() !== '') {
      props.onValueChange(Number(inputValue));
    }
  };

  const handleKeyDown = (evt: React.KeyboardEvent<HTMLInputElement>): void => {
    if (evt.key === 'Enter') {
      inputRef.current?.blur();
    }
  };

  return (
    <div className="value-editor">
      <input
        ref={inputRef}
        type="text"
        inputMode="numeric"
        value={inputValue}
        onChange={handleChange}
        onBlur={handleBlur}
        onKeyDown={handleKeyDown}
        className={errors.length > 0 ? 'has-validation-error' : ''}
      />
      <ValidationErrorDisplay errors={errors} />
    </div>
  );
}

type DateEditorProps = {
  value?: { year: number; month: number; day: number };
  onValueChange(newValue: { year: number; month: number; day: number }): void;
  validationId?: string;
};

function DateEditor(props: DateEditorProps): ReactElement {
  const defaultId = useId();
  const validationId = props.validationId ?? `date_${defaultId}`;
  const inputRef = useRef<HTMLInputElement>(null);
  const [errors, setErrors] = useState<ValidationError[]>([]);

  const formatDate = (date: {
    year: number;
    month: number;
    day: number;
  }): string => {
    return `${date.year}-${String(date.month).padStart(2, '0')}-${String(
      date.day
    ).padStart(2, '0')}`;
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

  const validate = (value: string): ValidationError[] => {
    const result = validateDate(value);
    return result.errors;
  };

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>): void => {
    setInternalValue(event.target.value);

    // Clear errors during typing for better UX
    if (errors.length > 0) {
      setErrors([]);
      clearValidationErrors(validationId);
    }
  };

  const handleBlur = (): void => {
    const validationErrors = validate(internalValue);
    setErrors(validationErrors);
    registerValidationErrors(validationId, validationErrors);

    if (validationErrors.length === 0) {
      validateAndUpdate();
    }
  };

  const handleKeyDown = (
    event: React.KeyboardEvent<HTMLInputElement>
  ): void => {
    if (event.key === 'Enter') {
      inputRef.current?.blur();
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
        ref={inputRef}
        type="date"
        value={internalValue}
        onChange={handleChange}
        onBlur={handleBlur}
        onKeyDown={handleKeyDown}
        className={errors.length > 0 ? 'has-validation-error' : ''}
      />
      <ValidationErrorDisplay errors={errors} />
    </div>
  );
}

type RatEditorProps = {
  value?: number; // initial value, may not exist
  onValueChange(newValue: number): void;
  validationId?: string;
};

function RatEditor(props: RatEditorProps): ReactElement {
  const defaultId = useId();
  const validationId = props.validationId ?? `rat_${defaultId}`;
  const [inputValue, setInputValue] = useState(props.value?.toString() ?? '');
  const [errors, setErrors] = useState<ValidationError[]>([]);
  const inputRef = useRef<HTMLInputElement>(null);

  // Update internal state when props change
  useEffect(() => {
    if (props.value !== undefined) {
      setInputValue(props.value.toString());
    }
  }, [props.value]);

  const validate = (value: string): ValidationError[] => {
    const result = validateNumeric(value);
    return result.errors;
  };

  const handleChange = (evt: React.ChangeEvent<HTMLInputElement>): void => {
    const newValue = evt.target.value;
    setInputValue(newValue);

    // Clear errors during typing for better UX
    if (errors.length > 0) {
      setErrors([]);
      clearValidationErrors(validationId);
    }
  };

  const handleBlur = (): void => {
    const validationErrors = validate(inputValue);
    setErrors(validationErrors);
    registerValidationErrors(validationId, validationErrors);

    // If valid, update the value
    if (validationErrors.length === 0 && inputValue.trim() !== '') {
      props.onValueChange(Number(inputValue));
    }
  };

  const handleKeyDown = (evt: React.KeyboardEvent<HTMLInputElement>): void => {
    if (evt.key === 'Enter') {
      inputRef.current?.blur();
    }
  };

  return (
    <div className="value-editor">
      <input
        ref={inputRef}
        type="text"
        inputMode="numeric"
        value={inputValue}
        onChange={handleChange}
        onBlur={handleBlur}
        onKeyDown={handleKeyDown}
        className={errors.length > 0 ? 'has-validation-error' : ''}
      />
      <ValidationErrorDisplay errors={errors} />
    </div>
  );
}

type BoolEditorProps = {
  value?: boolean; // initial value, may not exist
  onValueChange(newValue: boolean): void;
  validationId?: string;
};

function BoolEditor(props: BoolEditorProps): ReactElement {
  // Boolean selects don't typically need validation as they're constrained
  // But we'll clear any validation errors when the component unmounts
  useEffect(() => {
    return () => {
      if (props.validationId) {
        clearValidationErrors(props.validationId);
      }
    };
  }, [props.validationId]);

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
  validationId?: string;
};

function DurationEditor(props: DurationEditorProps): ReactElement {
  // Clean up validation on unmount
  useEffect(() => {
    return () => {
      if (props.validationId) {
        clearValidationErrors(props.validationId);
      }
    };
  }, [props.validationId]);
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
  validationId?: string;
};

function MoneyEditor(props: MoneyEditorProps): ReactElement {
  const defaultId = useId();
  const validationId = props.validationId ?? `money_${defaultId}`;
  const inputRef = useRef<HTMLInputElement>(null);
  const [errors, setErrors] = useState<ValidationError[]>([]);

  const centsToDisplayValue = (cents: number | undefined): string =>
    cents !== undefined ? (cents / 100).toFixed(2) : '';

  const [displayValue, setDisplayValue] = useState(
    centsToDisplayValue(props.value)
  );

  useEffect(() => {
    setDisplayValue(centsToDisplayValue(props.value));
  }, [props.value]);

  const validate = (value: string): ValidationError[] => {
    const result = validateMoney(value);
    return result.errors;
  };

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>): void => {
    setDisplayValue(event.target.value);

    // Clear errors during typing for better UX
    if (errors.length > 0) {
      setErrors([]);
      clearValidationErrors(validationId);
    }
  };

  const handleBlur = (): void => {
    const validationErrors = validate(displayValue);
    setErrors(validationErrors);
    registerValidationErrors(validationId, validationErrors);

    if (validationErrors.length === 0) {
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
    }
  };

  const handleKeyDown = (evt: React.KeyboardEvent<HTMLInputElement>): void => {
    if (evt.key === 'Enter') {
      inputRef.current?.blur();
    }
  };

  return (
    <div className="value-editor money-editor">
      <div className="money-input-container">
        <input
          ref={inputRef}
          type="text"
          value={displayValue}
          onChange={handleChange}
          onBlur={handleBlur}
          onKeyDown={handleKeyDown}
          placeholder="0.00"
          className={errors.length > 0 ? 'has-validation-error' : ''}
        />
        <ValidationErrorDisplay errors={errors} />
      </div>
    </div>
  );
}

type StructEditorProps = {
  structDeclaration: StructDeclaration;
  value?: [StructDeclaration, Map<string, RuntimeValue>];
  onValueChange(newValue: [StructDeclaration, Map<string, RuntimeValue>]): void;
  validationId?: string;
};

function StructEditor(props: StructEditorProps): ReactElement {
  // Clean up validation on unmount
  useEffect(() => {
    return () => {
      if (props.validationId) {
        clearValidationErrors(props.validationId);
      }
    };
  }, [props.validationId]);
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
  validationId?: string;
};

type ArrayEditorProps = {
  elementType: Typ;
  value?: RuntimeValue[];
  onValueChange(newValue: RuntimeValue[]): void;
  validationId?: string;
};

function ArrayEditor(props: ArrayEditorProps): ReactElement {
  // Clean up validation on unmount
  useEffect(() => {
    return () => {
      if (props.validationId) {
        clearValidationErrors(props.validationId);
      }
    };
  }, [props.validationId]);
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
  // Clean up validation on unmount
  useEffect(() => {
    return () => {
      if (props.validationId) {
        clearValidationErrors(props.validationId);
      }
    };
  }, [props.validationId]);
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
