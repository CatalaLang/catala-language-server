// Editors for a single value type (grouped with a factory function)

import { type ReactElement, useState, useEffect, useId } from 'react';
import { useValidatedInput } from './hooks/useValidatedInput';
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
  clearValidationErrors,
  validateNumeric,
  validateMoney,
} from './validation';

type Props = {
  testIO: TestIo;
  onValueChange(newValue: TestIo): void;
  validationId?: string; // Optional ID for validation tracking
};

import { ValidationErrorDisplay } from './components/ValidationErrorDisplay';

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

  const {
    inputValue,
    errors,
    handleChange,
    handleBlur,
    handleKeyDown,
    inputRef,
  } = useValidatedInput({
    initialValue: props.value,
    validator: (value) => validateNumeric(value).errors,
    onValidChange: props.onValueChange,
    validationId,
    parseValue: Number,
  });

  return (
    <div className="value-editor">
      <input
        ref={inputRef}
        type="number"
        step="1"
        min={0}
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

  const formatDate = (date: {
    year: number;
    month: number;
    day: number;
  }): string => {
    return `${date.year}-${String(date.month).padStart(2, '0')}-${String(
      date.day
    ).padStart(2, '0')}`;
  };

  const parseDate = (
    dateString: string
  ): { year: number; month: number; day: number } => {
    const [year, month, day] = dateString.split('-').map(Number);
    return { year, month, day };
  };

  // Custom validator for dates
  const validateDateInput = (value: string): ValidationError[] => {
    if (value.trim() === '') {
      return [{ message: 'Date cannot be empty' }];
    }
    return [];
  };

  const {
    inputValue: internalValue,
    errors,
    handleKeyDown,
    inputRef,
    handleBlur,
  } = useValidatedInput({
    initialValue: props.value ? formatDate(props.value) : '',
    validator: validateDateInput,
    onValidChange: (value) => {
      const date = parseDate(value);
      props.onValueChange(date);
    },
    validationId,
    parseValue: (value) => value,
  });

  // Custom change handler for dates to update immediately
  const handleChange = (event: React.ChangeEvent<HTMLInputElement>): void => {
    const newValue = event.target.value;

    // Clear errors
    if (errors.length > 0) {
      clearValidationErrors(validationId);
    }

    // HTML5 date input provides valid dates, so we can update immediately
    if (newValue) {
      const date = parseDate(newValue);
      props.onValueChange(date);
    }
  };

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

  const {
    inputValue,
    errors,
    handleChange,
    handleBlur,
    handleKeyDown,
    inputRef,
  } = useValidatedInput({
    initialValue: props.value,
    validator: (value) => validateNumeric(value).errors,
    onValidChange: props.onValueChange,
    validationId,
    parseValue: Number,
  });

  return (
    <div className="value-editor">
      <input
        ref={inputRef}
        type="number"
        step="any"
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
        onChange={(evt): void => {
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
    return (): void => {
      if (props.validationId) {
        clearValidationErrors(props.validationId);
      }
    };
  }, [props.validationId]);
  const [years, setYears] = useState(props.value?.years ?? 0);
  const [months, setMonths] = useState(props.value?.months ?? 0);
  const [days, setDays] = useState(props.value?.days ?? 0);

  useEffect((): void => {
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
            step="1"
            value={years}
            onChange={(e) => handleChange('years', parseInt(e.target.value))}
          />
        </label>
        <label>
          Months:
          <input
            type="number"
            min="0"
            step="1"
            max="11"
            value={months}
            onChange={(e) => handleChange('months', parseInt(e.target.value))}
          />
        </label>
        <label>
          Days:
          <input
            type="number"
            min="0"
            step="1"
            max="30"
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

  const centsToDisplayValue = (cents: number | undefined): string =>
    cents !== undefined ? (cents / 100).toFixed(2) : '';

  const parseMoneyValue = (value: string): number => {
    const numericValue = parseFloat(value);
    return Math.round(numericValue * 100);
  };

  const {
    inputValue: displayValue,
    errors,
    handleChange,
    handleBlur: handleRawBlur,
    handleKeyDown,
    inputRef,
  } = useValidatedInput({
    initialValue: centsToDisplayValue(props.value),
    validator: (value) => validateMoney(value).errors,
    onValidChange: (value) => props.onValueChange(parseMoneyValue(value)),
    validationId,
    parseValue: (value) => value, // We'll handle the parsing in onValidChange
  });

  // Custom blur handler to format the value
  const handleBlur = (): void => {
    handleRawBlur();

    // If no errors, format the value
    if (errors.length === 0 && displayValue.trim() !== '') {
      const numericValue = parseFloat(displayValue);
      if (!isNaN(numericValue)) {
        const centsValue = Math.round(numericValue * 100);
        if (centsValue >= 0) {
          props.onValueChange(centsValue);
        }
      }
    }
  };

  return (
    <div className="value-editor money-editor">
      <div className="money-input-container">
        <input
          ref={inputRef}
          type="number"
          step="0.01"
          min="0"
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
    return (): void => {
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
    const newMap = new Map(value?.[1] ?? []);
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
    return (): void => {
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

  const handleUpdate = (index: number, newValue: RuntimeValue): void => {
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
    return (): void => {
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
