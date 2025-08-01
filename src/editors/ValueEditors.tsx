// Editors for a single value type (grouped with a factory function)

import { type ReactElement, useState, useEffect } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import type {
  Option,
  TestIo,
  StructDeclaration,
  RuntimeValue,
  RuntimeValueRaw,
  Duration,
  EnumDeclaration,
  Typ,
  ValueDef,
} from '../generated/test_case';
import { ArrayEditor } from './ArrayEditor';
import { assertUnreachable } from '../util';
import { getDefaultValue } from '../defaults';
import { CompositeEditor } from './CompositeEditor';

// Helper to create a RuntimeValue from a RuntimeValueRaw, preserving attrs
export function createRuntimeValue(
  newValueRaw: RuntimeValueRaw,
  originalRuntimeValue?: RuntimeValue
): RuntimeValue {
  return {
    value: newValueRaw,
    attrs: originalRuntimeValue?.attrs ?? [],
  };
}

type Props = {
  testIO: TestIo;
  onValueChange(newValue: TestIo): void;
};

export function isCollapsible(typ: Typ): boolean {
  return typ.kind === 'TStruct' || typ.kind === 'TArray';
}

export default function ValueEditor(props: Props): ReactElement {
  const { testIO, onValueChange } = props;
  const { typ, value: valueDef } = testIO;

  const handleValueChange = (newRuntimeValue: RuntimeValue): void => {
    onValueChange({
      typ,
      value: {
        value: newRuntimeValue,
      },
    });
  };

  switch (typ.kind) {
    case 'TInt':
      return (
        <IntEditor valueDef={valueDef} onValueChange={handleValueChange} />
      );
    case 'TBool':
      return (
        <BoolEditor valueDef={valueDef} onValueChange={handleValueChange} />
      );
    case 'TStruct':
      return (
        <StructEditor
          structDeclaration={typ.value}
          valueDef={valueDef}
          onValueChange={handleValueChange}
        />
      );
    case 'TRat':
      return (
        <RatEditor valueDef={valueDef} onValueChange={handleValueChange} />
      );
    case 'TDate':
      return (
        <DateEditor valueDef={valueDef} onValueChange={handleValueChange} />
      );
    case 'TDuration':
      return (
        <DurationEditor valueDef={valueDef} onValueChange={handleValueChange} />
      );
    case 'TMoney':
      return (
        <MoneyEditor valueDef={valueDef} onValueChange={handleValueChange} />
      );
    case 'TEnum':
      return (
        <EnumEditor
          enumDeclaration={typ.value}
          valueDef={valueDef}
          onValueChange={handleValueChange}
        />
      );
    case 'TArray':
      return (
        <ArrayEditor
          elementType={typ.value}
          valueDef={valueDef}
          onValueChange={handleValueChange}
        />
      );
    case 'TTuple':
    case 'TOption':
      return <i>Unimplemented Editor</i>;
    default:
      assertUnreachable(typ);
  }
}

const INT_PATTERN = /^-?\d+$/;

function isValidInt(value: string): boolean {
  return INT_PATTERN.test(value);
}

type IntEditorProps = {
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
};

function IntEditor(props: IntEditorProps): ReactElement {
  const runtimeValue = props.valueDef?.value;
  const initialValue =
    runtimeValue?.value.kind === 'Integer'
      ? runtimeValue.value.value
      : undefined;

  const [displayValue, setDisplayValue] = useState(
    initialValue?.toString() ?? ''
  );

  // Update display value if prop changes externally
  useEffect(() => {
    const newValue =
      runtimeValue?.value.kind === 'Integer'
        ? runtimeValue.value.value
        : undefined;
    setDisplayValue(newValue?.toString() ?? '');
  }, [runtimeValue]);

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>): void => {
    const valueStr = event.target.value;
    setDisplayValue(valueStr);

    // Only update if valid
    if (isValidInt(valueStr)) {
      const newValueRaw: RuntimeValueRaw = {
        kind: 'Integer',
        value: Number(valueStr),
      };
      props.onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
    }
  };

  const handleBlur = (): void => {
    if (!isValidInt(displayValue)) {
      // Reset to the last valid value or empty string
      const resetValue =
        runtimeValue?.value.kind === 'Integer'
          ? runtimeValue.value.value
          : undefined;
      setDisplayValue(resetValue?.toString() ?? '');
    }
  };

  return (
    <div className="value-editor int-editor">
      <input
        type="text"
        pattern={INT_PATTERN.source}
        required
        value={displayValue}
        onChange={handleChange}
        onBlur={handleBlur}
        className={displayValue && !isValidInt(displayValue) ? 'invalid' : ''}
        placeholder="0"
      />
    </div>
  );
}

type DateEditorProps = {
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
};

function DateEditor(props: DateEditorProps): ReactElement {
  const runtimeValue = props.valueDef?.value;
  const initialValue =
    runtimeValue?.value.kind === 'Date' ? runtimeValue.value.value : undefined;

  const formatDate = (
    date: { year: number; month: number; day: number } | undefined
  ): string => {
    if (!date) return '';
    return `${String(date.year).padStart(4, '0')}-${String(date.month).padStart(
      2,
      '0'
    )}-${String(date.day).padStart(2, '0')}`;
  };

  const [internalValue, setInternalValue] = useState(formatDate(initialValue));

  // Update display value if prop changes externally
  useEffect(() => {
    const newValue =
      runtimeValue?.value.kind === 'Date'
        ? runtimeValue.value.value
        : undefined;
    setInternalValue(formatDate(newValue));
  }, [runtimeValue]);

  const parseDate = (
    dateString: string
  ): { year: number; month: number; day: number } | null => {
    const [year, month, day] = dateString.split('-').map(Number);
    if (isNaN(year) || isNaN(month) || isNaN(day)) {
      return null;
    }
    if (year > 9999) {
      //catala date literals are coded on 4 digits
      return null;
    }
    return { year, month, day };
  };

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>): void => {
    const dateStr = event.target.value;
    setInternalValue(dateStr);

    const newDate = parseDate(dateStr);
    if (newDate) {
      const newValueRaw: RuntimeValueRaw = { kind: 'Date', value: newDate };
      props.onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
    }
  };

  // TODO: Add onBlur validation?

  return (
    <div className="value-editor">
      <input type="date" value={internalValue} onChange={handleChange} />
    </div>
  );
}

const RAT_PATTERN = /^-?\d+(\.\d*)?$/;

function isValidRat(value: string): boolean {
  return RAT_PATTERN.test(value);
}

type RatEditorProps = {
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
};

function RatEditor(props: RatEditorProps): ReactElement {
  const runtimeValue = props.valueDef?.value;
  const initialValue =
    runtimeValue?.value.kind === 'Decimal'
      ? runtimeValue.value.value
      : undefined;

  const [displayValue, setDisplayValue] = useState(
    initialValue?.toString() ?? ''
  );

  // Update display value if prop changes externally
  useEffect(() => {
    const newValue =
      runtimeValue?.value.kind === 'Decimal'
        ? runtimeValue.value.value
        : undefined;
    if (parseFloat(displayValue) == newValue) {
      // we're doing an equality check on floats here;
      // but its sole purpose is to preserve user input when
      // inputting a decimal separator, i.e. when
      // the user is typing '13.4' in the process of typing
      // '13.42' ; so the usefulness of this comparison is
      // to keep the user-supplied string to '13.' instead of '13'
      return;
    }
    setDisplayValue(newValue?.toString() ?? '');
  }, [runtimeValue]);

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>): void => {
    const valueStr = event.target.value;
    setDisplayValue(valueStr);

    // Only update if valid
    if (isValidRat(valueStr)) {
      const newValueRaw: RuntimeValueRaw = {
        kind: 'Decimal',
        value: Number(valueStr),
      };
      props.onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
    }
  };

  const handleBlur = (): void => {
    if (!isValidRat(displayValue)) {
      // Reset to the last valid value or empty string
      const resetValue =
        runtimeValue?.value.kind === 'Decimal'
          ? runtimeValue.value.value
          : undefined;
      setDisplayValue(resetValue?.toString() ?? '');
    }
  };

  return (
    <div className="value-editor rat-editor">
      <input
        type="text"
        pattern={RAT_PATTERN.source}
        required
        value={displayValue}
        onChange={handleChange}
        onBlur={handleBlur}
        className={displayValue && !isValidRat(displayValue) ? 'invalid' : ''}
        placeholder="0.0"
      />
    </div>
  );
}

type BoolEditorProps = {
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
};

function BoolEditor(props: BoolEditorProps): ReactElement {
  const runtimeValue = props.valueDef?.value;
  const currentValue =
    runtimeValue?.value.kind === 'Bool' ? runtimeValue.value.value : false; // Default to false

  const handleChange = (event: React.ChangeEvent<HTMLSelectElement>): void => {
    const boolValue = event.target.value === 'true';
    const newValueRaw: RuntimeValueRaw = { kind: 'Bool', value: boolValue };
    props.onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
  };

  return (
    <div className="value-editor">
      <select value={currentValue.toString()} onChange={handleChange}>
        <option value="false">
          <FormattedMessage id="false" />
        </option>
        <option value="true">
          <FormattedMessage id="true" />
        </option>
      </select>
    </div>
  );
}

type DurationEditorProps = {
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
};

function DurationEditor(props: DurationEditorProps): ReactElement {
  const runtimeValue = props.valueDef?.value;
  const initialValue =
    runtimeValue?.value.kind === 'Duration'
      ? runtimeValue.value.value
      : undefined;

  const [years, setYears] = useState(initialValue?.years ?? 0);
  const [months, setMonths] = useState(initialValue?.months ?? 0);
  const [days, setDays] = useState(initialValue?.days ?? 0);

  // Update state if prop changes externally
  useEffect(() => {
    const newValue =
      runtimeValue?.value.kind === 'Duration'
        ? runtimeValue.value.value
        : undefined;
    setYears(newValue?.years ?? 0);
    setMonths(newValue?.months ?? 0);
    setDays(newValue?.days ?? 0);
  }, [runtimeValue]);

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
    const newDuration: Duration = { years, months, days, [field]: newValue };
    const newValueRaw: RuntimeValueRaw = {
      kind: 'Duration',
      value: newDuration,
    };
    props.onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
  };

  return (
    <div className="value-editor duration-editor">
      <div className="duration-fields">
        <label>
          <FormattedMessage id="durationEditor.years" defaultMessage="Years:" />
          <input
            type="number"
            min="0"
            value={years}
            onChange={(e) => handleChange('years', parseInt(e.target.value))}
          />
        </label>
        <label>
          <FormattedMessage
            id="durationEditor.months"
            defaultMessage="Months:"
          />
          <input
            type="number"
            min="0"
            value={months}
            onChange={(e) => handleChange('months', parseInt(e.target.value))}
          />
        </label>
        <label>
          <FormattedMessage id="durationEditor.days" defaultMessage="Days:" />
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

const MONEY_PATTERN = /^\d+(\.\d{1,2})?$/;

function isValidMoney(value: string): boolean {
  return MONEY_PATTERN.test(value);
}

type MoneyEditorProps = {
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
};

function MoneyEditor(props: MoneyEditorProps): ReactElement {
  const intl = useIntl();
  const runtimeValue = props.valueDef?.value;
  const initialValue = // in cents
    runtimeValue?.value.kind === 'Money' ? runtimeValue.value.value : undefined;

  const centsToDisplayValue = (cents: number | undefined): string => {
    if (cents === undefined) {
      return '';
    }
    if (cents % 100 === 0) {
      return String(cents / 100);
    }
    if (cents % 10 === 0) {
      return (cents / 100).toFixed(1);
    }
    return (cents / 100).toFixed(2);
  };

  const [displayValue, setDisplayValue] = useState(
    centsToDisplayValue(initialValue)
  );

  // Update display value if prop changes externally
  useEffect(() => {
    const newValue =
      runtimeValue?.value.kind === 'Money'
        ? runtimeValue.value.value
        : undefined;
    setDisplayValue(centsToDisplayValue(newValue));
  }, [runtimeValue]);

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>): void => {
    const valueStr = event.target.value;
    setDisplayValue(valueStr);

    // Only update if valid
    if (isValidMoney(valueStr)) {
      const centsValue = Math.round(parseFloat(valueStr) * 100);
      const newValueRaw: RuntimeValueRaw = {
        kind: 'Money',
        value: centsValue,
      };
      props.onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
    }
  };

  // Determine currency style based on locale
  const locale = intl.locale;
  const currencyClass = locale.startsWith('fr')
    ? 'currency-eur'
    : 'currency-usd';

  return (
    <div className={`value-editor money-editor ${currencyClass}`}>
      <input
        type="text"
        pattern={MONEY_PATTERN.source}
        required
        value={displayValue}
        onChange={handleChange}
        className={displayValue && !isValidMoney(displayValue) ? 'invalid' : ''}
        placeholder="0.00"
      />
    </div>
  );
}

type StructEditorProps = {
  structDeclaration: StructDeclaration;
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
};

function StructEditor(props: StructEditorProps): ReactElement {
  const { structDeclaration, valueDef, onValueChange } = props;
  const runtimeValue = valueDef?.value;
  const fields = structDeclaration.fields;
  const currentStructData =
    runtimeValue?.value.kind === 'Struct'
      ? runtimeValue.value.value
      : undefined;
  const currentMap = currentStructData?.[1] ?? new Map<string, RuntimeValue>();

  const handleFieldChange = (
    fieldName: string,
    fieldRuntimeValue: RuntimeValue // Note: This is the full RuntimeValue for the field
  ): void => {
    const newMap = new Map(currentMap);
    newMap.set(fieldName, fieldRuntimeValue);
    const newValueRaw: RuntimeValueRaw = {
      kind: 'Struct',
      value: [structDeclaration, newMap],
    };
    onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
  };

  // Create editor items for CompositeEditor
  const editorItems = Array.from(fields.entries()).map(
    ([fieldName, fieldType]) => {
      return {
        key: fieldName,
        label: fieldName,
        type: fieldType,
        editor: (
          <ValueEditor
            testIO={{
              typ: fieldType,
              value: currentMap.get(fieldName)
                ? { value: currentMap.get(fieldName)! }
                : undefined,
            }}
            onValueChange={(newFieldTestIo) => {
              if (newFieldTestIo.value) {
                handleFieldChange(fieldName, newFieldTestIo.value.value);
              }
            }}
          />
        ),
      };
    }
  );

  return (
    <div className="struct-editor struct-container">
      <CompositeEditor items={editorItems} />
    </div>
  );
}

type EnumEditorProps = {
  enumDeclaration: EnumDeclaration;
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
};

function EnumEditor(props: EnumEditorProps): ReactElement {
  const { enumDeclaration, valueDef, onValueChange } = props;
  const runtimeValue = valueDef?.value;
  const currentEnumData =
    runtimeValue?.value.kind === 'Enum' ? runtimeValue.value.value : undefined;

  // Note that in Catala, an enum should always have at least 1 constructor
  // so dereferencing the first array element is valid
  const defaultCtor = Array.from(enumDeclaration.constructors.keys())[0];
  const currentCtor = currentEnumData ? currentEnumData[1][0] : defaultCtor;
  const currentPayload = currentEnumData ? currentEnumData[1][1] : null;
  const currentCtorType = enumDeclaration.constructors.get(currentCtor); // Option<Typ>

  const handleCtorChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ): void => {
    const newCtor = event.target.value;
    const newCtorType = enumDeclaration.constructors.get(newCtor);

    let newPayload: Option<RuntimeValue> = null;
    if (newCtorType?.value) {
      // If the new constructor has a payload, create a default value for it
      newPayload = { value: getDefaultValue(newCtorType.value) };
    }

    const newValueRaw: RuntimeValueRaw = {
      kind: 'Enum',
      value: [enumDeclaration, [newCtor, newPayload]],
    };
    onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
  };

  const handlePayloadChange = (newPayloadTestIo: TestIo): void => {
    // newPayloadTestIo contains the updated ValueDef for the payload
    const newPayloadValue = newPayloadTestIo.value?.value ?? null; // Extract RuntimeValue or null

    // Construct the Option<RuntimeValue> correctly
    const newPayloadOption: Option<RuntimeValue> =
      newPayloadValue === null ? null : { value: newPayloadValue };

    const newValueRaw: RuntimeValueRaw = {
      kind: 'Enum',
      value: [enumDeclaration, [currentCtor, newPayloadOption]],
    };
    onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
  };

  return (
    <div className="value-editor enum-editor">
      <select onChange={handleCtorChange} value={currentCtor}>
        {Array.from(enumDeclaration.constructors.keys()).map((ctorName) => (
          <option key={ctorName} value={ctorName}>
            {ctorName}
          </option>
        ))}
      </select>
      {/* Render payload editor only if the current constructor expects one */}
      {currentCtorType?.value && (
        <div className="enum-payload-editor">
          <ValueEditor
            testIO={{
              typ: currentCtorType.value,
              value: currentPayload?.value
                ? { value: currentPayload.value } // Create temporary ValueDef for payload
                : undefined,
            }}
            onValueChange={handlePayloadChange}
          />
        </div>
      )}
    </div>
  );
}
