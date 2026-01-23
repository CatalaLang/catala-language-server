// Editors for a single value type (grouped with a factory function)

import { type ReactElement, useState, useEffect } from 'react';
import type React from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import type {
  Option,
  TestIo,
  StructDeclaration,
  RuntimeValue,
  RuntimeValueRaw,
  EnumDeclaration,
  Typ,
  ValueDef,
  PathSegment,
  Diff,
} from '../generated/catala_types';
import { ArrayEditor } from './ArrayEditor';
import { assertUnreachable } from '../util';
import { CompositeEditor } from './CompositeEditor';
import { findMatchingDiff } from '../diff/highlight';
import { isAtomicRuntime } from '../diff/diff';

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

function makeUnset(originalRuntimeValue?: RuntimeValue): RuntimeValue {
  // Use Unset as a GUI placeholder; server/runtime should treat it as "impossible" value.
  return createRuntimeValue({ kind: 'Unset' }, originalRuntimeValue);
}

export function getDefaultValue(
  typ: Typ,
  originalRuntimeValue?: RuntimeValue
): RuntimeValue {
  // Arrays are the only type with a "sane default": empty list.
  // All other types start as explicit Unset placeholders.
  return typ.kind === 'TArray'
    ? createRuntimeValue({ kind: 'Array', value: [] }, originalRuntimeValue)
    : makeUnset(originalRuntimeValue);
}

function UnsetBadge(): ReactElement {
  return (
    <span className="unset-badge">
      <FormattedMessage id="editor.unset" defaultMessage="Unset" />
    </span>
  );
}

function InvalidBadge(): ReactElement {
  return (
    <span className="invalid-badge">
      <FormattedMessage id="editor.invalid" defaultMessage="Invalid" />
    </span>
  );
}

type Props = {
  testIO: TestIo;
  onValueChange(newValue: TestIo): void;
  editorHook?: (editor: ReactElement, path: PathSegment[]) => ReactElement;
  currentPath: PathSegment[];
  diffs: Diff[];
  editable?: boolean;
  onDiffResolved?: (path: PathSegment[]) => void;
  onInvalidateDiffs?: (pathPrefix: PathSegment[]) => void;
};

/*
  Diff policy note:
  - Leaf editors do not auto-accept actual values; acceptance is explicit (e.g., Enum preview button).
  - Arrays handle path-unstable edits (insert/remove/move) in ArrayEditor, asserting arrays as a whole.
*/
export default function ValueEditor(props: Props): ReactElement {
  const {
    testIO,
    onValueChange,
    editorHook = (editor): ReactElement => editor,
    currentPath,
    editable = true,
  } = props;
  const { typ, value: valueDef } = testIO;

  const handleValueChange = (newRuntimeValue: RuntimeValue): void => {
    if (!editable) {
      throw new Error('Attempted to change a read-only value');
    }
    onValueChange({
      typ,
      value: {
        value: newRuntimeValue,
      },
    });
  };

  let editor: ReactElement;

  switch (typ.kind) {
    case 'TInt':
      editor = (
        <IntEditor
          valueDef={valueDef}
          onValueChange={handleValueChange}
          editable={editable}
        />
      );
      break;
    case 'TBool':
      editor = (
        <BoolEditor
          valueDef={valueDef}
          onValueChange={handleValueChange}
          editable={editable}
        />
      );
      break;
    case 'TStruct':
      editor = (
        <StructEditor
          structDeclaration={typ.value}
          valueDef={valueDef}
          onValueChange={handleValueChange}
          editorHook={editorHook}
          currentPath={currentPath}
          diffs={props.diffs}
          editable={editable}
          onDiffResolved={props.onDiffResolved}
          onInvalidateDiffs={props.onInvalidateDiffs}
        />
      );
      break;
    case 'TRat':
      editor = (
        <RatEditor
          valueDef={valueDef}
          onValueChange={handleValueChange}
          editable={editable}
        />
      );
      break;
    case 'TDate':
      editor = (
        <DateEditor
          valueDef={valueDef}
          onValueChange={handleValueChange}
          editable={editable}
        />
      );
      break;
    case 'TDuration':
      editor = (
        <DurationEditor
          valueDef={valueDef}
          onValueChange={handleValueChange}
          editable={editable}
        />
      );
      break;
    case 'TMoney':
      editor = (
        <MoneyEditor
          valueDef={valueDef}
          onValueChange={handleValueChange}
          editable={editable}
        />
      );
      break;
    case 'TEnum':
      editor = (
        <EnumEditor
          enumDeclaration={typ.value}
          valueDef={valueDef}
          onValueChange={handleValueChange}
          editorHook={editorHook}
          currentPath={currentPath}
          diffs={props.diffs}
          editable={editable}
          onDiffResolved={props.onDiffResolved}
          onInvalidateDiffs={props.onInvalidateDiffs}
        />
      );
      break;
    case 'TArray':
      editor = (
        <ArrayEditor
          elementType={typ.value}
          valueDef={valueDef}
          onValueChange={handleValueChange}
          editorHook={editorHook}
          currentPath={currentPath}
          diffs={props.diffs}
          editable={editable}
          onDiffResolved={props.onDiffResolved}
          onInvalidateDiffs={props.onInvalidateDiffs}
        />
      );
      break;
    case 'TOption': {
      const constructors = new Map<string, Option<Typ>>();
      constructors.set('Absent', null);
      constructors.set('Present', { value: typ.value });
      const option_decl: EnumDeclaration = {
        enum_name: 'Optional',
        constructors,
      };
      editor = (
        <EnumEditor
          enumDeclaration={option_decl}
          valueDef={valueDef}
          onValueChange={handleValueChange}
          editorHook={editorHook}
          currentPath={currentPath}
          diffs={props.diffs}
          editable={editable}
          onDiffResolved={props.onDiffResolved}
          onInvalidateDiffs={props.onInvalidateDiffs}
        />
      );
      break;
    }
    case 'TTuple':
      editor = <i>Unimplemented Tuple type</i>;
      break;
    case 'TArrow':
    case 'TUnit':
      editor = <i>Unsupported type</i>;
      break;
    default:
      assertUnreachable(typ);
  }

  return editorHook(editor, currentPath);
}

const INT_PATTERN = /^-?\d+$/;

function isValidInt(value: string): boolean {
  return INT_PATTERN.test(value);
}

type IntEditorProps = {
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
  editable?: boolean;
};

function IntEditor(props: IntEditorProps): ReactElement {
  const runtimeValue = props.valueDef?.value;
  const isUnset = !runtimeValue || runtimeValue.value.kind === 'Unset';
  const initialValue =
    runtimeValue?.value.kind === 'Integer'
      ? runtimeValue.value.value
      : undefined;

  const [displayValue, setDisplayValue] = useState(
    initialValue?.toString() ?? ''
  );
  const [isInvalid, setIsInvalid] = useState(false);

  // Update display value if prop changes externally
  useEffect(() => {
    if (isInvalid) return;
    const newValue =
      runtimeValue?.value.kind === 'Integer'
        ? runtimeValue.value.value
        : undefined;
    setDisplayValue(newValue?.toString() ?? '');
  }, [runtimeValue, isInvalid]);

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>): void => {
    const valueStr = event.target.value;
    setDisplayValue(valueStr);

    if (isValidInt(valueStr)) {
      setIsInvalid(false);
      const newValueRaw: RuntimeValueRaw = {
        kind: 'Integer',
        value: Number(valueStr),
      };
      props.onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
    } else if (valueStr.trim() === '') {
      setIsInvalid(false);
      props.onValueChange(makeUnset(runtimeValue));
    } else {
      setIsInvalid(true);
      props.onValueChange(makeUnset(runtimeValue));
    }
  };

  const handleBlur = (): void => {
    if (displayValue.trim() === '') {
      props.onValueChange(makeUnset(runtimeValue));
      setIsInvalid(false);
      return;
    }
    if (!isValidInt(displayValue)) {
      // keep the user's partial invalid input; do not restore previous value
      setIsInvalid(true);
    }
  };

  return (
    <div className="value-editor int-editor-wrapper">
      {isInvalid ? <InvalidBadge /> : isUnset && <UnsetBadge />}
      <input
        type="text"
        pattern={INT_PATTERN.source}
        value={displayValue}
        onChange={handleChange}
        onBlur={handleBlur}
        className={`int-editor ${displayValue && !isValidInt(displayValue) ? 'invalid' : ''}`}
        disabled={props.editable === false}
      />
    </div>
  );
}

type DateEditorProps = {
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
  editable?: boolean;
};

function DateEditor(props: DateEditorProps): ReactElement {
  const runtimeValue = props.valueDef?.value;
  const isUnset = !runtimeValue || runtimeValue.value.kind === 'Unset';
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
  const [isInvalid, setIsInvalid] = useState(false);

  // Update display value if prop changes externally
  useEffect(() => {
    if (isInvalid) return;
    const newValue =
      runtimeValue?.value.kind === 'Date'
        ? runtimeValue.value.value
        : undefined;
    setInternalValue(formatDate(newValue));
  }, [runtimeValue, isInvalid]);

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
    if (dateStr.trim() === '') {
      setIsInvalid(false);
      props.onValueChange(makeUnset(runtimeValue));
      return;
    }
    if (newDate) {
      setIsInvalid(false);
      const newValueRaw: RuntimeValueRaw = { kind: 'Date', value: newDate };
      props.onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
    } else {
      setIsInvalid(true);
      props.onValueChange(makeUnset(runtimeValue));
    }
  };

  const handleBlur = (): void => {
    if (internalValue === '') {
      props.onValueChange(makeUnset(runtimeValue));
      setIsInvalid(false);
      return;
    }
  };

  return (
    <div className="value-editor date-editor-wrapper">
      {isInvalid ? <InvalidBadge /> : isUnset && <UnsetBadge />}
      <input
        type="date"
        value={internalValue}
        onChange={handleChange}
        onBlur={handleBlur}
        disabled={props.editable === false}
        max="9999-12-31" /* catala date literals are coded on 4 digits */
      />
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
  editable?: boolean;
};

function RatEditor(props: RatEditorProps): ReactElement {
  const runtimeValue = props.valueDef?.value;
  const isUnset = !runtimeValue || runtimeValue.value.kind === 'Unset';
  const initialValue =
    runtimeValue?.value.kind === 'Decimal'
      ? runtimeValue.value.value
      : undefined;

  const [displayValue, setDisplayValue] = useState(
    initialValue?.toString() ?? ''
  );
  const [isInvalid, setIsInvalid] = useState(false);

  // Update display value if prop changes externally
  useEffect(() => {
    if (isInvalid) return;
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
  }, [runtimeValue, isInvalid]);

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>): void => {
    const valueStr = event.target.value;
    setDisplayValue(valueStr);

    if (isValidRat(valueStr)) {
      setIsInvalid(false);
      const newValueRaw: RuntimeValueRaw = {
        kind: 'Decimal',
        value: Number(valueStr),
      };
      props.onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
    } else if (valueStr.trim() === '') {
      setIsInvalid(false);
      props.onValueChange(makeUnset(runtimeValue));
    } else {
      setIsInvalid(true);
      props.onValueChange(makeUnset(runtimeValue));
    }
  };

  const handleBlur = (): void => {
    if (displayValue.trim() === '') {
      props.onValueChange(makeUnset(runtimeValue));
      setIsInvalid(false);
      return;
    }
    if (!isValidRat(displayValue)) {
      // keep the user's partial invalid input; do not restore previous value
      setIsInvalid(true);
    }
  };

  return (
    <div className="value-editor rat-editor-wrapper">
      {isInvalid ? <InvalidBadge /> : isUnset && <UnsetBadge />}
      <input
        type="text"
        pattern={RAT_PATTERN.source}
        value={displayValue}
        onChange={handleChange}
        onBlur={handleBlur}
        className={`rat-editor ${displayValue && !isValidRat(displayValue) ? 'invalid' : ''}`}
        disabled={props.editable === false}
      />
    </div>
  );
}

type BoolEditorProps = {
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
  editable?: boolean;
};

function BoolEditor(props: BoolEditorProps): ReactElement {
  const runtimeValue = props.valueDef?.value;
  const isUnset = !runtimeValue || runtimeValue.value.kind === 'Unset';
  const selectValue: 'unset' | 'true' | 'false' =
    runtimeValue?.value.kind === 'Bool'
      ? runtimeValue.value.value
        ? 'true'
        : 'false'
      : 'unset';

  const handleChange = (event: React.ChangeEvent<HTMLSelectElement>): void => {
    if (event.target.value === 'unset') {
      props.onValueChange(makeUnset(runtimeValue));
      return;
    }
    const boolValue = event.target.value === 'true';
    const newValueRaw: RuntimeValueRaw = { kind: 'Bool', value: boolValue };
    props.onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
  };

  return (
    <div className="value-editor bool-editor-wrapper">
      {isUnset && <UnsetBadge />}
      <select
        value={selectValue}
        onChange={handleChange}
        disabled={props.editable === false}
      >
        <option value="unset">
          {/* We leave the Unset option empty so as to avoid giving
          the user the impression that it is a tri-state value (i.e. signal
          that Unset is unexpected when running a test) */}
        </option>
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
  editable?: boolean;
};

function DurationEditor(props: DurationEditorProps): ReactElement {
  const runtimeValue = props.valueDef?.value;
  const initialValue =
    runtimeValue?.value.kind === 'Duration'
      ? runtimeValue.value.value
      : undefined;

  const isUnset = !runtimeValue || runtimeValue.value.kind === 'Unset';

  const [years, setYears] = useState<string>(
    initialValue?.years !== undefined ? String(initialValue.years) : ''
  );
  const [months, setMonths] = useState<string>(
    initialValue?.months !== undefined ? String(initialValue.months) : ''
  );
  const [days, setDays] = useState<string>(
    initialValue?.days !== undefined ? String(initialValue.days) : ''
  );
  const [isInvalid, setIsInvalid] = useState(false);

  // Update state if prop changes externally
  useEffect(() => {
    if (isInvalid) return;
    const newValue =
      runtimeValue?.value.kind === 'Duration'
        ? runtimeValue.value.value
        : undefined;
    setYears((prev) => {
      if (newValue?.years !== undefined) {
        const v = newValue.years;
        return prev === '' && v === 0 ? '' : String(v);
      }
      return '';
    });
    // Preserve blank inputs for zero values so optional fields remain visually empty
    setMonths((prev) => {
      if (newValue?.months !== undefined) {
        const v = newValue.months;
        return prev === '' && v === 0 ? '' : String(v);
      }
      return '';
    });
    setDays((prev) => {
      if (newValue?.days !== undefined) {
        const v = newValue.days;
        return prev === '' && v === 0 ? '' : String(v);
      }
      return '';
    });
  }, [runtimeValue, isInvalid]);

  const parseIntWithSign = (s: string): number | null => {
    if (s.trim() === '') return null;
    const n = Number(s);
    if (!Number.isInteger(n)) return null;
    return n;
  };

  const SIGNED_INT = /^-?\d*$/;

  const maybeEmitWith = (yStr: string, mStr: string, dStr: string): void => {
    const yEmpty = yStr.trim() === '';
    const mEmpty = mStr.trim() === '';
    const dEmpty = dStr.trim() === '';

    if (yEmpty && mEmpty && dEmpty) {
      props.onValueChange(makeUnset(runtimeValue));
      setIsInvalid(false);
      return;
    }

    const yParsed = yEmpty ? null : parseIntWithSign(yStr);
    const mParsed = mEmpty ? null : parseIntWithSign(mStr);
    const dParsed = dEmpty ? null : parseIntWithSign(dStr);

    const anyInvalid =
      (!yEmpty && yParsed === null) ||
      (!mEmpty && mParsed === null) ||
      (!dEmpty && dParsed === null);

    if (anyInvalid) {
      setIsInvalid(true);
      props.onValueChange(makeUnset(runtimeValue));
      return;
    }

    const newValueRaw: RuntimeValueRaw = {
      kind: 'Duration',
      value: {
        years: yParsed ?? 0,
        months: mParsed ?? 0,
        days: dParsed ?? 0,
      },
    };
    setIsInvalid(false);
    props.onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
  };

  const maybeEmitFromState = (): void => {
    maybeEmitWith(years, months, days);
  };

  const handleBlur = (): void => {
    maybeEmitFromState();
  };

  return (
    <div className="value-editor duration-editor">
      {isInvalid ? <InvalidBadge /> : isUnset && <UnsetBadge />}
      <div className="duration-fields">
        <label>
          <FormattedMessage id="durationEditor.years" defaultMessage="Years:" />
          <input
            type="text"
            inputMode="numeric"
            pattern="-?[0-9]*"
            value={years}
            onChange={(e) => {
              const v = e.target.value;
              if (!SIGNED_INT.test(v)) return;
              setYears(v);
              maybeEmitWith(v, months, days);
            }}
            onBlur={handleBlur}
            disabled={props.editable === false}
          />
        </label>
        <label>
          <FormattedMessage
            id="durationEditor.months"
            defaultMessage="Months:"
          />
          <input
            type="text"
            inputMode="numeric"
            pattern="-?[0-9]*"
            value={months}
            onChange={(e) => {
              const v = e.target.value;
              if (!SIGNED_INT.test(v)) return;
              setMonths(v);
              maybeEmitWith(years, v, days);
            }}
            onBlur={handleBlur}
            disabled={props.editable === false}
          />
        </label>
        <label>
          <FormattedMessage id="durationEditor.days" defaultMessage="Days:" />
          <input
            type="text"
            inputMode="numeric"
            pattern="-?[0-9]*"
            value={days}
            onChange={(e) => {
              const v = e.target.value;
              if (!SIGNED_INT.test(v)) return;
              setDays(v);
              maybeEmitWith(years, months, v);
            }}
            onBlur={handleBlur}
            disabled={props.editable === false}
          />
        </label>
      </div>
    </div>
  );
}

const MONEY_PATTERN = /^-?\d+(\.\d{1,2})?$/;

function isValidMoney(value: string): boolean {
  return MONEY_PATTERN.test(value);
}

type MoneyEditorProps = {
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
  editable?: boolean;
};

function MoneyEditor(props: MoneyEditorProps): ReactElement {
  const intl = useIntl();
  const runtimeValue = props.valueDef?.value;
  const isUnset = !runtimeValue || runtimeValue.value.kind === 'Unset';
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
  const [isInvalid, setIsInvalid] = useState(false);

  // Update display value if prop changes externally
  useEffect(() => {
    if (isInvalid) return;
    const newValue =
      runtimeValue?.value.kind === 'Money'
        ? runtimeValue.value.value
        : undefined;
    setDisplayValue(centsToDisplayValue(newValue));
  }, [runtimeValue, isInvalid]);

  const handleChange = (event: React.ChangeEvent<HTMLInputElement>): void => {
    const valueStr = event.target.value;
    setDisplayValue(valueStr);

    if (isValidMoney(valueStr)) {
      setIsInvalid(false);
      const centsValue = Math.round(parseFloat(valueStr) * 100);
      const newValueRaw: RuntimeValueRaw = {
        kind: 'Money',
        value: centsValue,
      };
      props.onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
    } else if (valueStr.trim() === '') {
      setIsInvalid(false);
      props.onValueChange(makeUnset(runtimeValue));
    } else {
      setIsInvalid(true);
      props.onValueChange(makeUnset(runtimeValue));
    }
  };

  const handleBlur = (): void => {
    if (displayValue.trim() === '') {
      props.onValueChange(makeUnset(runtimeValue));
      setIsInvalid(false);
      return;
    }
  };

  // Determine currency style based on locale
  const locale = intl.locale;
  const currencyClass = locale.startsWith('fr')
    ? 'currency-eur'
    : 'currency-usd';

  return (
    <div className={`value-editor money-editor ${currencyClass}`}>
      {isInvalid ? <InvalidBadge /> : isUnset && <UnsetBadge />}
      <input
        type="text"
        pattern={MONEY_PATTERN.source}
        value={displayValue}
        onChange={handleChange}
        onBlur={handleBlur}
        className={displayValue && !isValidMoney(displayValue) ? 'invalid' : ''}
        disabled={props.editable === false}
      />
    </div>
  );
}

type StructEditorProps = {
  structDeclaration: StructDeclaration;
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
  editorHook?: (editor: ReactElement, path: PathSegment[]) => ReactElement;
  currentPath: PathSegment[];
  diffs: Diff[];
  editable?: boolean;
  onDiffResolved?: (path: PathSegment[]) => void;
  onInvalidateDiffs?: (pathPrefix: PathSegment[]) => void;
};

function StructEditor(props: StructEditorProps): ReactElement {
  const {
    structDeclaration,
    valueDef,
    onValueChange,
    editorHook,
    currentPath,
    editable,
  } = props;
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
    const isStructPresent = runtimeValue?.value.kind === 'Struct';
    const newMap = new Map(currentMap);

    // If the struct did not exist yet, pre-fill all fields with Unset placeholders
    if (!isStructPresent) {
      for (const [fname, ftype] of fields.entries()) {
        if (!newMap.has(fname)) {
          newMap.set(fname, getDefaultValue(ftype));
        }
      }
    }

    // Set/overwrite the edited field
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
      const childPath: PathSegment[] = [
        ...currentPath,
        { kind: 'StructField', value: fieldName },
      ];
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
            editorHook={editorHook}
            currentPath={childPath}
            diffs={props.diffs}
            editable={editable}
            onDiffResolved={props.onDiffResolved}
            onInvalidateDiffs={props.onInvalidateDiffs}
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
  editorHook?: (editor: ReactElement, path: PathSegment[]) => ReactElement;
  currentPath: PathSegment[];
  diffs: Diff[];
  editable?: boolean;
  onDiffResolved?: (path: PathSegment[]) => void;
  onInvalidateDiffs?: (pathPrefix: PathSegment[]) => void;
};

function EnumEditor(props: EnumEditorProps): ReactElement {
  const {
    enumDeclaration,
    valueDef,
    onValueChange,
    editorHook,
    currentPath,
    editable,
  } = props;
  const runtimeValue = valueDef?.value;
  const currentEnumData =
    runtimeValue?.value.kind === 'Enum' ? runtimeValue.value.value : undefined;

  const isUnset = !runtimeValue || runtimeValue.value.kind === 'Unset';
  const currentCtor = currentEnumData ? currentEnumData[1][0] : null;
  const selectValue: string = currentCtor ?? '__unset__';
  const currentPayload = currentEnumData ? currentEnumData[1][1] : null;
  const currentCtorType =
    currentCtor !== null
      ? enumDeclaration.constructors.get(currentCtor)
      : undefined; // Option<Typ> | undefined

  // If there is a diff exactly at this enum node and the enum labels differ,
  // and the actual payload is non-atomic (complex), render an actual preview
  // (read-only), similar to arrays' phantom view.
  const enumTyp: Typ = { kind: 'TEnum', value: enumDeclaration };
  const matchingDiff = findMatchingDiff(props.diffs, currentPath);

  const showEnumActualPreview =
    !!matchingDiff &&
    matchingDiff.actual.value.kind === 'Enum' &&
    matchingDiff.expected.value.kind === 'Enum' &&
    ((): boolean => {
      const [, [labelA, payloadA]] = matchingDiff.actual.value.value;
      const [, [labelE]] = matchingDiff.expected.value.value;
      const payloadActualRv = payloadA?.value;
      return (
        labelA !== labelE &&
        !!payloadActualRv &&
        !isAtomicRuntime(payloadActualRv)
      );
    })();

  // Note: do not early-return here; we render the expected editor and, when applicable,
  // an "actual preview" block alongside it further below.
  const handleCtorChange = (
    event: React.ChangeEvent<HTMLSelectElement>
  ): void => {
    if (editable === false) return;
    const v = event.target.value;
    if (v === '__unset__') {
      onValueChange(makeUnset(runtimeValue));
      return;
    }
    const newCtor = v;
    const newCtorType = enumDeclaration.constructors.get(newCtor);

    let newPayload: Option<RuntimeValue> = null;
    if (newCtorType?.value) {
      // Initialize payload with default for its type (empty array for arrays, Unset otherwise)
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
      value: [enumDeclaration, [currentCtor!, newPayloadOption]],
    };
    onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
  };

  // Build the expected editor (existing UI)
  const expectedEditor = (
    <div className="value-editor enum-editor">
      {isUnset && <UnsetBadge />}
      <select
        onChange={handleCtorChange}
        value={selectValue}
        disabled={editable === false}
      >
        <option value="__unset__">
          {/* We keep that blank as it is more distinguishable
              from a user-defined Catala enum label than an
              explicit 'Unset' value; we show a badge in addition. */}
        </option>
        {Array.from(enumDeclaration.constructors.keys()).map((ctorName) => (
          <option key={ctorName} value={ctorName}>
            {ctorName}
          </option>
        ))}
      </select>
      {/* Render payload editor only if the current constructor expects one */}
      {selectValue !== '__unset__' && currentCtorType?.value && (
        <div className="enum-payload-editor">
          <ValueEditor
            testIO={{
              typ: currentCtorType.value,
              value: currentPayload?.value
                ? { value: currentPayload.value } // Create temporary ValueDef for payload
                : undefined,
            }}
            onValueChange={handlePayloadChange}
            editorHook={editorHook}
            // Do NOT add EnumPayload here: server diff paths are transparent over enums
            currentPath={currentPath}
            diffs={props.diffs}
            editable={editable}
            onDiffResolved={props.onDiffResolved}
            onInvalidateDiffs={props.onInvalidateDiffs}
          />
        </div>
      )}
    </div>
  );

  if (showEnumActualPreview) {
    return (
      <div className="diff-highlight container-diff enum-preview enum-phantom">
        <div className="expected-value">
          <div className="expected-label">
            <FormattedMessage id="diff.expected" defaultMessage="Expected" />
          </div>
          {expectedEditor}
        </div>
        <div className="actual-preview-value phantom-actual-value">
          <div className="actual-preview-label phantom-actual-label">
            <FormattedMessage
              id="diff.actualValue"
              defaultMessage="Actual value"
            />
          </div>
          <div className="actual-preview-content phantom-actual-content">
            <ValueEditor
              testIO={{ typ: enumTyp, value: { value: matchingDiff.actual } }}
              onValueChange={() => {
                // read-only preview
              }}
              editorHook={undefined}
              currentPath={currentPath}
              diffs={[]}
              editable={false}
            />
          </div>
          <button
            className="diff-action-accept array-phantom-action array-phantom-add"
            onClick={() => {
              if (editable === false) return;
              onValueChange(
                createRuntimeValue(matchingDiff.actual.value, runtimeValue)
              );
              props.onDiffResolved?.(currentPath);
            }}
            disabled={editable === false}
          >
            <span className="codicon codicon-check"></span>
            <FormattedMessage
              id="diff.replaceExpected"
              defaultMessage="Replace expected with computed value"
            />
          </button>
        </div>
      </div>
    );
  }

  // Fallback: just the expected editor
  return expectedEditor;
}
