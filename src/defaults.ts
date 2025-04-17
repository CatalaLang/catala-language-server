import type { Typ, RuntimeValue, RuntimeValueRaw } from './generated/test_case';
import { assertUnreachable } from './util';

export function getDefaultValue(typ: Typ): RuntimeValue {
  return {
    value: getDefaultValueRaw(typ),
    attrs: [],
  };
}

function getDefaultValueRaw(typ: Typ): RuntimeValueRaw {
  switch (typ.kind) {
    case 'TBool':
      return { kind: 'Bool', value: false };
    case 'TInt':
      return { kind: 'Integer', value: 0 };
    case 'TMoney':
      return { kind: 'Money', value: 0 };
    case 'TRat':
      return { kind: 'Decimal', value: 0 };
    case 'TDate': {
      const today = new Date();
      return {
        kind: 'Date',
        value: {
          year: today.getFullYear(),
          month: today.getMonth() + 1,
          day: today.getDate(),
        },
      };
    }
    case 'TDuration':
      return { kind: 'Duration', value: { years: 0, months: 0, days: 0 } };
    case 'TStruct':
      return {
        kind: 'Struct',
        value: [
          typ.value,
          new Map(
            Array.from(typ.value.fields, ([fieldName, fieldType]) => [
              fieldName,
              getDefaultValue(fieldType),
            ])
          ),
        ],
      };
    case 'TArray':
      return { kind: 'Array', value: [] };
    case 'TTuple':
    case 'TOption':
      throw new Error(
        `Default value not implemented for compound type: ${typ.kind}`
      );
    case 'TEnum': {
      // Get first constructor (enums must have at least one)
      const firstCtor = Array.from(typ.value.constructors.keys())[0];
      const ctorType = typ.value.constructors.get(firstCtor);
      return {
        kind: 'Enum',
        value: [
          typ.value,
          [
            firstCtor,
            ctorType ? { value: getDefaultValue(ctorType.value) } : null,
          ],
        ],
      };
    }
    default:
      assertUnreachable(typ);
  }
}
