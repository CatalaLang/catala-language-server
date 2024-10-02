import type { Typ, RuntimeValue } from './generated/test_case';
import { assertUnreachable } from './util';

export function getDefaultValue(typ: Typ): RuntimeValue {
  switch (typ.kind) {
    case 'TBool':
      return { kind: 'Bool', value: false };
    case 'TInt':
    case 'TMoney':
      return { kind: 'Integer', value: 0 };
    case 'TRat':
      return { kind: 'Decimal', value: 0 };
    case 'TDate':
      return { kind: 'Date', value: { year: 1970, month: 1, day: 1 } };
    case 'TDuration':
      return { kind: 'Duration', value: { years: 0, months: 0, days: 0 } };
    case 'TTuple':
    case 'TStruct':
    case 'TEnum':
    case 'TOption':
    case 'TArray':
      throw new Error(
        `Default value not implemented for compound type: ${typ.kind}`
      );
    default:
      assertUnreachable(typ);
  }
}
