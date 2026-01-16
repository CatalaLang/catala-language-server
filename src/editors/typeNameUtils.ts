/**
 * Type name extraction utilities for displaying Catala types in the UI.
 */

import type { IntlShape } from 'react-intl';
import type { Typ } from '../generated/catala_types';

function extractTypeName(qualifiedName: string): string {
  return qualifiedName.split('.').pop() ?? qualifiedName;
}

export function getTypeName(typ: Typ): string {
  switch (typ.kind) {
    case 'TStruct':
      return extractTypeName(typ.value.struct_name);
    case 'TEnum':
      return extractTypeName(typ.value.enum_name);
    case 'TArray':
      return `${getTypeName(typ.value)}[]`;
    case 'TInt':
      return 'Integer';
    case 'TBool':
      return 'Boolean';
    case 'TRat':
      return 'Decimal';
    case 'TMoney':
      return 'Money';
    case 'TDate':
      return 'Date';
    case 'TDuration':
      return 'Duration';
    case 'TUnit':
      return 'Unit';
    case 'TTuple':
      return 'Tuple';
    case 'TOption':
      return `${getTypeName(typ.value)}?`;
    case 'TArrow':
      return 'Function';
    default: {
      const _exhaustive: never = typ;
      void _exhaustive;
      return 'Unknown';
    }
  }
}

export function getTypeDisplayName(typ: Typ, intl: IntlShape): string {
  switch (typ.kind) {
    case 'TBool':
      return intl.formatMessage({ id: 'type.boolean' });
    case 'TInt':
      return intl.formatMessage({ id: 'type.integer' });
    case 'TRat':
      return intl.formatMessage({ id: 'type.decimal' });
    case 'TMoney':
      return intl.formatMessage({ id: 'type.money' });
    case 'TDate':
      return intl.formatMessage({ id: 'type.date' });
    case 'TDuration':
      return intl.formatMessage({ id: 'type.duration' });
    case 'TArray': {
      const baseType = getTypeDisplayName(typ.value, intl);
      return intl.formatMessage({ id: 'type.array' }, { baseType });
    }
    case 'TOption': {
      const baseType = getTypeDisplayName(typ.value, intl);
      return intl.formatMessage({ id: 'type.optional' }, { baseType });
    }
    case 'TTuple':
      return intl.formatMessage({ id: 'type.tuple' });
    case 'TStruct':
      return extractTypeName(typ.value.struct_name);
    case 'TEnum':
      return extractTypeName(typ.value.enum_name);
    case 'TArrow':
      throw new Error('Unexpected type: TArrow');
    case 'TUnit':
      throw new Error('Unexpected type: TUnit');
    default: {
      const _exhaustive: never = typ;
      void _exhaustive;
      throw new Error('Unknown type');
    }
  }
}
