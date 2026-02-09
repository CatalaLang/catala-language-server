import type {
  Typ,
  StructDeclaration,
  EnumDeclaration,
  RuntimeValue,
  RuntimeValueRaw,
  ValueDef,
  TestIo,
  Option,
} from '../src/generated/test_case';

// ============================================================================
// Type builders
// ============================================================================

export function tStruct(name: string, fields: Map<string, Typ>): Typ {
  return { kind: 'TStruct', value: { struct_name: name, fields } };
}

export function tEnum(name: string, ctors: Map<string, Option<Typ>>): Typ {
  return { kind: 'TEnum', value: { enum_name: name, constructors: ctors } };
}

export function tRat(): Typ {
  return { kind: 'TRat' } as Typ;
}

// ============================================================================
// RuntimeValue builders
// ============================================================================

export function rv(raw: RuntimeValueRaw): RuntimeValue {
  return { value: raw, attrs: [] };
}

export function rvWithUid(raw: RuntimeValueRaw): RuntimeValue {
  return { value: raw, attrs: [{ kind: 'Uid', value: crypto.randomUUID() }] };
}

export function intVal(n: number): RuntimeValue {
  return rv({ kind: 'Integer', value: n });
}

export function moneyVal(cents: number): RuntimeValue {
  return rv({ kind: 'Money', value: cents });
}

export function dateVal(
  year: number,
  month: number,
  day: number
): RuntimeValue {
  return rv({ kind: 'Date', value: { year, month, day } });
}

export function boolVal(b: boolean): RuntimeValue {
  return rv({ kind: 'Bool', value: b });
}

export function arrayVal(items: RuntimeValue[]): RuntimeValue {
  return rv({ kind: 'Array', value: items });
}

export function structVal(
  decl: StructDeclaration,
  fields: Map<string, RuntimeValue>
): RuntimeValue {
  return rv({ kind: 'Struct', value: [decl, fields] });
}

export function enumVal(
  decl: EnumDeclaration,
  label: string,
  payload?: RuntimeValue | null
): RuntimeValue {
  return rv({
    kind: 'Enum',
    value: [decl, [label, payload ? { value: payload } : null]],
  });
}

// ============================================================================
// Test data builders
// ============================================================================

function vd(r: RuntimeValue): ValueDef {
  return { value: r };
}

export function io(typ: Typ, r?: RuntimeValue): TestIo {
  return { typ, value: r ? vd(r) : undefined };
}
