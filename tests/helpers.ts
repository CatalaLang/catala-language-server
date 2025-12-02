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

export function tStruct(name: string, fields: Map<string, Typ>): Typ {
  return { kind: 'TStruct', value: { struct_name: name, fields } };
}

export function tEnum(name: string, ctors: Map<string, Option<Typ>>): Typ {
  return { kind: 'TEnum', value: { enum_name: name, constructors: ctors } };
}

export function tRat(): Typ {
  return { kind: 'TRat' } as Typ;
}

function rv(raw: RuntimeValueRaw): RuntimeValue {
  return { value: raw, attrs: [] };
}

function vd(r: RuntimeValue): ValueDef {
  return { value: r };
}

export function io(typ: Typ, r?: RuntimeValue): TestIo {
  return { typ, value: r ? vd(r) : undefined };
}

export function structValue(
  decl: StructDeclaration,
  fields: Map<string, RuntimeValue>
): RuntimeValue {
  return rv({ kind: 'Struct', value: [decl, fields] });
}

export function enumValue(
  decl: EnumDeclaration,
  label: string,
  payload?: RuntimeValue | null
): RuntimeValue {
  return rv({
    kind: 'Enum',
    value: [decl, [label, payload ? { value: payload } : null]],
  });
}
