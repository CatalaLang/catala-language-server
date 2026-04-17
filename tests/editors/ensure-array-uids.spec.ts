import { describe, it, expect } from 'vitest';
import type {
  RuntimeValue,
  StructDeclaration,
  EnumDeclaration,
} from '../../src/generated/catala_types';
import { ensureArrayUids } from '../../src/editors/tableArrayUtils';

describe('ensureArrayUids', () => {
  it('stamps UIDs on array items that have none', () => {
    const value: RuntimeValue = {
      value: {
        kind: 'Array',
        value: [
          { value: { kind: 'Integer', value: 1 }, attrs: [] },
          { value: { kind: 'Integer', value: 2 }, attrs: [] },
        ],
      },
      attrs: [],
    };

    const result = ensureArrayUids(value);
    const items = (result.value as { kind: 'Array'; value: RuntimeValue[] })
      .value;

    expect(items[0].attrs.some((a) => a.kind === 'Uid')).toBe(true);
    expect(items[1].attrs.some((a) => a.kind === 'Uid')).toBe(true);
    const uid0 = items[0].attrs.find((a) => a.kind === 'Uid')?.value;
    const uid1 = items[1].attrs.find((a) => a.kind === 'Uid')?.value;
    expect(uid0).not.toBe(uid1);
  });

  it('leaves existing UIDs untouched (idempotent)', () => {
    const value: RuntimeValue = {
      value: {
        kind: 'Array',
        value: [
          {
            value: { kind: 'Integer', value: 1 },
            attrs: [{ kind: 'Uid', value: 'existing-uid' }],
          },
        ],
      },
      attrs: [],
    };

    const result = ensureArrayUids(value);
    const items = (result.value as { kind: 'Array'; value: RuntimeValue[] })
      .value;
    const uid = items[0].attrs.find((a) => a.kind === 'Uid')?.value;

    expect(uid).toBe('existing-uid');
  });

  it('handles mixed array: stamps only items missing UIDs', () => {
    const value: RuntimeValue = {
      value: {
        kind: 'Array',
        value: [
          {
            value: { kind: 'Integer', value: 1 },
            attrs: [{ kind: 'Uid', value: 'keep-me' }],
          },
          { value: { kind: 'Integer', value: 2 }, attrs: [] },
        ],
      },
      attrs: [],
    };

    const result = ensureArrayUids(value);
    const items = (result.value as { kind: 'Array'; value: RuntimeValue[] })
      .value;

    expect(items[0].attrs.find((a) => a.kind === 'Uid')?.value).toBe('keep-me');
    expect(items[1].attrs.some((a) => a.kind === 'Uid')).toBe(true);
  });

  it('stamps UIDs recursively through struct fields', () => {
    const structDecl: StructDeclaration = {
      struct_name: 'Person',
      fields: new Map([['roles', { kind: 'TArray', value: { kind: 'TInt' } }]]),
    };

    const value: RuntimeValue = {
      value: {
        kind: 'Struct',
        value: [
          structDecl,
          new Map([
            [
              'roles',
              {
                value: {
                  kind: 'Array',
                  value: [{ value: { kind: 'Integer', value: 1 }, attrs: [] }],
                },
                attrs: [],
              },
            ],
          ]),
        ],
      },
      attrs: [],
    };

    const result = ensureArrayUids(value);
    const structData = (
      result.value as {
        kind: 'Struct';
        value: [StructDeclaration, Map<string, RuntimeValue>];
      }
    ).value[1];
    const rolesItems = (
      structData.get('roles')!.value as { kind: 'Array'; value: RuntimeValue[] }
    ).value;

    expect(rolesItems[0].attrs.some((a) => a.kind === 'Uid')).toBe(true);
  });

  it('stamps UIDs recursively through enum payloads', () => {
    const enumDecl: EnumDeclaration = {
      enum_name: 'Result',
      constructors: new Map([
        ['Some', { kind: 'TArray', value: { kind: 'TInt' } }],
      ]),
    };

    const value: RuntimeValue = {
      value: {
        kind: 'Enum',
        value: [
          enumDecl,
          [
            'Some',
            {
              value: {
                value: {
                  kind: 'Array',
                  value: [{ value: { kind: 'Integer', value: 42 }, attrs: [] }],
                },
                attrs: [],
              },
            },
          ],
        ],
      },
      attrs: [],
    };

    const result = ensureArrayUids(value);
    const inner = (
      result.value as {
        kind: 'Enum';
        value: [EnumDeclaration, [string, { value: RuntimeValue } | null]];
      }
    ).value[1][1]!.value;
    const items = (inner.value as { kind: 'Array'; value: RuntimeValue[] })
      .value;

    expect(items[0].attrs.some((a) => a.kind === 'Uid')).toBe(true);
  });

  it('is a no-op for primitive values', () => {
    const value: RuntimeValue = {
      value: { kind: 'Integer', value: 7 },
      attrs: [],
    };

    const result = ensureArrayUids(value);
    expect(result).toBe(value);
  });

  it('handles empty arrays without error', () => {
    const value: RuntimeValue = {
      value: { kind: 'Array', value: [] },
      attrs: [],
    };

    const result = ensureArrayUids(value);
    const items = (result.value as { kind: 'Array'; value: RuntimeValue[] })
      .value;
    expect(items).toHaveLength(0);
  });
});
