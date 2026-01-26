import { describe, it, expect } from 'vitest';
import type {
  RuntimeValue,
  StructDeclaration,
} from '../../src/generated/catala_types';
import { cloneWithNewUid } from '../../src/editors/tableArrayUtils';

describe('cloneWithNewUid', () => {
  it('should regenerate UID at the top level', () => {
    const original: RuntimeValue = {
      value: { kind: 'Integer', value: 42 },
      attrs: [{ kind: 'Uid', value: 'original-uid' }],
    };

    const cloned = cloneWithNewUid(original);

    // Top-level UID should be different
    const originalUid = original.attrs?.find((a) => a.kind === 'Uid');
    const clonedUid = cloned.attrs?.find((a) => a.kind === 'Uid');

    expect(clonedUid).toBeDefined();
    expect(clonedUid?.value).not.toBe(originalUid?.value);
  });

  it('should regenerate UIDs for nested array items (REGRESSION: duplicate key warning)', () => {
    // This test reproduces the bug where duplicating a parent row
    // causes duplicate key warnings in sub-tables because nested
    // array items keep their original UIDs.

    const structDecl: StructDeclaration = {
      struct_name: 'Person',
      fields: new Map([['roles', { kind: 'TArray', value: { kind: 'TInt' } }]]),
    };

    // Create a person with 2 roles, each role has its own UID
    const original: RuntimeValue = {
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
                  value: [
                    {
                      value: { kind: 'Integer', value: 1 },
                      attrs: [{ kind: 'Uid', value: 'role-uid-1' }],
                    },
                    {
                      value: { kind: 'Integer', value: 2 },
                      attrs: [{ kind: 'Uid', value: 'role-uid-2' }],
                    },
                  ],
                },
                attrs: [],
              },
            ],
          ]),
        ],
      },
      attrs: [{ kind: 'Uid', value: 'person-uid' }],
    };

    const cloned = cloneWithNewUid(original);

    // Extract UIDs from original
    const originalPersonUid = original.attrs?.find(
      (a) => a.kind === 'Uid'
    )?.value;
    const originalRoles = (original.value as any).value[1].get('roles').value
      .value;
    const originalRoleUid1 = originalRoles[0].attrs?.find(
      (a: any) => a.kind === 'Uid'
    )?.value;
    const originalRoleUid2 = originalRoles[1].attrs?.find(
      (a: any) => a.kind === 'Uid'
    )?.value;

    // Extract UIDs from clone
    const clonedPersonUid = cloned.attrs?.find((a) => a.kind === 'Uid')?.value;
    const clonedRoles = (cloned.value as any).value[1].get('roles').value.value;
    const clonedRoleUid1 = clonedRoles[0].attrs?.find(
      (a: any) => a.kind === 'Uid'
    )?.value;
    const clonedRoleUid2 = clonedRoles[1].attrs?.find(
      (a: any) => a.kind === 'Uid'
    )?.value;

    // Top-level person UID should be different (this passes)
    expect(clonedPersonUid).not.toBe(originalPersonUid);

    // Nested role UIDs should ALSO be different (this currently FAILS - the bug!)
    // When this fails, it means sub-table will have duplicate keys after duplication
    expect(clonedRoleUid1).not.toBe(originalRoleUid1);
    expect(clonedRoleUid2).not.toBe(originalRoleUid2);
  });

  it('should regenerate UIDs for deeply nested structures', () => {
    // Test that UIDs are regenerated at all levels of nesting

    const innerStructDecl: StructDeclaration = {
      struct_name: 'Address',
      fields: new Map([['city', { kind: 'TInt' }]]),
    };

    const outerStructDecl: StructDeclaration = {
      struct_name: 'Person',
      fields: new Map([
        [
          'addresses',
          {
            kind: 'TArray',
            value: { kind: 'TStruct', value: innerStructDecl },
          },
        ],
      ]),
    };

    const original: RuntimeValue = {
      value: {
        kind: 'Struct',
        value: [
          outerStructDecl,
          new Map([
            [
              'addresses',
              {
                value: {
                  kind: 'Array',
                  value: [
                    {
                      value: {
                        kind: 'Struct',
                        value: [
                          innerStructDecl,
                          new Map([
                            [
                              'city',
                              {
                                value: { kind: 'Integer', value: 1 },
                                attrs: [],
                              },
                            ],
                          ]),
                        ],
                      },
                      attrs: [{ kind: 'Uid', value: 'address-uid-1' }],
                    },
                  ],
                },
                attrs: [],
              },
            ],
          ]),
        ],
      },
      attrs: [{ kind: 'Uid', value: 'person-uid' }],
    };

    const cloned = cloneWithNewUid(original);

    // Extract nested address UID
    const originalAddresses = (original.value as any).value[1].get('addresses')
      .value.value;
    const originalAddressUid = originalAddresses[0].attrs?.find(
      (a: any) => a.kind === 'Uid'
    )?.value;

    const clonedAddresses = (cloned.value as any).value[1].get('addresses')
      .value.value;
    const clonedAddressUid = clonedAddresses[0].attrs?.find(
      (a: any) => a.kind === 'Uid'
    )?.value;

    // Nested struct UID should be different
    expect(clonedAddressUid).not.toBe(originalAddressUid);
  });
});
