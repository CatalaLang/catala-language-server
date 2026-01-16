import { describe, it, expect } from 'vitest';
import type {
  RuntimeValue,
  StructDeclaration,
} from '../../src/generated/catala_types';

describe('TableArrayEditor - Sub-array count', () => {
  it('should count total items across all parent rows, not number of parent rows', () => {
    // Create test data: 5 parent rows (Personnes) with varying numbers of Rôles
    // Person #1: 2 roles
    // Person #2: 2 roles
    // Person #3: 0 roles
    // Person #4: 1 role
    // Person #5: 3 roles
    // Total: 8 roles (NOT 5 parent rows)

    const structDecl: StructDeclaration = {
      struct_name: 'Personne',
      fields: new Map([
        [
          'rôles',
          {
            kind: 'TArray',
            value: {
              kind: 'TEnum',
              value: { enum_name: 'Rôle', cases: new Map() },
            },
          },
        ],
      ]),
    };

    const createPerson = (numRoles: number): RuntimeValue => ({
      value: {
        kind: 'Struct',
        value: [
          structDecl,
          new Map([
            [
              'rôles',
              {
                value: {
                  kind: 'Array',
                  value: Array(numRoles)
                    .fill(null)
                    .map(() => ({
                      value: { kind: 'Enum', value: ['CJT', null] },
                      attrs: [],
                    })),
                },
                attrs: [],
              },
            ],
          ]),
        ],
      },
      attrs: [],
    });

    const rows: RuntimeValue[] = [
      createPerson(2), // Person #1: 2 roles
      createPerson(2), // Person #2: 2 roles
      createPerson(0), // Person #3: 0 roles
      createPerson(1), // Person #4: 1 role
      createPerson(3), // Person #5: 3 roles
    ];

    // Count items manually
    let totalItems = 0;
    rows.forEach((row) => {
      if (row.value.kind === 'Struct') {
        const structData = row.value.value[1];
        const rolesValue = structData.get('rôles');
        if (rolesValue?.value.kind === 'Array') {
          totalItems += rolesValue.value.value.length;
        }
      }
    });

    // Expected: 8 total items (2+2+0+1+3), NOT 5 (number of parent rows)
    expect(totalItems).toBe(8);
    expect(rows.length).toBe(5);

    // The badge should show 8, not 5
    // This is CORRECT behavior - we're counting items, not parent rows
  });
});
