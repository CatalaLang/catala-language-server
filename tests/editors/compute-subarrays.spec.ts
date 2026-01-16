import { describe, it, expect } from 'vitest';
import type {
  RuntimeValue,
  StructDeclaration,
  Typ,
} from '../../src/generated/catala_types';
import { computeSubArrays } from '../../src/editors/tableArrayUtils';

describe('computeSubArrays - All items should be collected', () => {
  it('should collect all 8 role items from 5 parent persons (2+2+0+1+3=8)', () => {
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
                    .map((_, idx) => ({
                      value: { kind: 'Integer', value: idx },
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
      createPerson(0), // Person #3: 0 roles (should not contribute)
      createPerson(1), // Person #4: 1 role
      createPerson(3), // Person #5: 3 roles
    ];

    const arrayFields = [
      {
        label: 'rôles',
        fieldPath: ['rôles'],
        arrayType: {
          kind: 'TArray' as const,
          value: { kind: 'TInt' as const },
        },
      },
    ];

    const result = computeSubArrays(rows, arrayFields, [], []);

    // Should have one entry for the "rôles" field
    expect(result.length).toBe(1);
    expect(result[0].label).toBe('rôles');

    // BUG TEST: Should have 8 total items (2+2+0+1+3=8)
    // If the bug exists, we might only get 5 items
    const items = result[0].items;

    console.log('Items collected:', items.length);
    console.log(
      'Items by parent:',
      items.map((i) => `parent${i.parentRowIndex}[${i.itemIndex}]`).join(', ')
    );

    // Expected distribution:
    // Parent 0 (Person #1): items 0,1 (2 items)
    // Parent 1 (Person #2): items 0,1 (2 items)
    // Parent 2 (Person #3): (0 items)
    // Parent 3 (Person #4): item 0 (1 item)
    // Parent 4 (Person #5): items 0,1,2 (3 items)
    // Total: 8 items

    expect(items.length).toBe(8);

    // Verify correct parent assignment
    const itemsByParent = items.reduce(
      (acc, item) => {
        acc[item.parentRowIndex] = (acc[item.parentRowIndex] || 0) + 1;
        return acc;
      },
      {} as Record<number, number>
    );

    expect(itemsByParent[0]).toBe(2); // Person #1 has 2 roles
    expect(itemsByParent[1]).toBe(2); // Person #2 has 2 roles
    expect(itemsByParent[2]).toBeUndefined(); // Person #3 has 0 roles
    expect(itemsByParent[3]).toBe(1); // Person #4 has 1 role
    expect(itemsByParent[4]).toBe(3); // Person #5 has 3 roles
  });
});
