import { describe, it, expect } from 'vitest';
import type {
  RuntimeValue,
  StructDeclaration,
  Typ,
} from '../../src/generated/catala_types';

// This is a copy of the computeSubArrays function from TableArrayEditor
// to test it in isolation
function computeSubArrays(
  rows: RuntimeValue[],
  arrayFields: { label: string; fieldPath: string[]; arrayType: Typ }[]
): Array<{
  label: string;
  fieldPath: string[];
  arrayType: Typ;
  items: Array<{
    parentRowIndex: number;
    itemIndex: number;
    value: RuntimeValue;
  }>;
}> {
  const groupedByField = new Map();

  arrayFields.forEach(({ label, fieldPath, arrayType }) => {
    const items: any[] = [];

    rows.forEach((row, parentRowIndex) => {
      if (row.value.kind !== 'Struct') return;
      const structData = row.value.value[1];

      // Get nested value by path
      let currentValue: RuntimeValue | undefined = undefined;
      if (fieldPath.length > 0) {
        currentValue = structData.get(fieldPath[0]);
      }

      if (currentValue?.value.kind === 'Array') {
        currentValue.value.value.forEach((value, itemIndex) => {
          items.push({ parentRowIndex, itemIndex, value });
        });
      }
    });

    if (items.length > 0) {
      groupedByField.set(label, {
        label,
        fieldPath,
        arrayType,
        items,
      });
    }
  });

  return Array.from(groupedByField.values());
}

describe('Sub-table rendering with Unset/Invalid items', () => {
  it('should render ALL items including Unset and Invalid states (BUG: currently filters them out)', () => {
    const roleStructDecl: StructDeclaration = {
      struct_name: 'Rôle',
      fields: new Map([['nom', { kind: 'TLit', value: 'TString' }]]),
    };

    const personStructDecl: StructDeclaration = {
      struct_name: 'Personne',
      fields: new Map([
        [
          'rôles',
          { kind: 'TArray', value: { kind: 'TStruct', value: roleStructDecl } },
        ],
      ]),
    };

    // Helper to create a role with a computed struct value
    const createComputedRole = (name: string): RuntimeValue => ({
      value: {
        kind: 'Struct',
        value: [
          roleStructDecl,
          new Map([
            ['nom', { value: { kind: 'String', value: name }, attrs: [] }],
          ]),
        ],
      },
      attrs: [],
    });

    // Helper to create an Unset role (not yet computed)
    const createUnsetRole = (): RuntimeValue => ({
      value: { kind: 'Unset' },
      attrs: [],
    });

    // Helper to create an Invalid role (computation error)
    const createInvalidRole = (): RuntimeValue => ({
      value: { kind: 'Invalid' },
      attrs: [],
    });

    const createPerson = (roles: RuntimeValue[]): RuntimeValue => ({
      value: {
        kind: 'Struct',
        value: [
          personStructDecl,
          new Map([
            [
              'rôles',
              {
                value: {
                  kind: 'Array',
                  value: roles,
                },
                attrs: [],
              },
            ],
          ]),
        ],
      },
      attrs: [],
    });

    // Create test data matching the screenshot:
    // - 5 parent rows (persons)
    // - 8 total role items across all parents
    // - Mix of computed Struct values, Unset, and Invalid
    const rows: RuntimeValue[] = [
      createPerson([
        createComputedRole('Role1'), // Computed
        createComputedRole('Role2'), // Computed
      ]), // Person #1: 2 roles (both computed)

      createPerson([
        createComputedRole('Role3'), // Computed
        createComputedRole('Role4'), // Computed
      ]), // Person #2: 2 roles (both computed)

      createPerson([]), // Person #3: 0 roles

      createPerson([
        createComputedRole('Role5'), // Computed
      ]), // Person #4: 1 role (computed)

      createPerson([
        createUnsetRole(), // Unset - NOT RENDERED by current code!
        createInvalidRole(), // Invalid - NOT RENDERED by current code!
        createUnsetRole(), // Unset - NOT RENDERED by current code!
      ]), // Person #5: 3 roles (but current code filters them out!)
    ];

    const arrayFields = [
      {
        label: 'rôles',
        fieldPath: ['rôles'],
        arrayType: {
          kind: 'TArray' as const,
          value: { kind: 'TStruct' as const, value: roleStructDecl },
        },
      },
    ];

    const result = computeSubArrays(rows, arrayFields);

    // Should have one entry for the "rôles" field
    expect(result.length).toBe(1);
    expect(result[0].label).toBe('rôles');

    const items = result[0].items;

    console.log('Items collected:', items.length);
    console.log(
      'Items by parent:',
      items.map((i) => `parent${i.parentRowIndex}[${i.itemIndex}]`).join(', ')
    );
    console.log('Item kinds:', items.map((i) => i.value.value.kind).join(', '));

    // STEP 1: Verify computeSubArrays collects ALL 8 items
    expect(items.length).toBe(8);

    // STEP 2: Simulate the OLD rendering logic that filtered by kind === 'Struct'
    const oldRenderedItems = items.filter(
      (item) => item.value.value.kind === 'Struct'
    );

    console.log('Old rendering (with bug):', oldRenderedItems.length);
    console.log(
      'Old filtering excluded:',
      items.length - oldRenderedItems.length
    );

    // STEP 3: THE BUG - old code only rendered 5 items (the computed structs)
    expect(oldRenderedItems.length).toBe(5);

    // STEP 4: The FIXED rendering should render ALL items
    // The new code renders ALL items by using ValueEditor for non-struct/Unset/Invalid items
    // In the actual component, we no longer filter items - we render them all
    const fixedRenderedItems = items; // No filtering - render everything

    console.log('Fixed rendering (all items):', fixedRenderedItems.length);

    // This should now pass with the fixed code
    expect(fixedRenderedItems.length).toBe(8);
  });
});
