import { describe, it, expect } from 'vitest';
import { render, screen } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import { TableArrayEditor } from '../../src/editors/TableArrayEditor';
import { tryCreateTableSchema } from '../../src/editors/tableArrayUtils';
import enMessages from '../../src/locales/en.json';
import type {
  RuntimeValue,
  StructDeclaration,
  Typ,
  ValueDef,
} from '../../src/generated/catala_types';

describe('TableArrayEditor - Sub-table move button disabled state', () => {
  it('should disable move-down button for last item within each parent, not just last item overall', () => {
    // Create Role struct (sub-array item)
    const roleStruct: StructDeclaration = {
      struct_name: 'Role',
      fields: new Map([['description', { kind: 'TInt' }]]),
    };

    // Create Person struct with roles sub-array
    const personStruct: StructDeclaration = {
      struct_name: 'Person',
      fields: new Map([
        ['name', { kind: 'TInt' }],
        [
          'roles',
          { kind: 'TArray', value: { kind: 'TStruct', value: roleStruct } },
        ],
      ]),
    };

    const personType: Typ = {
      kind: 'TStruct',
      value: personStruct,
    };

    // Helper to create a role
    const createRole = (desc: number): RuntimeValue => ({
      value: {
        kind: 'Struct',
        value: [
          roleStruct,
          new Map([
            [
              'description',
              { value: { kind: 'Integer', value: desc }, attrs: [] },
            ],
          ]),
        ],
      },
      attrs: [{ kind: 'Uid', value: `role-${desc}` }],
    });

    // Helper to create a person with roles
    const createPerson = (id: number, roles: RuntimeValue[]): RuntimeValue => ({
      value: {
        kind: 'Struct',
        value: [
          personStruct,
          new Map([
            ['name', { value: { kind: 'Integer', value: id }, attrs: [] }],
            ['roles', { value: { kind: 'Array', value: roles }, attrs: [] }],
          ]),
        ],
      },
      attrs: [{ kind: 'Uid', value: `person-${id}` }],
    });

    // Create 2 persons:
    // - Person 1 has 2 roles (indices 0, 1 in flattened array)
    // - Person 2 has 2 roles (indices 2, 3 in flattened array)
    const persons: RuntimeValue[] = [
      createPerson(1, [createRole(10), createRole(11)]), // 2 roles
      createPerson(2, [createRole(20), createRole(21)]), // 2 roles
    ];

    const valueDef: ValueDef = {
      value: {
        value: { kind: 'Array', value: persons },
        attrs: [],
      },
    };

    const schemaResult = tryCreateTableSchema(personType);
    if (!schemaResult.ok) throw new Error('Expected schema to be ok');

    render(
      <IntlProvider locale="en" messages={enMessages}>
        <TableArrayEditor
          elementType={personType}
          schema={schemaResult.schema}
          valueDef={valueDef}
          onValueChange={() => {}}
          currentPath={[]}
          diffs={[]}
          editable={true}
        />
      </IntlProvider>
    );

    // Find all move-down buttons in the sub-table (roles table)
    // The sub-table should have 4 rows total (2 roles per person × 2 persons)
    const moveDownButtons = screen.getAllByTitle(
      'Move element to next position'
    );

    // We expect 4 move-down buttons (one per role row in sub-table)
    // Plus 2 from main table = 6 total? Let's check what we get
    console.log('Move-down buttons found:', moveDownButtons.length);

    // Filter to get only sub-table buttons by checking parent elements
    // Sub-table rows have class 'sub-table-row'
    const subTableMoveDownButtons = moveDownButtons.filter((btn) => {
      const row = btn.closest('tr');
      return row?.classList.contains('sub-table-row');
    });

    console.log('Sub-table move-down buttons:', subTableMoveDownButtons.length);

    // We should have 4 sub-table move-down buttons
    expect(subTableMoveDownButtons.length).toBe(4);

    // Check disabled states:
    // - Row 0 (Person 1, Role 0): should be ENABLED (not last in parent's sub-array)
    // - Row 1 (Person 1, Role 1): should be DISABLED (last in Person 1's sub-array)
    // - Row 2 (Person 2, Role 0): should be ENABLED (not last in parent's sub-array)
    // - Row 3 (Person 2, Role 1): should be DISABLED (last in Person 2's sub-array)

    const disabledStates = subTableMoveDownButtons.map((btn) =>
      btn.hasAttribute('disabled')
    );
    console.log('Disabled states:', disabledStates);
    console.log('Expected: [false, true, false, true]');

    // BUG: With the buggy code, only the very last button (index 3) would be disabled
    // because it checks rowIndex === currentArray.length - 1 (3 === 3)
    // But button at index 1 should also be disabled (last item in Person 1's sub-array)

    // Button 0: Person 1's first role - should be enabled
    expect(subTableMoveDownButtons[0].hasAttribute('disabled')).toBe(false);

    // Button 1: Person 1's last role - should be DISABLED
    // THIS IS THE KEY ASSERTION THAT EXPOSES THE BUG
    expect(subTableMoveDownButtons[1].hasAttribute('disabled')).toBe(true);

    // Button 2: Person 2's first role - should be enabled
    expect(subTableMoveDownButtons[2].hasAttribute('disabled')).toBe(false);

    // Button 3: Person 2's last role - should be disabled
    expect(subTableMoveDownButtons[3].hasAttribute('disabled')).toBe(true);
  });
});
