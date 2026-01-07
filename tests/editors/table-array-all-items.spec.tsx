import { describe, it, expect } from 'vitest';
import { render, screen } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import { TableArrayEditor } from '../../src/editors/TableArrayEditor';
import enMessages from '../../src/locales/en.json';
import type {
  RuntimeValue,
  StructDeclaration,
  Typ,
  ValueDef,
} from '../../src/generated/catala_types';

describe('TableArrayEditor - All sub-array items should be visible', () => {
  it('should render all 8 role items across 5 parent persons in the sub-table', () => {
    // Create Rôle enum type
    const roleEnumDeclaration = {
      enum_name: 'Rôle',
      constructors: new Map([
        ['CJT', null],
        ['ENF', null],
        ['PAC', null],
      ]),
    };

    const roleEnumType: Typ = {
      kind: 'TEnum',
      value: roleEnumDeclaration,
    };

    // Create Personne struct with rôles field
    const personneStruct: StructDeclaration = {
      struct_name: 'Personne',
      fields: new Map([
        ['identifiant', { kind: 'TInt' }],
        ['rôles', { kind: 'TArray', value: roleEnumType }],
      ]),
    };

    const personneType: Typ = {
      kind: 'TStruct',
      value: personneStruct,
    };

    // Helper to create a person with N roles
    const createPerson = (id: number, roles: string[]): RuntimeValue => ({
      value: {
        kind: 'Struct',
        value: [
          personneStruct,
          new Map([
            [
              'identifiant',
              {
                value: { kind: 'Integer', value: id },
                attrs: [],
              },
            ],
            [
              'rôles',
              {
                value: {
                  kind: 'Array',
                  value: roles.map((role) => ({
                    value: {
                      kind: 'Enum',
                      value: [roleEnumDeclaration, [role, null]],
                    },
                    attrs: [{ kind: 'Uid', value: `role-${id}-${role}` }],
                  })),
                },
                attrs: [],
              },
            ],
          ]),
        ],
      },
      attrs: [{ kind: 'Uid', value: `person-${id}` }],
    });

    // Create 5 persons with a total of 8 roles:
    // Person 1: 2 roles (CJT, ENF)
    // Person 2: 2 roles (ENF, CJT)
    // Person 3: 0 roles
    // Person 4: 1 role (ENF)
    // Person 5: 3 roles (CJT, ENF, PAC)
    const persons: RuntimeValue[] = [
      createPerson(1, ['CJT', 'ENF']),
      createPerson(2, ['ENF', 'CJT']),
      createPerson(3, []),
      createPerson(4, ['ENF']),
      createPerson(5, ['CJT', 'ENF', 'PAC']),
    ];

    const valueDef: ValueDef = {
      value: {
        value: { kind: 'Array', value: persons },
        attrs: [],
      },
    };

    render(
      <IntlProvider locale="en" messages={enMessages}>
        <TableArrayEditor
          elementType={personneType}
          structType={personneStruct}
          valueDef={valueDef}
          onValueChange={() => {}}
          currentPath={[]}
          diffs={[]}
          editable={true}
        />
      </IntlProvider>
    );

    // Check main table has 5 rows
    const mainTable = screen.getAllByText(/#[1-5]/i);
    expect(mainTable.length).toBeGreaterThanOrEqual(5);

    // Check sub-table header shows count of 8
    // Look for the badge with "8" which appears in the sub-table header
    const badge = screen.getByText('8');
    expect(badge).toBeInTheDocument();

    // Verify the badge is inside a sub-table-header element
    const subTableHeader = badge.parentElement;
    expect(subTableHeader).toHaveClass('sub-table-header');

    // THE BUG: Sub-table should have 8 rows (one for each role)
    // But we previously only saw 5 rows due to filtering out non-struct items
    // Look for all enum dropdowns (which appear as select elements with role="combobox")
    const allSelects = screen.getAllByRole('combobox');

    console.log('Total comboboxes found:', allSelects.length);
    console.log('Expected: 8 (all roles in sub-table)');
    console.log('If old bug existed: 5 (only struct items rendered)');

    // All 8 roles should be rendered as enum selects
    // This assertion should PASS now that the fix is applied
    expect(allSelects.length).toBe(8);
  });
});
