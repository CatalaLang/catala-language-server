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

describe('TableArrayEditor - Main table should render all items including Impossible/Unset/Invalid', () => {
  it('should render all 8 parent items in main table (5 computed + 3 impossible)', () => {
    // Create Personne struct
    const personneStruct: StructDeclaration = {
      struct_name: 'PersonneFiltrée',
      fields: new Map([['identifiant', { kind: 'TInt' }]]),
    };

    const personneType: Typ = {
      kind: 'TStruct',
      value: personneStruct,
    };

    // Helper to create a computed person
    const createComputedPerson = (id: number): RuntimeValue => ({
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
          ]),
        ],
      },
      attrs: [{ kind: 'Uid', value: `person-${id}` }],
    });

    // Helper to create an impossible person
    const createImpossiblePerson = (uid: string): RuntimeValue => ({
      value: { kind: 'Impossible' },
      attrs: [{ kind: 'Uid', value: uid }],
    });

    // Create 5 computed persons + 3 impossible persons = 8 total
    // This mirrors the actual test data from the Catala buffer
    const persons: RuntimeValue[] = [
      createComputedPerson(1),
      createComputedPerson(2),
      createComputedPerson(3),
      createComputedPerson(4),
      createComputedPerson(5),
      createImpossiblePerson('740de45d-eb83-4786-9f61-b0879de0fec2'),
      createImpossiblePerson('1d0a5f20-c849-483a-8d9d-241643088c8e'),
      createImpossiblePerson('d3f3c02b-c61d-47bd-91ef-2be61c623246'),
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

    // BUG: Main table should have 8 rows (one for each person)
    // But due to the filter on line 547, we only see 5 (the computed structs)

    // Count rows by finding all row label inputs (one per top-level row)
    const rowLabelInputs = screen.queryAllByPlaceholderText(/name/i);

    console.log('Row label inputs found:', rowLabelInputs.length);
    console.log('Expected: 8 (5 computed + 3 impossible)');
    console.log('If bug exists: only 5 (computed structs only)');

    // This assertion should FAIL before the fix
    // After fix, it should PASS
    expect(rowLabelInputs.length).toBeGreaterThanOrEqual(8);
  });
});
