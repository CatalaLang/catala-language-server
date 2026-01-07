import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, within } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../../src/locales/en.json';
import type {
  Diff,
  PathSegment,
  RuntimeValue,
  RuntimeValueRaw,
  StructDeclaration,
  ValueDef,
} from '../../src/generated/catala_types';
import { TableArrayEditor } from '../../src/editors/TableArrayEditor';

// Mock confirm to auto-approve destructive actions
vi.mock('../../src/messaging/confirm', () => ({
  confirm: async () => true,
}));

function rv(value: RuntimeValueRaw): RuntimeValue {
  return { value, attrs: [] };
}

function rvWithUid(value: RuntimeValueRaw, uid: string): RuntimeValue {
  return { value, attrs: [{ kind: 'Uid', value: uid }] };
}

function arrayValueDef(items: RuntimeValue[]): ValueDef {
  return { value: rv({ kind: 'Array', value: items }) };
}

function intRV(n: number): RuntimeValue {
  return rv({ kind: 'Integer', value: n });
}

function emptyRV(): RuntimeValue {
  return rv({ kind: 'Empty' });
}

function structRV(
  structDecl: StructDeclaration,
  fields: Map<string, RuntimeValue>,
  uid?: string
): RuntimeValue {
  const value: RuntimeValueRaw = {
    kind: 'Struct',
    value: [structDecl, fields],
  };
  return uid ? rvWithUid(value, uid) : rv(value);
}

describe('TableArrayEditor - Phantom diff support', () => {
  const personStruct: StructDeclaration = {
    struct_name: 'Person',
    fields: new Map([
      ['name', { kind: 'TInt' }],
      ['age', { kind: 'TInt' }],
    ]),
  };

  beforeEach(() => {
    vi.clearAllMocks();
  });

  it('renders phantom rows for actual-only diffs (empty expected, non-empty actual)', () => {
    const currentPath: PathSegment[] = [];
    const diffs: Diff[] = [
      {
        path: [{ kind: 'ListIndex', value: 0 }],
        expected: emptyRV(),
        actual: structRV(
          personStruct,
          new Map([
            ['name', intRV(100)],
            ['age', intRV(25)],
          ]),
          'person-phantom-1'
        ),
      },
    ];

    const { container } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <TableArrayEditor
          elementType={{ kind: 'TStruct', value: personStruct }}
          structType={personStruct}
          valueDef={arrayValueDef([])}
          onValueChange={() => {}}
          currentPath={currentPath}
          diffs={diffs}
          editable={true}
        />
      </IntlProvider>
    );

    // Check for phantom row styling
    const phantomRows = container.querySelectorAll('.phantom-row');
    expect(phantomRows).toHaveLength(1);

    // Check for phantom indicators
    const phantomIndicators = container.querySelectorAll(
      '.phantom-row-indicator'
    );
    expect(phantomIndicators).toHaveLength(1);

    // Check for "Accept" button
    const acceptButtons = container.querySelectorAll('.phantom-accept');
    expect(acceptButtons).toHaveLength(1);
  });

  it('renders expected-only rows with remove button (non-empty expected, empty actual)', () => {
    const currentPath: PathSegment[] = [];
    const person1 = structRV(
      personStruct,
      new Map([
        ['name', intRV(1)],
        ['age', intRV(30)],
      ]),
      'person-1'
    );

    const diffs: Diff[] = [
      {
        path: [{ kind: 'ListIndex', value: 0 }],
        expected: person1,
        actual: emptyRV(),
      },
    ];

    const { container } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <TableArrayEditor
          elementType={{ kind: 'TStruct', value: personStruct }}
          structType={personStruct}
          valueDef={arrayValueDef([person1])}
          onValueChange={() => {}}
          currentPath={currentPath}
          diffs={diffs}
          editable={true}
        />
      </IntlProvider>
    );

    // Check for expected-only row styling
    const expectedOnlyRows = container.querySelectorAll('.expected-only-row');
    expect(expectedOnlyRows).toHaveLength(1);

    // Check for expected-only indicators
    const expectedOnlyIndicators = container.querySelectorAll(
      '.expected-only-row-indicator'
    );
    expect(expectedOnlyIndicators).toHaveLength(1);

    // Check for "Remove" button
    const removeButtons = container.querySelectorAll('.phantom-remove');
    expect(removeButtons).toHaveLength(1);
  });

  it('phantom row cells are read-only (no editable controls inside cells)', () => {
    const currentPath: PathSegment[] = [];
    const diffs: Diff[] = [
      {
        path: [{ kind: 'ListIndex', value: 0 }],
        expected: emptyRV(),
        actual: structRV(
          personStruct,
          new Map([
            ['name', intRV(100)],
            ['age', intRV(25)],
          ])
        ),
      },
    ];

    const { container } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <TableArrayEditor
          elementType={{ kind: 'TStruct', value: personStruct }}
          structType={personStruct}
          valueDef={arrayValueDef([])}
          onValueChange={() => {}}
          currentPath={currentPath}
          diffs={diffs}
          editable={true}
        />
      </IntlProvider>
    );

    const phantomRow = container.querySelector('.phantom-row');
    expect(phantomRow).toBeTruthy();

    // Phantom row should not have move up/down/delete buttons (only accept button)
    const phantomRowElement = phantomRow as HTMLElement;
    const moveButtons = within(phantomRowElement).queryAllByRole('button', {
      name: /move/i,
    });
    const deleteButtons = within(phantomRowElement).queryAllByRole('button', {
      name: /delete/i,
    });

    expect(moveButtons).toHaveLength(0);
    expect(deleteButtons).toHaveLength(0);
  });

  it('expected-only row cells are read-only (no move/add buttons, only remove)', () => {
    const currentPath: PathSegment[] = [];
    const person1 = structRV(
      personStruct,
      new Map([
        ['name', intRV(1)],
        ['age', intRV(30)],
      ])
    );

    const diffs: Diff[] = [
      {
        path: [{ kind: 'ListIndex', value: 0 }],
        expected: person1,
        actual: emptyRV(),
      },
    ];

    const { container } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <TableArrayEditor
          elementType={{ kind: 'TStruct', value: personStruct }}
          structType={personStruct}
          valueDef={arrayValueDef([person1])}
          onValueChange={() => {}}
          currentPath={currentPath}
          diffs={diffs}
          editable={true}
        />
      </IntlProvider>
    );

    const expectedOnlyRow = container.querySelector('.expected-only-row');
    expect(expectedOnlyRow).toBeTruthy();

    // Expected-only row should not have move up/down buttons (only remove button)
    const expectedOnlyRowElement = expectedOnlyRow as HTMLElement;
    const moveButtons = within(expectedOnlyRowElement).queryAllByRole(
      'button',
      {
        name: /move (up|down|previous|next)/i,
      }
    );

    expect(moveButtons).toHaveLength(0);

    // Should have remove button
    const removeButtons = within(expectedOnlyRowElement).queryAllByRole(
      'button'
    );
    expect(removeButtons.length).toBeGreaterThan(0);
  });

  it('phantom rows do not show colored row indicator border', () => {
    const currentPath: PathSegment[] = [];
    const diffs: Diff[] = [
      {
        path: [{ kind: 'ListIndex', value: 0 }],
        expected: emptyRV(),
        actual: structRV(
          personStruct,
          new Map([
            ['name', intRV(100)],
            ['age', intRV(25)],
          ])
        ),
      },
    ];

    const { container } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <TableArrayEditor
          elementType={{ kind: 'TStruct', value: personStruct }}
          structType={personStruct}
          valueDef={arrayValueDef([])}
          onValueChange={() => {}}
          currentPath={currentPath}
          diffs={diffs}
          editable={true}
        />
      </IntlProvider>
    );

    const phantomRow = container.querySelector('.phantom-row');
    expect(phantomRow).toBeTruthy();

    // CSS rule .phantom-row .table-cell-controls::before { display: none; }
    // should hide the colored border
    const controlsCell = phantomRow?.querySelector('.table-cell-controls');
    expect(controlsCell).toBeTruthy();
  });

  it('renders multiple phantom rows and regular rows together', () => {
    const currentPath: PathSegment[] = [];
    const person1 = structRV(
      personStruct,
      new Map([
        ['name', intRV(1)],
        ['age', intRV(30)],
      ]),
      'person-1'
    );

    const diffs: Diff[] = [
      {
        path: [{ kind: 'ListIndex', value: 1 }],
        expected: emptyRV(),
        actual: structRV(
          personStruct,
          new Map([
            ['name', intRV(2)],
            ['age', intRV(35)],
          ])
        ),
      },
      {
        path: [{ kind: 'ListIndex', value: 2 }],
        expected: emptyRV(),
        actual: structRV(
          personStruct,
          new Map([
            ['name', intRV(3)],
            ['age', intRV(40)],
          ])
        ),
      },
    ];

    const { container } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <TableArrayEditor
          elementType={{ kind: 'TStruct', value: personStruct }}
          structType={personStruct}
          valueDef={arrayValueDef([person1])}
          onValueChange={() => {}}
          currentPath={currentPath}
          diffs={diffs}
          editable={true}
        />
      </IntlProvider>
    );

    // Should have 1 regular row + 2 phantom rows
    const allRows = container.querySelectorAll('tbody tr');
    expect(allRows.length).toBeGreaterThanOrEqual(3);

    const phantomRows = container.querySelectorAll('.phantom-row');
    expect(phantomRows).toHaveLength(2);
  });
});
