/**
 * Tests that editing values in sub-tables correctly propagates changes
 * back to all affected parent rows.
 *
 * Regression test: the non-struct sub-array path previously called
 * handleParentSubArrayUpdate once per parent row, each working against a stale
 * copy of currentArray. When items spanned multiple parents, only the last
 * parent's update survived. Fixed by unifying both paths through
 * handleSubTableChange which batches all parent updates into a single call.
 */

import { describe, it, expect, vi } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import type {
  Typ,
  StructDeclaration,
  RuntimeValue,
  RuntimeValueRaw,
  ValueDef,
} from '../../src/generated/test_case';
import { ArrayEditor } from '../../src/editors/ArrayEditor';
import enMessages from '../../src/locales/en.json';

// ============================================================================
// Test Helpers
// ============================================================================

function rv(raw: RuntimeValueRaw): RuntimeValue {
  return { value: raw, attrs: [{ kind: 'Uid', value: crypto.randomUUID() }] };
}

function intVal(n: number): RuntimeValue {
  return rv({ kind: 'Integer', value: n });
}

function arrayVal(items: RuntimeValue[]): RuntimeValue {
  return rv({ kind: 'Array', value: items });
}

function structVal(
  decl: StructDeclaration,
  fields: Map<string, RuntimeValue>
): RuntimeValue {
  return rv({ kind: 'Struct', value: [decl, fields] });
}

function renderWithOnChange(
  elementType: Typ,
  items: RuntimeValue[]
): { onValueChange: ReturnType<typeof vi.fn> } {
  const onValueChange = vi.fn();
  const valueDef: ValueDef = { value: arrayVal(items) };

  render(
    <IntlProvider locale="en" messages={enMessages}>
      <ArrayEditor
        elementType={elementType}
        valueDef={valueDef}
        onValueChange={onValueChange}
        currentPath={[]}
        diffs={[]}
        editable={true}
      />
    </IntlProvider>
  );

  return { onValueChange };
}

// ============================================================================
// Sub-table edit propagation tests
// ============================================================================

describe('Sub-table edit propagation', () => {
  // Shared type: Person { name: int, scores: int[] }
  const personDecl: StructDeclaration = {
    struct_name: 'Person',
    fields: new Map<string, Typ>([
      ['name', { kind: 'TInt' }],
      ['scores', { kind: 'TArray', value: { kind: 'TInt' } }],
    ]),
  };
  const elementType: Typ = { kind: 'TStruct', value: personDecl };

  // Shared type for struct sub-arrays: Team { name: int, members: Member[] }
  const memberDecl: StructDeclaration = {
    struct_name: 'Member',
    fields: new Map<string, Typ>([['value', { kind: 'TInt' }]]),
  };
  const teamDecl: StructDeclaration = {
    struct_name: 'Team',
    fields: new Map<string, Typ>([
      ['name', { kind: 'TInt' }],
      [
        'members',
        { kind: 'TArray', value: { kind: 'TStruct', value: memberDecl } },
      ],
    ]),
  };
  const teamElementType: Typ = { kind: 'TStruct', value: teamDecl };

  it('non-struct sub-table: edit propagates to correct parent with single parent', () => {
    const rows = [
      structVal(
        personDecl,
        new Map([
          ['name', intVal(1)],
          ['scores', arrayVal([intVal(100), intVal(200)])],
        ])
      ),
    ];

    const { onValueChange } = renderWithOnChange(elementType, rows);

    // Find the score input showing "100" and change it to "999"
    const input = screen.getByDisplayValue('100');
    fireEvent.change(input, { target: { value: '999' } });

    expect(onValueChange).toHaveBeenCalled();
    const newValue = onValueChange.mock.calls[0][0] as RuntimeValue;
    expect(newValue.value.kind).toBe('Array');
    if (newValue.value.kind !== 'Array') return;

    // Parent row should be a Struct with updated scores
    const parentRow = newValue.value.value[0];
    expect(parentRow.value.kind).toBe('Struct');
    if (parentRow.value.kind !== 'Struct') return;

    const scores = parentRow.value.value[1].get('scores');
    expect(scores?.value.kind).toBe('Array');
    if (scores?.value.kind !== 'Array') return;

    // First score should be updated, second unchanged
    expect(scores.value.value[0].value).toEqual({
      kind: 'Integer',
      value: 999,
    });
    expect(scores.value.value[1].value).toEqual({
      kind: 'Integer',
      value: 200,
    });
  });

  it('non-struct sub-table: edits across multiple parents preserve all changes', () => {
    // Two parents, each with scores — items from both appear in the sub-table
    const rows = [
      structVal(
        personDecl,
        new Map([
          ['name', intVal(1)],
          ['scores', arrayVal([intVal(100)])],
        ])
      ),
      structVal(
        personDecl,
        new Map([
          ['name', intVal(2)],
          ['scores', arrayVal([intVal(200)])],
        ])
      ),
    ];

    const { onValueChange } = renderWithOnChange(elementType, rows);

    // The sub-table should show both scores: 100 (from parent 0) and 200 (from parent 1)
    expect(screen.getByDisplayValue('100')).toBeInTheDocument();
    expect(screen.getByDisplayValue('200')).toBeInTheDocument();

    // Edit the first parent's score (100 -> 999)
    const input = screen.getByDisplayValue('100');
    fireEvent.change(input, { target: { value: '999' } });

    expect(onValueChange).toHaveBeenCalled();
    const calls = onValueChange.mock.calls;

    // The non-struct path currently calls updateParent once per parent row.
    // React will use the LAST call, so we must check the last one contains
    // all updates (not just the last parent's).
    const lastCall = calls[calls.length - 1][0] as RuntimeValue;
    expect(lastCall.value.kind).toBe('Array');
    if (lastCall.value.kind !== 'Array') return;

    const [parent0, parent1] = lastCall.value.value;

    // Parent 0's scores should have the update (100 -> 999)
    expect(parent0.value.kind).toBe('Struct');
    if (parent0.value.kind !== 'Struct') return;
    const scores0 = parent0.value.value[1].get('scores');
    expect(scores0?.value.kind).toBe('Array');
    if (scores0?.value.kind !== 'Array') return;
    expect(scores0.value.value[0].value).toEqual({
      kind: 'Integer',
      value: 999,
    });

    // Parent 1's scores should be UNCHANGED
    expect(parent1.value.kind).toBe('Struct');
    if (parent1.value.kind !== 'Struct') return;
    const scores1 = parent1.value.value[1].get('scores');
    expect(scores1?.value.kind).toBe('Array');
    if (scores1?.value.kind !== 'Array') return;
    expect(scores1.value.value[0].value).toEqual({
      kind: 'Integer',
      value: 200,
    });
  });

  it('struct sub-table: edits across multiple parents preserve all changes', () => {
    const rows = [
      structVal(
        teamDecl,
        new Map([
          ['name', intVal(1)],
          [
            'members',
            arrayVal([
              structVal(memberDecl, new Map([['value', intVal(100)]])),
            ]),
          ],
        ])
      ),
      structVal(
        teamDecl,
        new Map([
          ['name', intVal(2)],
          [
            'members',
            arrayVal([
              structVal(memberDecl, new Map([['value', intVal(200)]])),
            ]),
          ],
        ])
      ),
    ];

    const { onValueChange } = renderWithOnChange(teamElementType, rows);

    expect(screen.getByDisplayValue('100')).toBeInTheDocument();
    expect(screen.getByDisplayValue('200')).toBeInTheDocument();

    // Edit first team's member value (100 -> 999)
    const input = screen.getByDisplayValue('100');
    fireEvent.change(input, { target: { value: '999' } });

    expect(onValueChange).toHaveBeenCalled();
    const newValue = onValueChange.mock.calls[0][0] as RuntimeValue;
    expect(newValue.value.kind).toBe('Array');
    if (newValue.value.kind !== 'Array') return;

    const [team0, team1] = newValue.value.value;

    // Team 0's member should have the update
    expect(team0.value.kind).toBe('Struct');
    if (team0.value.kind !== 'Struct') return;
    const members0 = team0.value.value[1].get('members');
    expect(members0?.value.kind).toBe('Array');
    if (members0?.value.kind !== 'Array') return;
    const member0 = members0.value.value[0];
    expect(member0.value.kind).toBe('Struct');
    if (member0.value.kind !== 'Struct') return;
    expect(member0.value.value[1].get('value')?.value).toEqual({
      kind: 'Integer',
      value: 999,
    });

    // Team 1's member should be UNCHANGED
    expect(team1.value.kind).toBe('Struct');
    if (team1.value.kind !== 'Struct') return;
    const members1 = team1.value.value[1].get('members');
    expect(members1?.value.kind).toBe('Array');
    if (members1?.value.kind !== 'Array') return;
    const member1 = members1.value.value[0];
    expect(member1.value.kind).toBe('Struct');
    if (member1.value.kind !== 'Struct') return;
    expect(member1.value.value[1].get('value')?.value).toEqual({
      kind: 'Integer',
      value: 200,
    });
  });
});
