/**
 * A nested carry mark must survive re-renders that do not go through
 * TestPanes: a tab switch, or table-local state.
 */
import React from 'react';
import { describe, it, expect } from 'vitest';
import { render, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import type {
  Recovery,
  StructDeclaration,
  Test,
  TestIo,
  Typ,
} from '../../src/generated/catala_types';
import BrokenTestView from '../../src/test-case-editor/BrokenTestView';
import { arrayVal, intVal, rv, structVal } from './test-helpers';
import enMessages from '../../src/locales/en.json';

const pointDecl: StructDeclaration = {
  struct_name: 'B.Point',
  fields: new Map<string, Typ>([
    ['x', { kind: 'TInt' }],
    ['y', { kind: 'TInt' }],
  ]),
};
const pointTyp: Typ = { kind: 'TStruct', value: pointDecl };
const pointsTyp: Typ = { kind: 'TArray', value: pointTyp };

function io(typ: Typ, value?: TestIo['value']): TestIo {
  return { typ, value };
}

function test(): Test {
  const holed = structVal(
    pointDecl,
    new Map([
      ['x', intVal(1)],
      ['y', rv({ kind: 'Unset' })],
    ])
  );
  return {
    testing_scope: 'T',
    tested_scope: {
      module_name: 'B',
      name: 'C',
      inputs: new Map([
        ['points', { typ: pointsTyp, is_context: false }],
        ['others', { typ: pointsTyp, is_context: false }],
      ]),
      outputs: new Map(),
      module_deps: [],
    },
    test_inputs: new Map<string, TestIo>([
      ['points', io(pointsTyp, { value: arrayVal([holed]) })],
      ['others', io(pointsTyp, { value: arrayVal([]) })],
    ]),
    test_outputs: new Map(),
    description: '',
    title: '',
  };
}

function view(): Recovery {
  return {
    tests: [
      {
        authored: test(),
        rebuilt: test(),
        outcomes: [
          {
            path: [{ kind: 'StructField', value: 'points' }],
            side: { kind: 'In' },
            outcome: { kind: 'Partial' },
          },
          {
            path: [
              { kind: 'StructField', value: 'points' },
              { kind: 'ListIndex', value: 0 },
              { kind: 'StructField', value: 'y' },
            ],
            side: { kind: 'In' },
            outcome: { kind: 'WasUnset' },
          },
        ],
      },
    ],
    notes: [],
    working_copy: 'x.repair',
  };
}

describe('nested carry marks under a subtree re-render', () => {
  it('keeps the mark on points[0].y after switching tabs and back', () => {
    const { container } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <BrokenTestView view={view()} onRun={(): void => {}} />
      </IntlProvider>
    );
    const rebuilt = container.querySelector('.broken-pane-rebuilt')!;
    const marksBefore = rebuilt.querySelectorAll('.carry-marked .fate-mark');
    expect(marksBefore.length).toBe(1);

    // A tab switch re-renders CompositeEditor and every editor under it,
    // without going through TestPanes.
    const tabs = Array.from(rebuilt.querySelectorAll('button.tab'));
    const others = tabs.find((t) => t.textContent?.includes('others'))!;
    const points = tabs.find((t) => t.textContent?.includes('points'))!;
    fireEvent.click(others);
    fireEvent.click(points);

    const marksAfter = rebuilt.querySelectorAll('.carry-marked .fate-mark');
    expect(marksAfter.length).toBe(1);
  });

  it('keeps the mark after the table re-renders from its own state', () => {
    const { container } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <BrokenTestView view={view()} onRun={(): void => {}} />
      </IntlProvider>
    );
    const rebuilt = container.querySelector('.broken-pane-rebuilt')!;
    expect(rebuilt.querySelectorAll('.carry-marked .fate-mark').length).toBe(1);

    // Right-clicking a row header opens the row context menu: table-local
    // state, so TableArrayEditor re-renders its cells without TestPanes.
    const rowControls = rebuilt.querySelector('.table-cell-controls')!;
    fireEvent.contextMenu(rowControls, { clientX: 10, clientY: 10 });

    expect(rebuilt.querySelectorAll('.carry-marked .fate-mark').length).toBe(1);
  });
});
