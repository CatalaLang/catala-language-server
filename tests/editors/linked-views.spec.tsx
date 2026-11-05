import React from 'react';
import { describe, it, expect, vi } from 'vitest';
import { render, screen, fireEvent, within } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import type {
  RuntimeValue,
  StructDeclaration,
  Typ,
} from '../../src/generated/catala_types';
import ValueEditor from '../../src/editors/ValueEditors';
import {
  LinkedViewsContext,
  useLinkedViewsState,
} from '../../src/editors/reveal';
import { arrayVal, intVal, structVal } from './test-helpers';
import enMessages from '../../src/locales/en.json';

const kidDecl: StructDeclaration = {
  struct_name: 'Kid',
  fields: new Map<string, Typ>([['age', { kind: 'TInt' }]]),
};
const kidTyp: Typ = { kind: 'TStruct', value: kidDecl };
const familyDecl: StructDeclaration = {
  struct_name: 'Family',
  fields: new Map<string, Typ>([
    ['children', { kind: 'TArray', value: kidTyp }],
    ['pets', { kind: 'TArray', value: kidTyp }],
  ]),
};
const familyTyp: Typ = { kind: 'TStruct', value: familyDecl };
const kid = (age: number): RuntimeValue =>
  structVal(kidDecl, new Map([['age', intVal(age)]]));
const family = structVal(
  familyDecl,
  new Map([
    ['children', arrayVal([kid(3)])],
    ['pets', arrayVal([kid(5)])],
  ])
);

const bobDecl: StructDeclaration = {
  struct_name: 'Family',
  fields: new Map<string, Typ>([
    ['children', { kind: 'TArray', value: kidTyp }],
    ['cousins', { kind: 'TArray', value: kidTyp }],
  ]),
};
const bobTyp: Typ = { kind: 'TStruct', value: bobDecl };
const bob = structVal(
  bobDecl,
  new Map([
    ['children', arrayVal([kid(3)])],
    ['cousins', arrayVal([kid(7)])],
  ])
);

function Pane({
  typ,
  value,
  testId,
}: {
  typ: Typ;
  value: RuntimeValue;
  testId: string;
}): React.JSX.Element {
  return (
    <div data-testid={testId}>
      <ValueEditor
        testIO={{ typ, value: { value } }}
        onValueChange={vi.fn()}
        currentPath={[]}
        diffs={[]}
      />
    </div>
  );
}

function Linked({
  right = [familyTyp, family],
  linked = true,
}: {
  right?: [Typ, RuntimeValue];
  linked?: boolean;
}): React.JSX.Element {
  const views = useLinkedViewsState();
  const panes = (
    <>
      <Pane typ={familyTyp} value={family} testId="left" />
      <Pane typ={right[0]} value={right[1]} testId="right" />
    </>
  );
  return (
    <IntlProvider locale="en" messages={enMessages}>
      {linked ? (
        <LinkedViewsContext.Provider value={views}>
          {panes}
        </LinkedViewsContext.Provider>
      ) : (
        panes
      )}
    </IntlProvider>
  );
}

const pane = (id: string): ReturnType<typeof within> =>
  within(screen.getByTestId(id));
const activeTab = (id: string): string | null | undefined =>
  screen.getByTestId(id).querySelector('.tab.active .tab-name')?.textContent;
const showsCards = (id: string, list: number): boolean =>
  screen
    .getByTestId(id)
    .querySelectorAll('.tab-panel')
    [list].querySelector('.table-array-editor') === null;

describe('Linked views', () => {
  it('opens the same tab on the other side', () => {
    render(<Linked />);
    expect(activeTab('right')).toBe('children');
    fireEvent.click(pane('left').getByRole('button', { name: /pets/ }));
    expect(activeTab('left')).toBe('pets');
    expect(activeTab('right')).toBe('pets');
    fireEvent.click(pane('right').getByRole('button', { name: /children/ }));
    expect(activeTab('left')).toBe('children');
  });

  it('leaves a side alone when it lacks the tab', () => {
    render(<Linked right={[bobTyp, bob]} />);
    fireEvent.click(pane('left').getByRole('button', { name: /pets/ }));
    expect(activeTab('right')).toBe('children');
  });

  it('switches the same list to cards and back on the other side', () => {
    render(<Linked />);
    expect(showsCards('right', 0)).toBe(false);
    fireEvent.click(pane('left').getAllByTitle('Switch to card view')[0]);
    expect(showsCards('left', 0)).toBe(true);
    expect(showsCards('right', 0)).toBe(true);
    expect(showsCards('right', 1)).toBe(false);
    fireEvent.click(pane('right').getAllByTitle('Switch to table view')[0]);
    expect(showsCards('left', 0)).toBe(false);
  });

  it('links nothing without a provider', () => {
    render(<Linked linked={false} />);
    fireEvent.click(pane('left').getByRole('button', { name: /pets/ }));
    expect(activeTab('right')).toBe('children');
    fireEvent.click(pane('left').getAllByTitle('Switch to card view')[0]);
    expect(showsCards('right', 0)).toBe(false);
  });
});
