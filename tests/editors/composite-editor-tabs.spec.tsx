import React from 'react';
import { describe, it, expect, vi } from 'vitest';
import { render, screen } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import type { StructDeclaration, Typ } from '../../src/generated/catala_types';
import {
  CompositeEditor,
  type EditorItem,
} from '../../src/editors/CompositeEditor';
import ValueEditor from '../../src/editors/ValueEditors';
import { renderEditor, arrayVal, intVal, rv, structVal } from './test-helpers';
import enMessages from '../../src/locales/en.json';

function arrayItem(key: string, count?: number): EditorItem {
  return {
    key,
    label: key,
    type: { kind: 'TArray', value: { kind: 'TInt' } },
    editor: <div />,
    count,
  };
}

// Person { children: int[], pets: int[] }
const personDecl: StructDeclaration = {
  struct_name: 'Person',
  fields: new Map<string, Typ>([
    ['children', { kind: 'TArray', value: { kind: 'TInt' } }],
    ['pets', { kind: 'TArray', value: { kind: 'TInt' } }],
  ]),
};
const personTyp: Typ = { kind: 'TStruct', value: personDecl };

function mkPersonValue(nChildren: number, nPets: number) {
  return structVal(
    personDecl,
    new Map([
      [
        'children',
        arrayVal(Array.from({ length: nChildren }, (_, i) => intVal(i))),
      ],
      ['pets', arrayVal(Array.from({ length: nPets }, (_, i) => intVal(i)))],
    ])
  );
}

describe('CompositeEditor tab labels', () => {
  it('shows count in tab label when count is set', () => {
    render(
      <IntlProvider locale="en" messages={enMessages}>
        <CompositeEditor
          items={[arrayItem('children', 3), arrayItem('pets', 1)]}
        />
      </IntlProvider>
    );
    expect(
      screen.getByRole('button', { name: 'children 3' })
    ).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'pets 1' })).toBeInTheDocument();
  });

  it('shows plain label when count is undefined', () => {
    render(
      <IntlProvider locale="en" messages={enMessages}>
        <CompositeEditor
          items={[arrayItem('children', undefined), arrayItem('pets', 5)]}
        />
      </IntlProvider>
    );
    expect(
      screen.getByRole('button', { name: 'children' })
    ).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'pets 5' })).toBeInTheDocument();
  });

  it('shows zero count', () => {
    render(
      <IntlProvider locale="en" messages={enMessages}>
        <CompositeEditor
          items={[arrayItem('children', 0), arrayItem('pets', 2)]}
        />
      </IntlProvider>
    );
    expect(
      screen.getByRole('button', { name: 'children 0' })
    ).toBeInTheDocument();
  });
});

describe('StructEditor tab count from value', () => {
  it('shows correct counts for each array field', () => {
    renderEditor(personTyp, vi.fn(), { value: mkPersonValue(3, 1) });
    expect(
      screen.getByRole('button', { name: 'children 3' })
    ).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'pets 1' })).toBeInTheDocument();
  });

  it('shows zero count for empty arrays', () => {
    renderEditor(personTyp, vi.fn(), { value: mkPersonValue(0, 1) });
    expect(
      screen.getByRole('button', { name: 'children 0' })
    ).toBeInTheDocument();
  });

  it('count updates when re-rendered with a new value', () => {
    const onValueChange = vi.fn();
    const { rerender } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <ValueEditor
          testIO={{ typ: personTyp, value: { value: mkPersonValue(2, 1) } }}
          onValueChange={onValueChange}
          currentPath={[]}
          diffs={[]}
        />
      </IntlProvider>
    );

    expect(
      screen.getByRole('button', { name: 'children 2' })
    ).toBeInTheDocument();

    rerender(
      <IntlProvider locale="en" messages={enMessages}>
        <ValueEditor
          testIO={{ typ: personTyp, value: { value: mkPersonValue(4, 1) } }}
          onValueChange={onValueChange}
          currentPath={[]}
          diffs={[]}
        />
      </IntlProvider>
    );

    expect(
      screen.getByRole('button', { name: 'children 4' })
    ).toBeInTheDocument();
  });
});

describe('Unfilled badges', () => {
  it('shows how many values are left to fill in a tab', () => {
    render(
      <IntlProvider locale="en" messages={enMessages}>
        <CompositeEditor
          items={[
            { ...arrayItem('children', 2), unfilled: 2 },
            { ...arrayItem('pets', 1), unfilled: 0 },
          ]}
        />
      </IntlProvider>
    );
    const badges = document.querySelectorAll('.count-badge-unfilled');
    expect(badges.length).toBe(1);
    expect(badges[0]).toHaveAttribute('title', '2 items, 2 values to fill');
    expect(badges[0].textContent).toBe('2·2');
  });

  it('counts unset elements inside an inactive tab from the value', () => {
    const value = structVal(
      personDecl,
      new Map([
        ['children', arrayVal([intVal(1), intVal(2)])],
        ['pets', arrayVal([rv({ kind: 'Unset' }), intVal(3)])],
      ])
    );
    renderEditor(personTyp, vi.fn(), { value });
    const badge = document.querySelector('.count-badge-unfilled');
    expect(badge?.textContent).toBe('2·1');
  });
});

describe('Tab labels keep their decorations', () => {
  it('renders the item label, not just its key, next to the count', () => {
    const decorated: EditorItem = {
      ...arrayItem('children', 2),
      label: (
        <>
          children
          <span data-testid="mark" />
        </>
      ),
    };
    render(
      <IntlProvider locale="en" messages={enMessages}>
        <CompositeEditor items={[decorated, arrayItem('pets', 1)]} />
      </IntlProvider>
    );
    expect(screen.getByTestId('mark').closest('.tab')).not.toBeNull();
    expect(
      screen.getByRole('button', { name: 'children 2' })
    ).toBeInTheDocument();
  });
});
