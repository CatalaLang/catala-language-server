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
import { renderEditor, arrayVal, intVal, structVal } from './test-helpers';
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
    expect(screen.getByText('children (3)')).toBeInTheDocument();
    expect(screen.getByText('pets (1)')).toBeInTheDocument();
  });

  it('shows plain label when count is undefined', () => {
    render(
      <IntlProvider locale="en" messages={enMessages}>
        <CompositeEditor
          items={[arrayItem('children', undefined), arrayItem('pets', 5)]}
        />
      </IntlProvider>
    );
    expect(screen.getByText('children')).toBeInTheDocument();
    expect(screen.getByText('pets (5)')).toBeInTheDocument();
  });

  it('shows zero count', () => {
    render(
      <IntlProvider locale="en" messages={enMessages}>
        <CompositeEditor
          items={[arrayItem('children', 0), arrayItem('pets', 2)]}
        />
      </IntlProvider>
    );
    expect(screen.getByText('children (0)')).toBeInTheDocument();
  });
});

describe('StructEditor tab count from value', () => {
  it('shows correct counts for each array field', () => {
    renderEditor(personTyp, vi.fn(), { value: mkPersonValue(3, 1) });
    expect(screen.getByText('children (3)')).toBeInTheDocument();
    expect(screen.getByText('pets (1)')).toBeInTheDocument();
  });

  it('shows zero count for empty arrays', () => {
    renderEditor(personTyp, vi.fn(), { value: mkPersonValue(0, 1) });
    expect(screen.getByText('children (0)')).toBeInTheDocument();
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

    expect(screen.getByText('children (2)')).toBeInTheDocument();

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

    expect(screen.getByText('children (4)')).toBeInTheDocument();
  });
});
