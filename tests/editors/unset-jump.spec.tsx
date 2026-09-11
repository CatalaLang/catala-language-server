import React from 'react';
import { describe, it, expect } from 'vitest';
import { render } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import type {
  StructDeclaration,
  Test,
  Typ,
} from '../../src/generated/catala_types';
import {
  CompositeEditor,
  type EditorItem,
} from '../../src/editors/CompositeEditor';
import { RevealContext } from '../../src/editors/reveal';
import { firstUnsetPath } from '../../src/editors/unsetValidation';
import { arrayVal, intVal, rv, structVal } from './test-helpers';
import enMessages from '../../src/locales/en.json';

const pointDecl: StructDeclaration = {
  struct_name: 'Point',
  fields: new Map<string, Typ>([['x', { kind: 'TInt' }]]),
};
const pointTyp: Typ = { kind: 'TStruct', value: pointDecl };
const pointsTyp: Typ = { kind: 'TArray', value: pointTyp };

describe('firstUnsetPath', () => {
  it('finds the first hole, inputs first, inside lists and structs', () => {
    const test: Test = {
      testing_scope: 'T',
      tested_scope: {
        name: 'S',
        module_name: 'M',
        inputs: new Map([
          ['a', { typ: { kind: 'TInt' }, is_context: false }],
          ['b', { typ: pointsTyp, is_context: false }],
        ]),
        outputs: new Map(),
        module_deps: [],
      },
      test_inputs: new Map([
        ['a', { typ: { kind: 'TInt' }, value: { value: intVal(1) } }],
        [
          'b',
          {
            typ: pointsTyp,
            value: {
              value: arrayVal([
                structVal(pointDecl, new Map([['x', intVal(2)]])),
                structVal(pointDecl, new Map([['x', rv({ kind: 'Unset' })]])),
              ]),
            },
          },
        ],
      ]),
      test_outputs: new Map(),
      description: '',
      title: '',
    };
    expect(firstUnsetPath(test)).toEqual([
      { kind: 'StructField', value: 'b' },
      { kind: 'ListIndex', value: 1 },
      { kind: 'StructField', value: 'x' },
    ]);
  });
});

function tab(key: string): EditorItem {
  return {
    key,
    label: key,
    type: pointsTyp,
    editor: <div data-testid={`panel-${key}`} />,
  };
}

describe('reveal opens the tab on the path', () => {
  it('activates the tab named by the next path segment', () => {
    const { container } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <RevealContext.Provider
          value={{
            path: [
              { kind: 'StructField', value: 'pets' },
              { kind: 'ListIndex', value: 0 },
            ],
            nonce: 1,
          }}
        >
          <CompositeEditor
            items={[tab('children'), tab('pets')]}
            currentPath={[]}
          />
        </RevealContext.Provider>
      </IntlProvider>
    );
    const active = container.querySelector('.tab-panel.active');
    expect(active?.querySelector('[data-testid="panel-pets"]')).not.toBeNull();
  });
});
