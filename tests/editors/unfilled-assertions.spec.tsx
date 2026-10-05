/** Expected outputs come from a run: an assertion still to fill is not a
 *  value to fill before running, and must not stop the run. */
import React from 'react';
import { describe, it, expect } from 'vitest';
import { render, screen } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import type {
  StructDeclaration,
  Test,
  Typ,
} from '../../src/generated/catala_types';
import {
  countUnsetValues,
  firstUnsetPath,
  withoutUnfilledAssertions,
} from '../../src/editors/unsetValidation';
import { ReadinessChip } from '../../src/test-case-editor/RunControl';
import { intVal, rv, structVal } from './test-helpers';
import enMessages from '../../src/locales/en.json';

const resultDecl: StructDeclaration = {
  struct_name: 'Result',
  fields: new Map<string, Typ>([
    ['total', { kind: 'TInt' }],
    ['bonus', { kind: 'TInt' }],
  ]),
};
const resultTyp: Typ = { kind: 'TStruct', value: resultDecl };

// Inputs all filled; one assertion just added (fields unset), one filled.
const test: Test = {
  testing_scope: 'T',
  tested_scope: {
    name: 'S',
    module_name: 'M',
    inputs: new Map([['a', { typ: { kind: 'TInt' }, is_context: false }]]),
    outputs: new Map<string, Typ>([
      ['result', resultTyp],
      ['count', { kind: 'TInt' }],
    ]),
    module_deps: [],
  },
  test_inputs: new Map([
    ['a', { typ: { kind: 'TInt' }, value: { value: intVal(1) } }],
  ]),
  test_outputs: new Map([
    [
      'result',
      {
        typ: resultTyp,
        value: {
          value: structVal(
            resultDecl,
            new Map([
              ['total', rv({ kind: 'Unset' })],
              ['bonus', rv({ kind: 'Unset' })],
            ])
          ),
        },
      },
    ],
    ['count', { typ: { kind: 'TInt' }, value: { value: intVal(3) } }],
  ]),
  description: '',
  title: '',
};

describe('an assertion still to fill', () => {
  it('leaves the readiness chip at "All fields set"', () => {
    render(
      <IntlProvider locale="en" messages={enMessages}>
        <ReadinessChip test={test} onJump={() => {}} />
      </IntlProvider>
    );
    expect(screen.getByText('All fields set')).toBeTruthy();
  });

  it('is not counted', () => {
    expect(countUnsetValues(test)).toBe(0);
  });

  it('is not jumped to', () => {
    expect(firstUnsetPath(test)).toBeUndefined();
  });

  it('is left out of the run, the filled assertions kept', () => {
    const [runnable] = withoutUnfilledAssertions([test]);
    expect([...runnable.test_outputs.keys()]).toEqual(['count']);
    expect([...test.test_outputs.keys()]).toEqual(['result', 'count']);
  });
});
