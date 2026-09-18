/**
 * On the authored pane, a dropped list input keeps its red mark on the tab
 * header even when its value holds a blank: the authored pane counts nothing.
 */
import React from 'react';
import { describe, it, expect } from 'vitest';
import { render } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import type {
  Recovery,
  Test,
  TestIo,
  Typ,
} from '../../src/generated/catala_types';
import BrokenTestView from '../../src/test-case-editor/BrokenTestView';
import { arrayVal, intVal, rv } from './test-helpers';
import enMessages from '../../src/locales/en.json';

const intsTyp: Typ = { kind: 'TArray', value: { kind: 'TInt' } };

function io(typ: Typ, value?: TestIo['value']): TestIo {
  return { typ, value };
}

function authored(): Test {
  return {
    testing_scope: 'T',
    tested_scope: {
      module_name: 'B',
      name: 'C',
      inputs: new Map([
        ['kept', { typ: intsTyp, is_context: false }],
        ['gone', { typ: intsTyp, is_context: false }],
      ]),
      outputs: new Map(),
      module_deps: [],
    },
    test_inputs: new Map<string, TestIo>([
      ['kept', io(intsTyp, { value: arrayVal([intVal(1)]) })],
      // The tester left one element as `impossible`.
      ['gone', io(intsTyp, { value: arrayVal([rv({ kind: 'Unset' })]) })],
    ]),
    test_outputs: new Map(),
    description: '',
    title: '',
  };
}

function rebuilt(): Test {
  const a = authored();
  return {
    ...a,
    tested_scope: {
      ...a.tested_scope,
      inputs: new Map([['kept', { typ: intsTyp, is_context: false }]]),
    },
    test_inputs: new Map<string, TestIo>([
      ['kept', io(intsTyp, { value: arrayVal([intVal(1)]) })],
    ]),
  };
}

describe('authored pane tab headers', () => {
  it('shows the dropped mark on a list input that also holds a blank', () => {
    const v: Recovery = {
      tests: [
        {
          authored: authored(),
          rebuilt: rebuilt(),
          outcomes: [
            {
              path: [{ kind: 'StructField', value: 'kept' }],
              side: { kind: 'In' },
              outcome: { kind: 'Fits' },
            },
            {
              path: [{ kind: 'StructField', value: 'gone' }],
              side: { kind: 'In' },
              outcome: { kind: 'Dropped' },
            },
          ],
        },
      ],
      notes: [],
      working_copy: 'x.repair',
    };
    const { container } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <BrokenTestView view={v} onRun={(): void => {}} />
      </IntlProvider>
    );
    const authoredPane = container.querySelector('.broken-pane-authored')!;
    const tabs = Array.from(authoredPane.querySelectorAll('button.tab'));
    expect(tabs.length).toBe(2);
    const gone = tabs.find((t) => t.textContent?.includes('gone'))!;
    expect(gone.querySelector('.fate-dropped')).not.toBeNull();
  });
});
