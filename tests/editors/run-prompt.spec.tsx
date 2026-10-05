/** "Run with unset values?" is about inputs: an assertion still to fill
 *  is filled by the run itself. */
import React from 'react';
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import type {
  RuntimeValue,
  StructDeclaration,
  Test,
  Typ,
} from '../../src/generated/catala_types';
import TestEditor from '../../src/test-case-editor/TestEditor';
import { intVal, rv, structVal } from './test-helpers';
import enMessages from '../../src/locales/en.json';

const confirm = vi.fn(async () => true);
vi.mock('../../src/messaging/confirm', () => ({
  confirm: (...args: unknown[]) => confirm(...(args as [])),
}));

const resultDecl: StructDeclaration = {
  struct_name: 'Result',
  fields: new Map<string, Typ>([['total', { kind: 'TInt' }]]),
};
const resultTyp: Typ = { kind: 'TStruct', value: resultDecl };
const unset = (): RuntimeValue => rv({ kind: 'Unset' });

function testWith(input: RuntimeValue): Test {
  return {
    testing_scope: 'T',
    tested_scope: {
      name: 'S',
      module_name: 'M',
      inputs: new Map([['a', { typ: { kind: 'TInt' }, is_context: false }]]),
      outputs: new Map<string, Typ>([['result', resultTyp]]),
      module_deps: [],
    },
    test_inputs: new Map([
      ['a', { typ: { kind: 'TInt' }, value: { value: input } }],
    ]),
    test_outputs: new Map([
      [
        'result',
        {
          typ: resultTyp,
          value: {
            value: structVal(resultDecl, new Map([['total', unset()]])),
          },
        },
      ],
    ]),
    description: '',
    title: 'T',
  };
}

function renderEditor(test: Test) {
  const onTestRun = vi.fn();
  const onTestOutputsReset = vi.fn();
  render(
    <IntlProvider locale="en" messages={enMessages}>
      <TestEditor
        test={test}
        onTestChange={() => {}}
        onTestDelete={() => {}}
        onTestRun={onTestRun}
        onTestOutputsReset={onTestOutputsReset}
        onDiffResolved={() => {}}
        onInvalidateDiffs={() => {}}
      />
    </IntlProvider>
  );
  return { onTestRun, onTestOutputsReset };
}

beforeEach(() => {
  confirm.mockClear();
  Element.prototype.scrollIntoView = vi.fn();
});

describe('running with an assertion still to fill', () => {
  it('runs without asking', async () => {
    const { onTestRun } = renderEditor(testWith(intVal(1)));
    fireEvent.click(screen.getByText('Run test'));
    await waitFor(() => expect(onTestRun).toHaveBeenCalledWith('T'));
    expect(confirm).not.toHaveBeenCalled();
  });

  it('replaces with the execution output without asking', async () => {
    const { onTestOutputsReset } = renderEditor(testWith(intVal(1)));
    fireEvent.click(screen.getByText('Replace with execution output'));
    await waitFor(() => expect(onTestOutputsReset).toHaveBeenCalledWith('T'));
    expect(confirm).not.toHaveBeenCalled();
  });

  it('still asks when an input is unset', async () => {
    const { onTestRun } = renderEditor(testWith(unset()));
    fireEvent.click(screen.getByText('Run test'));
    await waitFor(() =>
      expect(confirm).toHaveBeenCalledWith('RunTestWithUnsetValues')
    );
    await waitFor(() => expect(onTestRun).toHaveBeenCalledWith('T'));
  });
});
