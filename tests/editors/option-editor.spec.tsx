import React from 'react';
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../../src/locales/en.json';
import ValueEditor from '../../src/editors/ValueEditors';
import type {
  EnumDeclaration,
  RuntimeValue,
  StructDeclaration,
  Typ,
} from '../../src/generated/catala_types';
import { rv, intVal } from './test-helpers';

const confirmMock = vi.fn(async () => true);
vi.mock('../../src/messaging/confirm', () => ({
  confirm: (...args: unknown[]) => confirmMock(...(args as [])),
}));

function optionDecl(payload: Typ): EnumDeclaration {
  return {
    enum_name: 'Optional',
    constructors: new Map([
      ['Absent', null],
      ['Present', { value: payload }],
    ]),
    ctor_attrs: new Map(),
  };
}

function absent(payload: Typ): RuntimeValue {
  return rv({ kind: 'Enum', value: [optionDecl(payload), ['Absent', null]] });
}

function present(payload: Typ, value: RuntimeValue): RuntimeValue {
  return rv({
    kind: 'Enum',
    value: [optionDecl(payload), ['Present', { value }]],
  });
}

function renderOption(
  typ: Typ,
  value: RuntimeValue,
  opts: { editable?: boolean } = {}
) {
  const onValueChange = vi.fn();
  render(
    <IntlProvider locale="en" messages={enMessages}>
      <ValueEditor
        testIO={{ typ, value: { value } }}
        onValueChange={onValueChange}
        currentPath={[]}
        diffs={[]}
        editable={opts.editable ?? true}
      />
    </IntlProvider>
  );
  const lastValue = (): RuntimeValue =>
    onValueChange.mock.calls.at(-1)![0].value.value;
  return { onValueChange, lastValue };
}

const tInt: Typ = { kind: 'TInt' };
const optInt: Typ = { kind: 'TOption', value: tInt };

describe('OptionEditor', () => {
  beforeEach(() => confirmMock.mockClear());

  it('shows Absent as a chip that sets an unset value', () => {
    const { lastValue } = renderOption(optInt, absent(tInt));
    expect(screen.queryByRole('combobox')).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: /Absent/ }));
    const v = lastValue().value;
    expect(v.kind === 'Enum' && v.value[1][0]).toBe('Present');
    expect(v.kind === 'Enum' && v.value[1][1]?.value.value.kind).toBe('Unset');
  });

  it('shows the payload alone, with a × back to Absent', () => {
    const { lastValue } = renderOption(optInt, present(tInt, intVal(3)));
    expect(screen.queryByRole('combobox')).toBeNull();
    expect(screen.getByDisplayValue('3')).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: 'Back to Absent' }));
    const v = lastValue().value;
    expect(v.kind === 'Enum' && v.value[1]).toEqual(['Absent', null]);
    expect(confirmMock).not.toHaveBeenCalled();
  });

  it('asks before clearing a filled struct, and keeps it if refused', async () => {
    const decl: StructDeclaration = {
      struct_name: 'M.S',
      fields: new Map([['n', tInt]]),
    };
    const tStruct: Typ = { kind: 'TStruct', value: decl };
    const filled = rv({
      kind: 'Struct',
      value: [decl, new Map([['n', intVal(1)]])],
    });
    confirmMock.mockResolvedValueOnce(false);
    const { onValueChange } = renderOption(
      { kind: 'TOption', value: tStruct },
      present(tStruct, filled)
    );
    fireEvent.click(screen.getByRole('button', { name: 'Back to Absent' }));
    await waitFor(() => expect(confirmMock).toHaveBeenCalledOnce());
    expect(onValueChange).not.toHaveBeenCalled();
  });

  it('read-only: plain Absent, no buttons', () => {
    renderOption(optInt, absent(tInt), { editable: false });
    expect(screen.getByText('Absent')).toBeTruthy();
    expect(screen.queryByRole('button')).toBeNull();
  });

  it('nested options keep the dropdown at the outer level', () => {
    const inner = present(tInt, intVal(3));
    renderOption({ kind: 'TOption', value: optInt }, present(optInt, inner));
    expect(screen.getAllByRole('combobox')).toHaveLength(1);
    expect(screen.getByDisplayValue('3')).toBeTruthy();
    expect(screen.getByRole('button', { name: 'Back to Absent' })).toBeTruthy();
  });
});
