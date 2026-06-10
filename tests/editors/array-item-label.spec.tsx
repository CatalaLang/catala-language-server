import React from 'react';
import { describe, it, expect, vi } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import type {
  RuntimeValue,
  StructDeclaration,
  Typ,
} from '../../src/generated/catala_types';
import { ArrayEditor } from '../../src/editors/ArrayEditor';
import { rv, arrayVal } from './test-helpers';
import enMessages from '../../src/locales/en.json';

// Struct with a TOption<TArray<TInt>> field:
//   - tryCreateTableSchema fails (option-wrapping-array is unsupported)
//   - hasNestedArrays = true → vertical layout → label input in the item header
const decl: StructDeclaration = {
  struct_name: 'S',
  fields: new Map<string, Typ>([
    [
      'f',
      { kind: 'TOption', value: { kind: 'TArray', value: { kind: 'TInt' } } },
    ],
  ]),
};
const elementType: Typ = { kind: 'TStruct', value: decl };

function makeItem(label?: string): RuntimeValue {
  const attrs: RuntimeValue['attrs'] = [
    { kind: 'Uid', value: crypto.randomUUID() },
  ];
  if (label !== undefined) attrs.push({ kind: 'ArrayItemLabel', value: label });
  return {
    value: {
      kind: 'Struct',
      value: [decl, new Map([['f', rv({ kind: 'Unset' })]])],
    },
    attrs,
  };
}

function setup(items: RuntimeValue[]) {
  const onValueChange = vi.fn();
  render(
    <IntlProvider locale="en" messages={enMessages}>
      <ArrayEditor
        elementType={elementType}
        valueDef={{ value: arrayVal(items) }}
        onValueChange={onValueChange}
        currentPath={[]}
        diffs={[]}
      />
    </IntlProvider>
  );
  return { onValueChange };
}

describe('array item label input', () => {
  it('renders a label input for each non-phantom item', () => {
    setup([makeItem(), makeItem()]);
    const inputs = screen.getAllByPlaceholderText('Name...');
    expect(inputs).toHaveLength(2);
  });

  it('shows existing label value in the input', () => {
    setup([makeItem('my label')]);
    expect(screen.getByDisplayValue('my label')).toBeInTheDocument();
  });

  it('calls onValueChange with updated ArrayItemLabel attr on input change', () => {
    const { onValueChange } = setup([makeItem('old')]);
    const input = screen.getByDisplayValue('old');
    fireEvent.change(input, { target: { value: 'new' } });
    expect(onValueChange).toHaveBeenCalled();
    const updatedRV: RuntimeValue = onValueChange.mock.calls[0][0];
    expect(updatedRV.value.kind).toBe('Array');
    const updatedAttrs =
      updatedRV.value.kind === 'Array'
        ? (updatedRV.value.value[0]?.attrs ?? [])
        : [];
    const labelAttr = updatedAttrs.find((a) => a.kind === 'ArrayItemLabel');
    expect(labelAttr?.kind === 'ArrayItemLabel' && labelAttr.value).toBe('new');
  });
});
