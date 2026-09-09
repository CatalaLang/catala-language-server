/**
 * One diff, one report. Two nested ValueEditors must not both claim the same
 * diff (the optional-enum double "Obtenu:"), and a diff the server addresses
 * INSIDE a payload (with an EnumPayload segment) must still reach it.
 */
import { render } from '@testing-library/react';
import { describe, it, expect } from 'vitest';
import { IntlProvider } from 'react-intl';
import enMessages from '../src/locales/en.json';
import AssertionValueEditor from '../src/test-case-editor/AssertionValueEditor';
import type {
  Diff,
  EnumDeclaration,
  PathSegment,
  RuntimeValue,
  Typ,
} from '../src/generated/catala_types';
import { tEnum, tRat, enumValue } from './helpers';

function seg(kind: 'StructField' | 'EnumPayload', value: string): PathSegment {
  return { kind, value };
}

const innerTyp = tEnum(
  'X.Code',
  new Map([
    ['C_A', null],
    ['C_B', null],
  ])
) as Extract<Typ, { kind: 'TEnum' }>;

const optionalDecl: EnumDeclaration = {
  enum_name: 'Optional',
  constructors: new Map([
    ['Absent', null],
    ['Present', { value: innerTyp }],
  ]),
  ctor_attrs: new Map(),
};

function present(payload: RuntimeValue): RuntimeValue {
  return enumValue(optionalDecl, 'Present', payload);
}

const optionTyp: Typ = { kind: 'TOption', value: innerTyp };

function renderWithDiffs(diffs: Diff[], value: RuntimeValue): HTMLElement {
  const { container } = render(
    <IntlProvider locale="en" messages={enMessages}>
      <AssertionValueEditor
        testIO={{ typ: optionTyp, value: { value } }}
        onValueChange={() => {}}
        diffs={diffs}
        currentPath={[seg('StructField', 'code')]}
      />
    </IntlProvider>
  );
  return container;
}

describe('a diff is reported exactly once', () => {
  it('an optional enum with a diff at its own path renders one actual, not two', () => {
    const expected = present(enumValue(innerTyp.value, 'C_A', undefined));
    const actual = present(enumValue(innerTyp.value, 'C_B', undefined));
    const container = renderWithDiffs(
      [{ path: [seg('StructField', 'code')], expected, actual }],
      expected
    );
    expect(container.querySelectorAll('.diff-actual').length).toBe(1);
  });

  it('a diff the server addresses inside the payload still renders', () => {
    // The server names the payload explicitly: ...code / EnumPayload Present.
    const expected = present(enumValue(innerTyp.value, 'C_A', undefined));
    const container = renderWithDiffs(
      [
        {
          path: [seg('StructField', 'code'), seg('EnumPayload', 'Present')],
          expected: enumValue(innerTyp.value, 'C_A', undefined),
          actual: enumValue(innerTyp.value, 'C_B', undefined),
        },
      ],
      expected
    );
    expect(container.querySelectorAll('.diff-actual').length).toBe(1);
  });
});
