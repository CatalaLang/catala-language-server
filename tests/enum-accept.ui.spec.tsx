import { describe, it, expect, vi } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../src/locales/en.json';
import AssertionValueEditor from '../src/test-case-editor/AssertionValueEditor';
import type {
  Diff,
  EnumDeclaration,
  PathSegment,
  RuntimeValue,
  StructDeclaration,
  TestIo,
  Typ,
} from '../src/generated/test_case';

function rv(value: RuntimeValue['value']): RuntimeValue {
  return { value, attrs: [] };
}

describe('AssertionValueEditor - Enum accept button', () => {
  it('shows accept for complex actual payload and calls onDiffResolved', () => {
    const structDecl: StructDeclaration = {
      struct_name: 'Pkg.Record',
      fields: new Map([['n', { kind: 'TInt' } as Typ]]),
    };
    const enumDecl: EnumDeclaration = {
      enum_name: 'Pkg.Choice',
      constructors: new Map([
        ['A', null],
        ['B', { value: { kind: 'TStruct', value: structDecl } }],
      ]),
    };
    const enumTyp: Typ = { kind: 'TEnum', value: enumDecl };

    const expected = rv({
      kind: 'Enum',
      value: [enumDecl, ['A', null]],
    });
    const actual = rv({
      kind: 'Enum',
      value: [
        enumDecl,
        [
          'B',
          {
            value: rv({
              kind: 'Struct',
              value: [
                structDecl,
                new Map([['n', rv({ kind: 'Integer', value: 42 })]]),
              ],
            }),
          },
        ],
      ],
    });

    const currentPath: PathSegment[] = [];
    const diffs: Diff[] = [{ path: currentPath, expected, actual } as Diff];
    const testIO: TestIo = { typ: enumTyp, value: { value: expected } };

    const onDiffResolved = vi.fn();
    const onValueChange = vi.fn();

    render(
      <IntlProvider locale="en" messages={enMessages}>
        <AssertionValueEditor
          testIO={testIO}
          onValueChange={onValueChange}
          diffs={diffs}
          currentPath={currentPath}
          onDiffResolved={onDiffResolved}
        />
      </IntlProvider>
    );

    const btn = screen.getByRole('button', {
      name: /Replace expected with computed value/i,
    });
    expect(btn).toBeInTheDocument();

    fireEvent.click(btn);
    expect(onDiffResolved).toHaveBeenCalledTimes(1);
    expect(onDiffResolved).toHaveBeenCalledWith(currentPath);
    expect(onValueChange).toHaveBeenCalled();
  });
});
