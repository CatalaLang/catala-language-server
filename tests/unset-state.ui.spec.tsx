import { describe, it, expect, vi } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../src/locales/en.json';
import ValueEditor from '../src/editors/ValueEditors';
import type {
  TestIo,
  Typ,
  StructDeclaration,
  EnumDeclaration,
  Option,
} from '../src/generated/catala_types';

function renderEditor(
  typ: Typ,
  onValueChange = vi.fn(),
  value?: TestIo['value']
) {
  const utils = render(
    <IntlProvider locale="en" messages={enMessages}>
      <ValueEditor
        testIO={{ typ, value }}
        onValueChange={onValueChange}
        currentPath={[]}
        diffs={[]}
      />
    </IntlProvider>
  );
  return { ...utils, onValueChange };
}

describe('ValueEditors - Unset handling', () => {
  it('IntEditor shows Unset initially and emits Unset when cleared', () => {
    const { onValueChange } = renderEditor({ kind: 'TInt' });
    // Unset badge visible initially
    expect(screen.getByText(/Unset/i)).toBeInTheDocument();

    const input = screen.getByRole('textbox');
    fireEvent.change(input, { target: { value: '42' } });
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({ kind: 'Integer', value: 42 }),
          }),
        }),
      })
    );

    fireEvent.change(input, { target: { value: '' } });
    fireEvent.blur(input);
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({ kind: 'Unset' }),
          }),
        }),
      })
    );
  });

  it('RatEditor shows Unset initially and emits Unset when cleared', () => {
    const { onValueChange } = renderEditor({ kind: 'TRat' });
    expect(screen.getByText(/Unset/i)).toBeInTheDocument();

    const input = screen.getByRole('textbox');
    fireEvent.change(input, { target: { value: '3.14' } });
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({ kind: 'Decimal', value: 3.14 }),
          }),
        }),
      })
    );

    fireEvent.change(input, { target: { value: '' } });
    fireEvent.blur(input);
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({ kind: 'Unset' }),
          }),
        }),
      })
    );
  });

  it('MoneyEditor shows Unset initially and emits Unset when cleared', () => {
    const { onValueChange } = renderEditor({ kind: 'TMoney' });
    expect(screen.getByText(/Unset/i)).toBeInTheDocument();

    const input = screen.getByRole('textbox');
    fireEvent.change(input, { target: { value: '12.34' } });
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({ kind: 'Money', value: 1234 }),
          }),
        }),
      })
    );

    fireEvent.change(input, { target: { value: '' } });
    fireEvent.blur(input);
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({ kind: 'Unset' }),
          }),
        }),
      })
    );
  });

  it('DateEditor shows Unset initially and emits Unset when cleared', () => {
    const { onValueChange } = renderEditor({ kind: 'TDate' });
    expect(screen.getByText(/Unset/i)).toBeInTheDocument();

    const input = screen.getByDisplayValue('', {
      selector: 'input[type="date"]',
    });
    fireEvent.change(input, { target: { value: '2024-05-01' } });
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({
              kind: 'Date',
              value: { year: 2024, month: 5, day: 1 },
            }),
          }),
        }),
      })
    );

    fireEvent.change(input, { target: { value: '' } });
    fireEvent.blur(input);
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({ kind: 'Unset' }),
          }),
        }),
      })
    );
  });

  it('BoolEditor shows Unset initially and emits Unset when selecting "unset"', () => {
    const { onValueChange } = renderEditor({ kind: 'TBool' });
    expect(screen.getByText(/Unset/i)).toBeInTheDocument();

    const select = screen.getByRole('combobox');
    fireEvent.change(select, { target: { value: 'true' } });
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({ kind: 'Bool', value: true }),
          }),
        }),
      })
    );

    fireEvent.change(select, { target: { value: 'unset' } });
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({ kind: 'Unset' }),
          }),
        }),
      })
    );
  });

  it('DurationEditor emits Unset when all fields are cleared and blurred', () => {
    const { onValueChange } = renderEditor({ kind: 'TDuration' });
    // DurationEditor does not display an Unset badge; verify behavior only.
    const years = screen.getByLabelText(/Years:/i) as HTMLInputElement;
    fireEvent.blur(years); // all fields empty by default -> Unset on blur
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({ kind: 'Unset' }),
          }),
        }),
      })
    );
  });

  it('DurationEditor shows Unset initially', () => {
    renderEditor({ kind: 'TDuration' });
    expect(screen.getByText(/Unset/i)).toBeInTheDocument();
  });

  it('EnumEditor shows Unset initially', () => {
    const enumDecl: EnumDeclaration = {
      enum_name: 'Pkg.MyEnum',
      constructors: new Map<string, Option<Typ>>([
        ['Foo', null],
        ['Bar', { value: { kind: 'TInt' } }],
      ]),
    };
    renderEditor({ kind: 'TEnum', value: enumDecl });
    expect(screen.getByText(/Unset/i)).toBeInTheDocument();
  });

  it('StructEditor renders fields in Unset state initially', () => {
    const structDecl: StructDeclaration = {
      struct_name: 'Pkg.MyStruct',
      fields: new Map<string, Typ>([
        ['a', { kind: 'TInt' }],
        ['b', { kind: 'TBool' }],
      ]),
    };
    renderEditor({ kind: 'TStruct', value: structDecl });
    // Two fields -> at least two Unset badges
    const badges = screen.getAllByText(/Unset/i);
    expect(badges.length).toBeGreaterThanOrEqual(2);
  });

  it('ArrayEditor defaults to empty array (no Unset)', () => {
    const { container } = renderEditor({
      kind: 'TArray',
      value: { kind: 'TInt' },
    });
    // No Unset badge inside the array editor
    expect(container.querySelector('.unset-badge')).toBeNull();
  });
});
