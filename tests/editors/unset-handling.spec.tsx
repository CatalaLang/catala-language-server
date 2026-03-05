import { describe, it, expect } from 'vitest';
import { screen, fireEvent } from '@testing-library/react';
import { renderEditor, expectValueKind } from './test-helpers.tsx';

describe('ValueEditors - Unset handling', () => {
  describe.each([
    { type: 'TInt' as const, name: 'IntEditor' },
    { type: 'TRat' as const, name: 'RatEditor' },
    { type: 'TMoney' as const, name: 'MoneyEditor' },
  ])('$name', ({ type }) => {
    it('shows unset underline and placeholder initially', () => {
      const { container } = renderEditor({ kind: type });
      expect(
        container.querySelector('.value-editor.unset')
      ).toBeInTheDocument();
      const input = screen.getByRole('textbox') as HTMLInputElement;
      expect(input.placeholder).toMatch(/unset/i);
    });

    it('emits Unset when input is cleared', () => {
      const { onValueChange } = renderEditor({ kind: type });
      const input = screen.getByRole('textbox');

      // Type some value
      fireEvent.change(input, { target: { value: '42' } });

      // Clear it
      fireEvent.change(input, { target: { value: '' } });
      fireEvent.blur(input);

      expectValueKind(onValueChange, 'Unset');
    });
  });

  it('DateEditor shows unset underline initially', () => {
    const { container } = renderEditor({ kind: 'TDate' });
    expect(container.querySelector('.value-editor.unset')).toBeInTheDocument();
  });

  it('DurationEditor shows unset underline initially', () => {
    const { container } = renderEditor({ kind: 'TDuration' });
    expect(container.querySelector('.value-editor.unset')).toBeInTheDocument();
  });

  it('BoolEditor shows unset underline initially (empty option is selected)', () => {
    const { container } = renderEditor({ kind: 'TBool' });
    expect(container.querySelector('.value-editor.unset')).toBeInTheDocument();
    const select = screen.getByRole('combobox') as HTMLSelectElement;
    expect(select.value).toBe('unset');
  });

  it('ArrayEditor shows no unset indicator initially (empty array is valid)', () => {
    const { container } = renderEditor({
      kind: 'TArray',
      value: { kind: 'TInt' },
    });
    expect(
      container.querySelector('.value-editor.unset')
    ).not.toBeInTheDocument();
  });
});
