import { describe, it, expect, vi } from 'vitest';
import { screen, fireEvent } from '@testing-library/react';
import { renderEditor, expectValueKind, boolVal } from './test-helpers.tsx';

describe('ValueEditors - Unset handling', () => {
  describe.each([
    { type: 'TInt' as const, name: 'IntEditor' },
    { type: 'TRat' as const, name: 'RatEditor' },
    { type: 'TMoney' as const, name: 'MoneyEditor' },
  ])('$name', ({ type }) => {
    it('shows unset underline initially', () => {
      const { container } = renderEditor({ kind: type });
      expect(
        container.querySelector('.value-editor.unset')
      ).toBeInTheDocument();
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

  it('BoolEditor shows unset underline initially (checkbox is unchecked)', () => {
    const { container } = renderEditor({ kind: 'TBool' });
    expect(container.querySelector('.value-editor.unset')).toBeInTheDocument();
    const checkbox = screen.getByRole('checkbox') as HTMLInputElement;
    expect(checkbox.checked).toBe(false);
    expect(container.querySelector('.bool-toggle--unset')).toBeInTheDocument();
  });

  it('BoolEditor: first click on Unset sets false', () => {
    const { onValueChange } = renderEditor({ kind: 'TBool' });
    fireEvent.click(screen.getByRole('checkbox'));
    expectValueKind(onValueChange, 'Bool', false);
  });

  it.each([
    [false, true],
    [true, false],
  ])('BoolEditor toggles %s → %s, never back to Unset', (from, to) => {
    const { onValueChange, container } = renderEditor(
      { kind: 'TBool' },
      vi.fn(),
      { value: boolVal(from) }
    );
    expect(container.querySelector('.bool-toggle--unset')).toBeNull();
    fireEvent.click(screen.getByRole('checkbox'));
    expectValueKind(onValueChange, 'Bool', to);
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
