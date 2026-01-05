import { describe, it, expect } from 'vitest';
import { screen, fireEvent } from '@testing-library/react';
import { renderEditor, expectValueKind } from './test-helpers.tsx';

describe('ValueEditors - Unset handling', () => {
  describe.each([
    { type: 'TInt' as const, name: 'IntEditor' },
    { type: 'TRat' as const, name: 'RatEditor' },
    { type: 'TMoney' as const, name: 'MoneyEditor' },
  ])('$name', ({ type }) => {
    it('shows Unset badge initially', () => {
      renderEditor({ kind: type });
      expect(screen.getByText(/Unset/i)).toBeInTheDocument();
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

  it('DateEditor shows Unset initially', () => {
    renderEditor({ kind: 'TDate' });
    expect(screen.getByText(/Unset/i)).toBeInTheDocument();
  });

  it('DurationEditor shows Unset initially', () => {
    renderEditor({ kind: 'TDuration' });
    expect(screen.getByText(/Unset/i)).toBeInTheDocument();
  });

  it('BoolEditor shows Unset initially (empty option is selected)', () => {
    renderEditor({ kind: 'TBool' });
    expect(screen.getByText(/Unset/i)).toBeInTheDocument();
    const select = screen.getByRole('combobox') as HTMLSelectElement;
    expect(select.value).toBe('unset');
  });

  it('ArrayEditor shows no badge initially (empty array is valid)', () => {
    renderEditor({ kind: 'TArray', value: { kind: 'TInt' } });
    expect(screen.queryByText(/Unset/i)).not.toBeInTheDocument();
  });
});
