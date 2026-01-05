import { describe, it, expect, vi, beforeEach } from 'vitest';
import { screen, fireEvent } from '@testing-library/react';
import { renderEditor, expectValueKind } from './test-helpers.tsx';

describe('Editors - partial invalid input handling', () => {
  beforeEach(() => vi.clearAllMocks());

  describe.each([
    { type: 'TInt' as const, name: 'IntEditor', input: '-' },
    { type: 'TMoney' as const, name: 'MoneyEditor', input: '-' },
    { type: 'TMoney' as const, name: 'MoneyEditor', input: '12.' },
    { type: 'TRat' as const, name: 'RatEditor', input: '-' },
  ])('$name with input "$input"', ({ type, input }) => {
    it('pushes Unset and preserves input while typing and after blur', () => {
      const { onValueChange } = renderEditor({ kind: type });
      const inputEl = screen.getByRole('textbox') as HTMLInputElement;

      fireEvent.change(inputEl, { target: { value: input } });

      // Should set Unset while partial invalid is present
      expectValueKind(onValueChange, 'Unset');

      // UI should keep the typed text
      expect(inputEl.value).toBe(input);

      // And not reset on blur
      fireEvent.blur(inputEl);
      expect(inputEl.value).toBe(input);
    });
  });
});
