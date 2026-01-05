import { describe, it, expect, vi, beforeEach } from 'vitest';
import { screen, fireEvent } from '@testing-library/react';
import { renderEditor, expectValueKind } from './test-helpers.tsx';

describe('Editors - Invalid badge display', () => {
  beforeEach(() => vi.clearAllMocks());

  describe.each([
    { type: 'TInt' as const, name: 'IntEditor', field: 'textbox' },
    { type: 'TMoney' as const, name: 'MoneyEditor', field: 'textbox' },
    { type: 'TDuration' as const, name: 'DurationEditor', field: 'Years:' },
  ])('$name', ({ type, field }) => {
    it("shows 'Invalid' badge for lone minus ('-') and preserves input", () => {
      const { onValueChange } = renderEditor({ kind: type });

      const input =
        field === 'textbox'
          ? (screen.getByRole('textbox') as HTMLInputElement)
          : (screen.getByLabelText(new RegExp(field, 'i')) as HTMLInputElement);

      fireEvent.change(input, { target: { value: '-' } });

      // Expect an "Invalid" badge to be visible
      expect(screen.getByText(/Invalid/i)).toBeTruthy();

      // Should push a placeholder value (Unset) while invalid
      expectValueKind(onValueChange, 'Unset');

      // The user's text should be preserved
      expect(input.value).toBe('-');
    });
  });
});
