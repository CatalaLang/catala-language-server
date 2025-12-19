import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../../src/locales/en.json';
import ValueEditor from '../../src/editors/ValueEditors';

describe('MoneyEditor - impossible badge for partial invalid input', () => {
  beforeEach(() => vi.clearAllMocks());

  function renderMoney(onValueChange = vi.fn()) {
    const utils = render(
      <IntlProvider locale="en" messages={enMessages}>
        <ValueEditor
          testIO={{ typ: { kind: 'TMoney' } }}
          onValueChange={onValueChange}
          currentPath={[]}
          diffs={[]}
        />
      </IntlProvider>
    );
    return { ...utils, onValueChange };
  }

  it("shows an 'Invalid' badge for lone minus ('-') and preserves input", () => {
    const { onValueChange } = renderMoney();
    const input = screen.getByRole('textbox') as HTMLInputElement;

    fireEvent.change(input, { target: { value: '-' } });

    // Expect an "Invalid" badge to be visible (failing until implemented)
    expect(screen.getByText(/Invalid/i)).toBeTruthy();

    // Should push a placeholder value (Unset/impossible) while invalid
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({ kind: 'Unset' }),
          }),
        }),
      })
    );

    // The user's text should be preserved
    expect(input.value).toBe('-');
  });
});
