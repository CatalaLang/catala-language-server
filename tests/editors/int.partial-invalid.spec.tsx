import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../../src/locales/en.json';
import ValueEditor from '../../src/editors/ValueEditors';

describe('IntEditor - partial invalid input', () => {
  beforeEach(() => vi.clearAllMocks());

  function renderInt(onValueChange = vi.fn()) {
    const utils = render(
      <IntlProvider locale="en" messages={enMessages}>
        <ValueEditor
          testIO={{ typ: { kind: 'TInt' } }}
          onValueChange={onValueChange}
          currentPath={[]}
          diffs={[]}
        />
      </IntlProvider>
    );
    return { ...utils, onValueChange };
  }

  it("pushes Unset and preserves '-' while typing and after blur", () => {
    const { onValueChange } = renderInt();
    const input = screen.getByRole('textbox') as HTMLInputElement;

    fireEvent.change(input, { target: { value: '-' } });

    // Should set Unset while partial invalid is present
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({ kind: 'Unset' }),
          }),
        }),
      })
    );

    // UI should keep the typed text
    expect(input.value).toBe('-');

    // And not reset on blur
    fireEvent.blur(input);
    expect(input.value).toBe('-');
  });
});
