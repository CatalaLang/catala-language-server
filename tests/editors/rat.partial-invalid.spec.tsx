import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../../src/locales/en.json';
import ValueEditor from '../../src/editors/ValueEditors';

describe('RatEditor - partial invalid input', () => {
  beforeEach(() => vi.clearAllMocks());

  function renderRat(onValueChange = vi.fn()) {
    const utils = render(
      <IntlProvider locale="en" messages={enMessages}>
        <ValueEditor
          testIO={{ typ: { kind: 'TRat' } }}
          onValueChange={onValueChange}
          currentPath={[]}
          diffs={[]}
        />
      </IntlProvider>
    );
    return { ...utils, onValueChange };
  }

  it("pushes Unset and preserves '-' while typing and after blur", () => {
    const { onValueChange } = renderRat();
    const input = screen.getByRole('textbox') as HTMLInputElement;

    fireEvent.change(input, { target: { value: '-' } });

    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({ kind: 'Unset' }),
          }),
        }),
      })
    );

    expect(input.value).toBe('-');

    fireEvent.blur(input);
    expect(input.value).toBe('-');
  });

  it("pushes Unset and preserves '.' while typing and after blur", () => {
    const { onValueChange } = renderRat();
    const input = screen.getByRole('textbox') as HTMLInputElement;

    fireEvent.change(input, { target: { value: '.' } });

    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({ kind: 'Unset' }),
          }),
        }),
      })
    );

    expect(input.value).toBe('.');

    fireEvent.blur(input);
    expect(input.value).toBe('.');
  });
});
