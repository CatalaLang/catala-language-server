import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../../src/locales/en.json';
import ValueEditor from '../../src/editors/ValueEditors';
import type { TestIo, Typ } from '../../src/generated/catala_types';

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

describe('MoneyEditor supports negative values', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it('emits cents for negative integer amounts', () => {
    const { onValueChange } = renderEditor({ kind: 'TMoney' });

    const input = screen.getByRole('textbox') as HTMLInputElement;
    fireEvent.change(input, { target: { value: '-10' } });

    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({
              kind: 'Money',
              value: -1000,
            }),
          }),
        }),
      })
    );
  });

  it('emits cents for negative decimal amounts with two decimals', () => {
    const { onValueChange } = renderEditor({ kind: 'TMoney' });

    const input = screen.getByRole('textbox') as HTMLInputElement;
    fireEvent.change(input, { target: { value: '-12.34' } });

    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({
              kind: 'Money',
              value: -1234,
            }),
          }),
        }),
      })
    );
  });
});
