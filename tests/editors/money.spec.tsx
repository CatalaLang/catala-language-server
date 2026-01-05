import { describe, it, vi, beforeEach } from 'vitest';
import { screen, fireEvent } from '@testing-library/react';
import { renderEditor, expectValueKind } from './test-helpers.tsx';

describe('MoneyEditor supports negative values', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it('emits cents for negative integer amounts', () => {
    const { onValueChange } = renderEditor({ kind: 'TMoney' });

    const input = screen.getByRole('textbox') as HTMLInputElement;
    fireEvent.change(input, { target: { value: '-10' } });

    expectValueKind(onValueChange, 'Money', -1000);
  });

  it('emits cents for negative decimal amounts with two decimals', () => {
    const { onValueChange } = renderEditor({ kind: 'TMoney' });

    const input = screen.getByRole('textbox') as HTMLInputElement;
    fireEvent.change(input, { target: { value: '-12.34' } });

    expectValueKind(onValueChange, 'Money', -1234);
  });
});
