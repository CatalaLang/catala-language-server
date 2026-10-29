import { describe, it, expect } from 'vitest';
import { fireEvent } from '@testing-library/react';
import { renderEditor, expectValueKind } from './test-helpers.tsx';

// As in the webview: the clipboard events go to the document.
function focusDate(container: HTMLElement): HTMLInputElement {
  const input = container.querySelector(
    'input[type="date"]'
  ) as HTMLInputElement;
  input.focus();
  return input;
}

function paste(text: string): void {
  fireEvent.paste(document, { clipboardData: { getData: () => text } });
}

describe('DateEditor copy and paste', () => {
  it('pastes a whole date', () => {
    const { container, onValueChange } = renderEditor({ kind: 'TDate' });
    const input = focusDate(container);
    paste(' 2024-03-31\n');
    expectValueKind(onValueChange, 'Date');
    expect(input.value).toBe('2024-03-31');
  });

  it('flags an impossible date as invalid', () => {
    const { container, onValueChange } = renderEditor({ kind: 'TDate' });
    focusDate(container);
    paste('2024-02-30');
    expectValueKind(onValueChange, 'Unset');
    expect(container.querySelector('.value-editor.invalid')).not.toBeNull();
  });

  it('copies the whole date', () => {
    const { container } = renderEditor({ kind: 'TDate' });
    focusDate(container);
    paste('2024-03-31');
    let copied = '';
    fireEvent.copy(document, {
      clipboardData: { setData: (_: string, t: string) => (copied = t) },
    });
    expect(copied).toBe('2024-03-31');
    expect(container.querySelector('.hint')).toHaveTextContent('Copied');
  });
});
