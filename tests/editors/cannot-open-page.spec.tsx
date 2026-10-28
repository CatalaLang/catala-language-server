/** The page shown when a test cannot be read and has nothing to repair. */
import React from 'react';
import { describe, it, expect, vi } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import type { WebviewApi } from 'vscode-webview';
import type { CannotOpen } from '../../src/generated/catala_types';
import { readUpMessage } from '../../src/generated/catala_types';
import { CannotOpenPage } from '../../src/test-case-editor/TestFileEditor';
import enMessages from '../../src/locales/en.json';

function page(value: CannotOpen): ReturnType<typeof vi.fn> {
  const postMessage = vi.fn();
  render(
    <IntlProvider locale="en" messages={enMessages}>
      <CannotOpenPage
        value={value}
        vscode={{ postMessage } as unknown as WebviewApi<unknown>}
      />
    </IntlProvider>
  );
  return postMessage;
}

const sent = (post: ReturnType<typeof vi.fn>): unknown[] =>
  post.mock.calls.map(([m]) => readUpMessage(m));

describe('CannotOpenPage', () => {
  it('retry reads the file again', () => {
    const post = page({
      cause: { kind: 'ToolProblem' },
      message: 'Required module not found: Benefit',
      ran_from: '/work',
    });
    screen.getByText(/problem with the tools/);
    fireEvent.click(screen.getByText('Retry'));
    expect(sent(post)).toEqual([{ kind: 'RetryOpenRequest' }]);
  });
});
