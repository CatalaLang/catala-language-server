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

const moduleError = `┌─[ERROR]─
│
│  Unknown type "Count": no declaration for a structure, enumeration, scope
│  or external type by this name was found.
│
├─➤ src/Benefit.catala_en:6.29-34:
│   │
│ 6 │   input child_count content Count
│   │                             ‾‾‾‾‾
└─
`;

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
  it("says why, shows catala's message and the folder it ran from", () => {
    page({
      cause: { kind: 'ModuleWontBuild' },
      message: moduleError,
      ran_from: '/work/project',
    });
    screen.getByText(/does not build/);
    screen.getByText(/Unknown type "Count"/);
    screen.getByText('/work/project');
  });

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
