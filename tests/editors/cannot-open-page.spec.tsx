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

const stdinError = `┌─[ERROR]─
│
│  Required module not found: Benefit
│
│ Module required from
├─➤ -stdin-:3.9-16:
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
  it('opens the first error where catala printed it', () => {
    const post = page({
      cause: { kind: 'ModuleWontBuild' },
      message: moduleError,
      ran_from: '/work/project',
    });
    screen.getByText(/does not build/);
    screen.getByText('/work/project');
    fireEvent.click(screen.getByText('Open Benefit.catala_en'));
    fireEvent.click(screen.getByText('src/Benefit.catala_en:6.29-34:'));
    expect(sent(post)).toEqual([
      {
        kind: 'OpenLocation',
        value: { file: 'src/Benefit.catala_en', line: 6 },
      },
      {
        kind: 'OpenLocation',
        value: { file: 'src/Benefit.catala_en', line: 6 },
      },
    ]);
  });

  it('has no file to open for a test read from stdin; retry reads again', () => {
    const post = page({
      cause: { kind: 'ToolProblem' },
      message: stdinError,
      ran_from: '/work',
    });
    screen.getByText(/problem with the tools/);
    expect(screen.queryByText(/^Open .*catala/)).toBeNull();
    expect(screen.queryByRole('link')).toBeNull();
    fireEvent.click(screen.getByText('Retry'));
    expect(sent(post)).toEqual([{ kind: 'RetryOpenRequest' }]);
  });
});
