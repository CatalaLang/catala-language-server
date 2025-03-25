import { createElement } from 'react';
import { createRoot } from 'react-dom/client';
import { IntlProvider } from 'react-intl';
import TestFileEditor from './TestFileEditor';
import { writeUpMessage } from './generated/test_case';
import messages from './locales/en.json';

const vscode = acquireVsCodeApi();

export function renderUi(): void {
  const root = createRoot(document.getElementById('root') as HTMLElement);
  root.render(
    createElement(
      IntlProvider,
      {
        locale: 'en',
        messages: messages,
      },
      createElement(TestFileEditor, {
        contents: { state: 'initializing' },
        vscode: vscode,
      })
    )
  );
  vscode.postMessage(writeUpMessage({ kind: 'Ready' }));
}
