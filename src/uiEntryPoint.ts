import { createElement } from 'react';
import { createRoot } from 'react-dom/client';
import TestFileEditor from './TestFileEditor';
import { writeUpMessage } from './generated/test_case';

const vscode = acquireVsCodeApi();

export function renderUi(): void {
  const root = createRoot(document.getElementById('root') as HTMLElement);
  root.render(
    createElement(TestFileEditor, {
      contents: { state: 'initializing' },
      vscode,
    })
  );
  vscode.postMessage(writeUpMessage({ kind: 'Ready' }));
}
