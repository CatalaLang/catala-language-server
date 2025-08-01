import { createElement } from 'react';
import { createRoot } from 'react-dom/client';
import { writeUpMessage } from './generated/test_case';
import App from './App';
import './styles/index.css';
import '../node_modules/@vscode/codicons/dist/codicon.css';

export function renderUi(language: string): void {
  const vscode = acquireVsCodeApi();

  const root = createRoot(document.getElementById('root') as HTMLElement);
  root.render(createElement(App, { language, vscode }));
  vscode.postMessage(writeUpMessage({ kind: 'Ready' }));
}
