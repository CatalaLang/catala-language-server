import { createElement } from 'react';
import { createRoot } from 'react-dom/client';
import { writeUpMessage } from './generated/catala_types';
import App, { GeneralTestsUi, InputApp, TraceApp } from './App';
import './styles/index.css';
import '../node_modules/@vscode/codicons/dist/codicon.css';

export function renderUi(language: string): void {
  const vscode = acquireVsCodeApi();

  const root = createRoot(document.getElementById('root') as HTMLElement);
  root.render(createElement(App, { language, vscode }));
  vscode.postMessage(writeUpMessage({ kind: 'Ready' }));
}

export function renderInputScopeUi(language: string, scopename: string): void {
  const vscode = acquireVsCodeApi();
  const root = createRoot(document.getElementById('root') as HTMLElement);
  root.render(createElement(InputApp, { language, vscode, scopename }));
  vscode.postMessage(writeUpMessage({ kind: 'Ready' }));
}

export function renderTraceUi(language: string): void {
  const vscode = acquireVsCodeApi();
  const root = createRoot(document.getElementById('root') as HTMLElement);
  root.render(createElement(TraceApp, { language, vscode }));
  // `ready` message sent by the component
}

export function renderMacroTestsUi(language: string): void {
  const vscode = acquireVsCodeApi();
  const root = createRoot(document.getElementById('root') as HTMLElement);
  root.render(createElement(GeneralTestsUi, { language, vscode }));
  vscode.postMessage(writeUpMessage({ kind: 'Ready' }));
}
