import { createElement } from 'react';
import { createRoot } from 'react-dom/client';
import { writeUpMessage } from './generated/catala_types';
import App, { InputApp } from './App';
import { graphviz } from 'd3-graphviz';
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

export function renderExplainGraph(): void {
  const vscode = acquireVsCodeApi();
  const errorEl = document.getElementById('error');

  const renderer = graphviz('#graph', {
    useWorker: false, // for wasm CSP
    fit: true,
    zoom: true,
  });

  // Intercept clicks on node links to open local source files
  const graphEl = document.getElementById('graph');
  graphEl?.addEventListener('click', (event: MouseEvent) => {
    if (!(event.target instanceof Element)) return;
    const anchor = event.target.closest('a');
    if (!anchor) return;
    const href =
      anchor.getAttribute('xlink:href') ?? anchor.getAttribute('href');
    if (!href) return;
    event.preventDefault();
    event.stopPropagation();
    vscode.postMessage({ kind: 'openLink', href });
  });

  window.addEventListener('message', (event: MessageEvent) => {
    const msg = event.data as { kind?: string; dot?: string };
    if (msg?.kind !== 'renderDot' || typeof msg.dot !== 'string') return;
    try {
      if (errorEl) errorEl.hidden = true;
      renderer.renderDot(msg.dot);
    } catch (e) {
      if (errorEl) {
        errorEl.hidden = false;
        errorEl.textContent = `Failed to render graph:\n${String(e)}`;
      }
    }
  });

  vscode.postMessage({ kind: 'ready' });
}
