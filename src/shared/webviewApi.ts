/**
 * VS Code webview API wrapper
 *
 * Rationale:
 * - The global acquireVsCodeApi() exists only inside the webview runtime.
 * - We get a typed WebviewApi instance at the top level (from props) and inject it once via setVsCodeApi.
 * - Utilities and modules outside React components (e.g., messaging/confirm) can postMessage without prop drilling.
 * - This avoids hard-coding access to the global, keeps modules decoupled, and makes testing/mocking easier.
 */
import type { WebviewApi } from 'vscode-webview';

let api: WebviewApi<unknown> | null = null;

export function setVsCodeApi(a: WebviewApi<unknown>): void {
  api = a;
}

export function getVsCodeApi(): WebviewApi<unknown> {
  if (!api) {
    throw new Error('VS Code API not initialized');
  }
  return api;
}
