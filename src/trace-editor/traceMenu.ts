import { type MouseEvent, useSyncExternalStore } from 'react';
import type { TraceDownMessage } from './messages';

export type AddFilter = (filter: string) => void;

export type TraceMenuActions = {
  addFilter: AddFilter;
};

const plainContext = JSON.stringify({
  webviewSection: 'tracePanel',
  preventDefaultContextMenuItems: false,
});
const selectionContext = JSON.stringify({
  webviewSection: 'tracePanelSelection',
  preventDefaultContextMenuItems: false,
});

let hasSelection = false;
const selectionListeners = new Set<() => void>();

function onSelectionChange(): void {
  const selected = (window.getSelection()?.toString() ?? '').trim() !== '';
  if (selected === hasSelection) {
    return;
  }
  hasSelection = selected;
  for (const listener of selectionListeners) {
    listener();
  }
}

function subscribeToSelection(onStoreChange: () => void): () => void {
  if (selectionListeners.size === 0) {
    document.addEventListener('selectionchange', onSelectionChange);
  }
  selectionListeners.add(onStoreChange);
  return (): void => {
    selectionListeners.delete(onStoreChange);
    if (selectionListeners.size === 0) {
      document.removeEventListener('selectionchange', onSelectionChange);
    }
  };
}

function useHasSelection(): boolean {
  return useSyncExternalStore(
    subscribeToSelection,
    () => hasSelection,
    () => false
  );
}

export function isSelectingText(): boolean {
  const selection = window.getSelection();
  return (
    selection !== null &&
    !selection.isCollapsed &&
    selection.toString().trim() !== ''
  );
}

let menuTarget: TraceMenuActions | null = null;
let menuListenerAttached = false;
let armedEvent: Event | null = null;

function armMenu(actions: TraceMenuActions, event: Event): void {
  if (armedEvent === event) {
    return;
  }
  armedEvent = event;
  menuTarget = actions;
  if (menuListenerAttached) {
    return;
  }
  menuListenerAttached = true;
  window.addEventListener('message', (message: MessageEvent): void => {
    const m = message.data as TraceDownMessage;
    if (m?.kind !== 'addToFilter') {
      return;
    }
    const text = window.getSelection()?.toString().trim() ?? '';
    if (text === '' || menuTarget === null) {
      return;
    }
    if (m.kind === 'addToFilter') {
      menuTarget.addFilter(text);
    }
  });
}

export function useTraceMenu(actions: TraceMenuActions): {
  'data-vscode-context': string;
  onContextMenu: (event: MouseEvent) => void;
} {
  const selected = useHasSelection();
  return {
    'data-vscode-context': selected ? selectionContext : plainContext,
    onContextMenu: (event: MouseEvent): void =>
      armMenu(actions, event.nativeEvent),
  };
}
