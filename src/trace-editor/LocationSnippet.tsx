import {
  type CSSProperties,
  type MouseEvent,
  type ReactElement,
  type ReactNode,
  createContext,
  useContext,
  useEffect,
  useState,
  useSyncExternalStore,
} from 'react';
import { useIntl } from 'react-intl';
import { getVsCodeApi } from '../shared/webviewApi';
import type { TraceDownMessage, TraceUpMessage } from './messages';
import type { CodeLocation } from './traceUtils';
import { posText } from './traceUtils';

export const CwdContext = createContext<string>('');
export type SnippetActions = {
  spawnPanel: (filter: string) => void;
  addFilter: (filter: string) => void;
};

export const SnippetActionsContext = createContext<SnippetActions | null>(null);

export function resolvePath(cwd: string, file: string): string {
  if (!cwd || file.startsWith('/') || /^[a-zA-Z]:[\\/]/.test(file)) {
    return file;
  }
  return `${cwd.replace(/[\\/]+$/, '')}/${file}`;
}

// -- The "View with filter" menu item -----------------------------------------

const snippetContextAttribute = JSON.stringify({
  webviewSection: 'traceSnippet',
  preventDefaultContextMenuItems: false,
});
const snippetSelectionContextAttribute = JSON.stringify({
  webviewSection: 'traceSnippetSelection',
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

let menuTarget: SnippetActions | null = null;
let menuListenerAttached = false;

function armSnippetMenu(actions: SnippetActions): void {
  menuTarget = actions;
  if (menuListenerAttached) {
    return;
  }
  menuListenerAttached = true;
  window.addEventListener('message', (event: MessageEvent): void => {
    const m = event.data as TraceDownMessage;
    if (m?.kind !== 'viewWithFilter' && m?.kind !== 'addToFilter') {
      return;
    }
    const text = window.getSelection()?.toString().trim() ?? '';
    if (text === '' || menuTarget === null) {
      return;
    }
    if (m.kind === 'viewWithFilter') {
      menuTarget.spawnPanel(text);
    } else {
      menuTarget.addFilter(text);
    }
  });
}

// -- Fetching the source lines ------------------------------------------------

const extractCache = new Map<string, string | null>();
const pendingExtracts = new Map<number, (line: string | null) => void>();
let extractSeq = 0;
let extractListenerAttached = false;

function ensureExtractListener(): void {
  if (extractListenerAttached) {
    return;
  }
  extractListenerAttached = true;
  window.addEventListener('message', (event: MessageEvent): void => {
    const m = event.data as TraceDownMessage;
    if (m?.kind === 'extract') {
      const callback = pendingExtracts.get(m.id);
      if (callback) {
        pendingExtracts.delete(m.id);
        callback(m.text);
      }
    }
  });
}

async function fetchExtract(
  file: string,
  line: number
): Promise<string | null> {
  const key = `${file}:${line}`;
  const cached = extractCache.get(key);
  if (cached !== undefined) {
    return Promise.resolve(cached);
  }
  ensureExtractListener();
  const id = extractSeq++;
  return new Promise<string | null>((resolve) => {
    pendingExtracts.set(id, resolve);
    const message: TraceUpMessage = {
      kind: 'requestExtract',
      id,
      file,
      line,
    };
    getVsCodeApi().postMessage(message);
  }).then((result) => {
    extractCache.set(key, result);
    return result;
  });
}

// -- Components ----------------------------------------------------------------

function SnippetBlock({
  pos,
  children,
}: {
  pos: CodeLocation;
  children: ReactNode;
}): ReactElement {
  const cwd = useContext(CwdContext);
  const actions = useContext(SnippetActionsContext);
  const selected = useHasSelection();
  const intl = useIntl();
  const [hover, setHover] = useState(false);
  const openLocation = (e: MouseEvent): void => {
    e.stopPropagation();
    const message: TraceUpMessage = {
      kind: 'openLocation',
      file: resolvePath(cwd, pos.file),
      start: pos.start,
      end: pos.end,
    };
    getVsCodeApi().postMessage(message);
  };
  const label = intl.formatMessage(
    { id: 'trace.openLocation' },
    { target: posText(pos) }
  );
  // VS Code builds the menu itself, so that its own copy / cut / paste items
  // stay: all this does is name the section our items are contributed to, and
  // note which snippet the click landed on for when one of them is picked.
  const onContextMenu = (): void => {
    if (actions !== null) {
      armSnippetMenu(actions);
    }
  };
  return (
    <div
      style={snippetStyle}
      onContextMenu={onContextMenu}
      data-vscode-context={
        actions === null
          ? undefined
          : selected
            ? snippetSelectionContextAttribute
            : snippetContextAttribute
      }
    >
      <pre style={sourceStyle}>{children}</pre>
      <button
        type="button"
        style={
          hover
            ? { ...openButtonStyle, ...openButtonHoverStyle }
            : openButtonStyle
        }
        onClick={openLocation}
        onMouseEnter={() => setHover(true)}
        onMouseLeave={() => setHover(false)}
        title={label}
        aria-label={label}
      >
        <span className="codicon codicon-go-to-file" />
      </button>
    </div>
  );
}

export function LocationSnippet({
  pos,
}: {
  pos: CodeLocation;
}): ReactElement | null {
  const cwd = useContext(CwdContext);
  const startLine = pos.start.line;
  const endLine = Math.max(startLine, pos.end.line);
  const [lines, setLines] = useState<(string | null)[] | null>(null);
  useEffect(() => {
    let cancelled = false;
    const nums: number[] = [];
    for (let l = startLine; l <= endLine; l++) nums.push(l);
    void Promise.all(
      nums.map((l) => fetchExtract(resolvePath(cwd, pos.file), l))
    ).then((texts) => {
      if (!cancelled) setLines(texts);
    });
    return (): void => {
      cancelled = true;
    };
  }, [cwd, pos]);

  if (lines === null || lines.every((l) => l === null)) {
    return null;
  }
  if (lines.length > 1) {
    return (
      <SnippetBlock pos={pos}>
        {lines.map((line) => line ?? '').join('\n')}
      </SnippetBlock>
    );
  }
  const text = lines[0] ?? '';
  const from = Math.max(0, pos.start.character - 1);
  const to = Math.max(from, pos.end.character - 1);
  return (
    <SnippetBlock pos={pos}>
      {text.slice(0, from)}
      <mark style={markStyle}>{text.slice(from, to) || ' '}</mark>
      {text.slice(to)}
    </SnippetBlock>
  );
}

// -- Styles --------------------------------------------------------------------

const snippetStyle: CSSProperties = {
  position: 'relative',
  margin: '2px 0 4px 22px',
};

const sourceStyle: CSSProperties = {
  margin: 0,
  padding: '2px 40px 2px 6px',
  background:
    'var(--vscode-textCodeBlock-background, var(--vscode-editor-background))',
  border: '1px solid var(--vscode-panel-border, transparent)',
  borderRadius: 2,
  overflowX: 'auto',
  whiteSpace: 'pre',
  userSelect: 'text',
  fontFamily: 'var(--vscode-editor-font-family, monospace)',
};

const openButtonStyle: CSSProperties = {
  position: 'absolute',
  top: 2,
  right: 8,
  display: 'inline-flex',
  alignItems: 'center',
  justifyContent: 'center',
  width: 20,
  height: 20,
  padding: 0,
  border: 'none',
  borderRadius: 3,
  cursor: 'pointer',
  background: 'transparent',
  color: 'var(--vscode-icon-foreground, var(--vscode-foreground))',
  opacity: 0.6,
};

const openButtonHoverStyle: CSSProperties = {
  background: 'var(--vscode-toolbar-hoverBackground, transparent)',
  opacity: 1,
};

const markStyle: CSSProperties = {
  background: 'var(--vscode-editor-findMatchHighlightBackground, yellow)',
  color: 'inherit',
  borderRadius: 2,
};
