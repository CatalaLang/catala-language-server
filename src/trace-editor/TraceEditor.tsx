import { type ReactElement, useEffect, useRef, useState } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import type { WebviewApi } from 'vscode-webview';
import {
  VscodeButton,
  VscodeOption,
  VscodeProgressRing,
  VscodeRadio,
  VscodeRadioGroup,
  VscodeSingleSelect,
  VscodeTextfield,
} from '@vscode-elements/react-elements';
import { setVsCodeApi } from '../shared/webviewApi';
import type { TraceDownMessage, TraceUpMessage } from './messages';
import type { TraceElement, TraceTest } from './traceUtils';
import { readTraceTest } from './traceUtils';
import TraceTreeView, { type ExpandCommand } from './TraceTreeView';
import { DataPanel } from './TraceData';

type RunState =
  | { status: 'idle' }
  | { status: 'running' }
  | { status: 'success'; trace: TraceElement[] }
  | { status: 'error'; message: string };

type OutputView = 'tree' | 'json';

type Props = {
  vscode: WebviewApi<unknown>;
};

type ScopeWithInfo = [string, TraceTest | undefined];

/** Read the `value` off the target of a vscode-elements form event. */
function fieldValue(e: Event): string {
  return (e.target as { value?: string } | null)?.value ?? '';
}

export default function TraceEditor({ vscode }: Props): ReactElement {
  const intl = useIntl();
  const [cwd, setCwd] = useState('');
  const [scopes, setScopes] = useState<Map<string, TraceTest | undefined>>(
    new Map()
  );
  const [scope, setScope] = useState<ScopeWithInfo>(['', undefined]);
  // Whether the scope was preset via the editor's inputs (hides the scope form).
  const [scopePreset, setScopePreset] = useState(false);
  const [runState, setRunState] = useState<RunState>({ status: 'idle' });
  const [initialized, setInitialized] = useState(false);

  useEffect(() => {
    setVsCodeApi(vscode);
  }, [vscode]);

  useEffect(() => {
    const handleMessage = (event: MessageEvent): void => {
      const message = event.data as TraceDownMessage;
      console.log('TEST0', message.kind);
      switch (message.kind) {
        case 'init': {
          console.log('TEST1');
          const scopeMap = new Map<string, TraceTest | undefined>(
            message.scopes.map(([s, json]) => {
              console.log('TEST3', s);
              return [s, json !== null ? readTraceTest(json) : undefined];
            })
          );
          console.log('TEST2');
          setCwd(message.cwd);
          setScopes(scopeMap);
          setScopePreset(message.scope !== undefined);
          setScope((prev: ScopeWithInfo): ScopeWithInfo => {
            // Preselect the requested scope if one was provided.
            if (message.scope !== undefined) {
              return [message.scope, scopeMap.get(message.scope)];
            }
            if (prev[0] !== '') {
              return prev;
            }
            const first: ScopeWithInfo | undefined = scopeMap
              .entries()
              .next().value;
            return first ?? ['', undefined];
          });
          setInitialized(true);
          // Show a pre-computed trace directly, if one was provided.
          if (message.trace !== undefined) {
            setRunState({ status: 'success', trace: message.trace });
          }
          // Or run the trace immediately if requested.
          if (message.run) {
            const runScope =
              message.scope ?? scopeMap.keys().next().value ?? '';
            if (runScope) {
              setRunState({ status: 'running' });
              post(vscode, { kind: 'run', scope: runScope });
            }
          }
          break;
        }
        case 'result':
          if (message.ok) {
            setRunState({ status: 'success', trace: message.trace });
          } else {
            setRunState({ status: 'error', message: message.error });
          }
          break;
      }
    };

    window.addEventListener('message', handleMessage);
    post(vscode, { kind: 'ready' });

    return (): void => {
      window.removeEventListener('message', handleMessage);
    };
  }, [vscode]);

  const onRunScope = (): void => {
    const trimmed = scope[0].trim();
    if (!trimmed) {
      setRunState({
        status: 'error',
        message: intl.formatMessage({ id: 'trace.noScope' }),
      });
      return;
    }
    setRunState({ status: 'running' });
    post(vscode, { kind: 'run', scope: trimmed });
  };

  const running = runState.status === 'running';

  if (!initialized) {
    return (
      <div
        style={{ display: 'flex', alignItems: 'center', gap: 8, padding: 8 }}
      >
        <VscodeProgressRing />
        <FormattedMessage id="trace.loading" />
      </div>
    );
  }

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 12 }}>
      <h2>
        <FormattedMessage id="trace.viewer.title" />
        {(scopePreset || scopes.size === 1) && scope[0] && `  —  ${scope[0]}`}
      </h2>

      {!scopePreset && scopes.size > 1 && (
        <label style={fieldStyle}>
          <span style={{ fontWeight: 600 }}>
            <FormattedMessage id="trace.scope" />
          </span>
          {scopes.size > 0 ? (
            <VscodeSingleSelect
              value={scope[0]}
              onChange={(e) => {
                const s = fieldValue(e);
                setScope([s, scopes.get(s)]);
              }}
              style={{ width: '100%' }}
            >
              {[...scopes.keys()].map((s) => (
                <VscodeOption key={s} value={s}>
                  {s}
                </VscodeOption>
              ))}
            </VscodeSingleSelect>
          ) : (
            <VscodeTextfield
              value={scope[0]}
              placeholder={intl.formatMessage({ id: 'trace.scopePlaceholder' })}
              onInput={(e) => {
                const s = fieldValue(e);
                setScope([s, scopes.get(s)]);
              }}
              style={{ width: '100%' }}
            />
          )}
        </label>
      )}

      <div>
        <VscodeButton icon="play" disabled={running} onClick={onRunScope}>
          <FormattedMessage id={running ? 'trace.running' : 'trace.run'} />
        </VscodeButton>
      </div>

      {scope[1] !== undefined ? (
        <SplitPane
          left={
            <DataPanel
              test={scope[1]}
              trace={runState.status === 'success' ? runState.trace : undefined}
            />
          }
          right={<TraceResult runState={runState} cwd={cwd} test={scope[1]} />}
        />
      ) : (
        <TraceResult runState={runState} cwd={cwd} />
      )}
    </div>
  );
}

function TraceResult({
  runState,
  cwd,
  test,
}: {
  runState: RunState;
  cwd: string;
  test?: TraceTest;
}): ReactElement | null {
  const intl = useIntl();
  const [view, setView] = useState<OutputView>('tree');
  const [filter, setFilter] = useState('');
  const [expand, setExpand] = useState<ExpandCommand | null>(null);
  const expandAll = (open: boolean): void =>
    setExpand((prev) => ({ open, nonce: (prev?.nonce ?? 0) + 1 }));

  switch (runState.status) {
    case 'idle':
      return null;
    case 'running':
      return (
        <p>
          <FormattedMessage id="trace.running" />
        </p>
      );
    case 'error':
      return (
        <div>
          <p style={{ color: 'var(--vscode-errorForeground)' }}>
            <FormattedMessage id="trace.error" />
          </p>
          <pre style={preStyle}>{runState.message}</pre>
        </div>
      );
    case 'success':
      return (
        <div>
          <div
            style={{
              display: 'flex',
              gap: 16,
              alignItems: 'center',
              margin: 0,
            }}
          >
            <span style={{ fontWeight: 600 }}>
              <FormattedMessage id="trace.label" />
            </span>
            <VscodeRadioGroup
              variant="horizontal"
              onChange={(e) => setView(fieldValue(e) as OutputView)}
            >
              <VscodeRadio
                value="tree"
                label={intl.formatMessage({ id: 'trace.view.tree' })}
                checked={view === 'tree'}
              />
              <VscodeRadio
                value="json"
                label={intl.formatMessage({ id: 'trace.view.json' })}
                checked={view === 'json'}
              />
            </VscodeRadioGroup>
          </div>
          {view === 'tree' ? (
            <>
              <div
                style={{
                  display: 'flex',
                  gap: 8,
                  alignItems: 'center',
                  margin: '8px 0',
                }}
              >
                <VscodeTextfield
                  placeholder={intl.formatMessage({
                    id: 'trace.filterPlaceholder',
                  })}
                  value={filter}
                  onInput={(e) => setFilter(fieldValue(e))}
                  style={{ flex: 1 }}
                />
                <VscodeButton
                  icon="expand-all"
                  secondary
                  title={intl.formatMessage({ id: 'trace.expandAllTitle' })}
                  onClick={() => expandAll(true)}
                >
                  <FormattedMessage id="trace.expandAll" />
                </VscodeButton>
                <VscodeButton
                  icon="collapse-all"
                  secondary
                  title={intl.formatMessage({ id: 'trace.collapseAllTitle' })}
                  onClick={() => expandAll(false)}
                >
                  <FormattedMessage id="trace.collapseAll" />
                </VscodeButton>
              </div>
              <TraceTreeView
                trace={runState.trace}
                filter={filter}
                cwd={cwd}
                expand={expand}
                test={test}
              />
            </>
          ) : (
            <>
              <div style={{ margin: '8px 0' }}>
                <VscodeButton
                  icon="copy"
                  secondary
                  title={intl.formatMessage({ id: 'trace.copyJson' })}
                  onClick={() => {
                    void navigator.clipboard.writeText(
                      JSON.stringify(runState.trace, null, 2)
                    );
                  }}
                >
                  <FormattedMessage id="trace.copyJson" />
                </VscodeButton>
              </div>
              <pre style={preStyle}>
                {JSON.stringify(runState.trace, null, 2)}
              </pre>
            </>
          )}
        </div>
      );
  }
}

function post(vscode: WebviewApi<unknown>, message: TraceUpMessage): void {
  vscode.postMessage(message);
}

function SplitPane({
  left,
  right,
}: {
  left: ReactElement;
  right: ReactElement;
}): ReactElement {
  const intl = useIntl();
  const [leftWidth, setLeftWidth] = useState<number | null>(null);
  const draggingRef = useRef(false);
  const containerRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (containerRef.current) {
      const w = containerRef.current.getBoundingClientRect().width;
      if (w > 0) {
        setLeftWidth(w * 0.25);
      }
    }
  }, []);

  useEffect(() => {
    const onMove = (e: MouseEvent): void => {
      if (!draggingRef.current || !containerRef.current) {
        return;
      }
      const rect = containerRef.current.getBoundingClientRect();
      const w = e.clientX - rect.left;
      // Keep a minimum width for both panes.
      setLeftWidth(Math.max(120, Math.min(w, rect.width - 120)));
    };
    const onUp = (): void => {
      draggingRef.current = false;
    };
    window.addEventListener('mousemove', onMove);
    window.addEventListener('mouseup', onUp);
    return (): void => {
      window.removeEventListener('mousemove', onMove);
      window.removeEventListener('mouseup', onUp);
    };
  }, []);

  return (
    <div
      ref={containerRef}
      style={{ display: 'flex', alignItems: 'stretch', width: '100%' }}
    >
      <div
        style={{
          width: leftWidth ?? '25%',
          flex: '0 0 auto',
          overflow: 'auto',
        }}
      >
        {left}
      </div>
      <div
        onMouseDown={() => {
          draggingRef.current = true;
        }}
        title={intl.formatMessage({ id: 'trace.dragToResize' })}
        style={{
          flex: '0 0 auto',
          width: 6,
          margin: '0 6px',
          cursor: 'col-resize',
          background: 'var(--vscode-panel-border, transparent)',
          borderRadius: 2,
        }}
      />
      <div style={{ flex: 1, minWidth: 0 }}>{right}</div>
    </div>
  );
}

const fieldStyle: React.CSSProperties = {
  display: 'flex',
  flexDirection: 'column',
  gap: 4,
};

const preStyle: React.CSSProperties = {
  background:
    'var(--vscode-textCodeBlock-background, var(--vscode-editor-background))',
  border: '1px solid var(--vscode-panel-border, transparent)',
  padding: 10,
  borderRadius: 2,
  overflow: 'auto',
  maxHeight: '70vh',
  whiteSpace: 'pre-wrap',
  wordBreak: 'break-word',
};
