import {
  type ReactElement,
  useEffect,
  useLayoutEffect,
  useRef,
  useState,
} from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import type { WebviewApi } from 'vscode-webview';
import {
  VscodeButton,
  VscodeOption,
  VscodeProgressRing,
  VscodeSingleSelect,
  VscodeTextfield,
} from '@vscode-elements/react-elements';
import { setVsCodeApi } from '../shared/webviewApi';
import type { TraceDownMessage, TraceUpMessage } from './messages';
import type { TraceElement, TraceTest } from './traceUtils';
import { PANEL_HEIGHT_VAR, fieldValue, readTraceTest } from './traceUtils';
import {
  TracePanel,
  codeBlockStyle,
  type FilterCommand,
} from './TraceTreeView';
import { DataPanel } from './TraceData';

type RunState =
  | { status: 'idle' }
  | { status: 'running' }
  | { status: 'success'; trace: TraceElement[] }
  | { status: 'error'; message: string };

type Props = {
  vscode: WebviewApi<unknown>;
};

type ScopeWithInfo = [string, TraceTest | undefined];

const PANE_LAYOUTS = ['data', 'both', 'trace'] as const;
type PaneLayout = (typeof PANE_LAYOUTS)[number];

const BOTTOM_MARGIN = 12;

export default function TraceEditor({ vscode }: Props): ReactElement {
  const intl = useIntl();
  const [cwd, setCwd] = useState('');
  const [scopes, setScopes] = useState<Map<string, TraceTest | undefined>>(
    new Map()
  );
  const [scope, setScope] = useState<ScopeWithInfo>(['', undefined]);
  const [scopePreset, setScopePreset] = useState(false);
  const [runState, setRunState] = useState<RunState>({ status: 'idle' });
  const [initialized, setInitialized] = useState(false);
  const [layout, setLayout] = useState<PaneLayout>('both');
  const [filterRequest, setFilterRequest] = useState<FilterCommand | null>(
    null
  );

  const requestFilter = (filter: string): void =>
    setFilterRequest((prev) => ({ filter, nonce: (prev?.nonce ?? 0) + 1 }));

  useEffect(() => {
    setVsCodeApi(vscode);
  }, [vscode]);

  useEffect(() => {
    const handleMessage = (event: MessageEvent): void => {
      const message = event.data as TraceDownMessage;
      switch (message.kind) {
        case 'init': {
          const scopeMap = new Map<string, TraceTest | undefined>(
            message.scopes.map(([s, json]) => {
              return [s, json !== null ? readTraceTest(json) : undefined];
            })
          );
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

  const resultsRef = useRef<HTMLDivElement>(null);
  const [panelHeight, setPanelHeight] = useState<string>();
  useLayoutEffect(() => {
    const element = resultsRef.current;
    if (element === null) {
      return;
    }
    const measure = (): void => {
      const top = element.getBoundingClientRect().top + window.scrollY;
      setPanelHeight(`calc(100vh - ${Math.round(top)}px - ${BOTTOM_MARGIN}px)`);
    };
    measure();
    const observer = new ResizeObserver(measure);
    observer.observe(document.body);
    return (): void => observer.disconnect();
  }, [initialized, scopePreset, scopes.size]);

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
      <div style={titleRowStyle}>
        <h2 style={{ margin: 0 }}>
          <FormattedMessage id="trace.viewer.title" />
          {(scopePreset || scopes.size === 1) && scope[0] && `  —  ${scope[0]}`}
        </h2>
        <VscodeButton icon="play" disabled={running} onClick={onRunScope}>
          <FormattedMessage id={running ? 'trace.running' : 'trace.run'} />
        </VscodeButton>
        {scope[1] !== undefined && (
          <LayoutSlider layout={layout} onLayout={setLayout} />
        )}
      </div>

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

      <div
        ref={resultsRef}
        style={{ [PANEL_HEIGHT_VAR]: panelHeight } as React.CSSProperties}
      >
        {scope[1] !== undefined ? (
          <SplitPane
            layout={layout}
            left={
              <DataPanel
                setFilter={requestFilter}
                test={scope[1]}
                trace={
                  runState.status === 'success' ? runState.trace : undefined
                }
                intl={intl}
              />
            }
            right={
              <TraceResult
                filterRequest={filterRequest}
                runState={runState}
                cwd={cwd}
                test={scope[1]}
              />
            }
          />
        ) : (
          <TraceResult
            filterRequest={filterRequest}
            runState={runState}
            cwd={cwd}
          />
        )}
      </div>
    </div>
  );
}

function LayoutSlider({
  layout,
  onLayout,
}: {
  layout: PaneLayout;
  onLayout: (layout: PaneLayout) => void;
}): ReactElement {
  const intl = useIntl();
  const [dragging, setDragging] = useState<number>();
  const index = dragging ?? PANE_LAYOUTS.indexOf(layout);
  const commit = (): void => {
    if (dragging !== undefined) {
      onLayout(PANE_LAYOUTS[dragging]);
      setDragging(undefined);
    }
  };
  return (
    <span style={layoutSliderStyle}>
      <span className="trace-layout-label">
        <FormattedMessage id="trace.layout.data" />
      </span>
      <input
        type="range"
        className="trace-layout-range"
        min={0}
        max={PANE_LAYOUTS.length - 1}
        step={1}
        value={index}
        list="trace-layout-stops"
        aria-label={intl.formatMessage({ id: 'trace.layout.label' })}
        onChange={(e) => setDragging(Number(e.target.value))}
        onPointerUp={commit}
        onKeyUp={commit}
        onBlur={commit}
      />
      <datalist id="trace-layout-stops">
        {PANE_LAYOUTS.map((_, i) => (
          <option key={i} value={i} />
        ))}
      </datalist>
      <span className="trace-layout-label">
        <FormattedMessage id="trace.layout.trace" />
      </span>
    </span>
  );
}

function TraceResult({
  runState,
  cwd,
  test,
  filterRequest,
}: {
  runState: RunState;
  cwd: string;
  test?: TraceTest;
  filterRequest: FilterCommand | null;
}): ReactElement | null {
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
          <pre style={codeBlockStyle}>{runState.message}</pre>
        </div>
      );
    case 'success':
      return (
        <TracePanel
          trace={runState.trace}
          cwd={cwd}
          test={test}
          filterRequest={filterRequest}
        />
      );
  }
}

function post(vscode: WebviewApi<unknown>, message: TraceUpMessage): void {
  vscode.postMessage(message);
}

function SplitPane({
  left,
  right,
  layout,
}: {
  left: ReactElement;
  right: ReactElement;
  layout: PaneLayout;
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

  if (layout !== 'both') {
    return (
      <div style={{ width: '100%', minWidth: 0 }}>
        {layout === 'data' ? left : right}
      </div>
    );
  }

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

const titleRowStyle: React.CSSProperties = {
  display: 'flex',
  alignItems: 'center',
  gap: 16,
};

const layoutSliderStyle: React.CSSProperties = {
  marginLeft: 'auto',
  display: 'inline-flex',
  alignItems: 'center',
  gap: 8,
};

const fieldStyle: React.CSSProperties = {
  display: 'flex',
  flexDirection: 'column',
  gap: 4,
};
