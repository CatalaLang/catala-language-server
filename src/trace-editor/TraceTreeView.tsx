import {
  type CSSProperties,
  type MouseEvent,
  type ReactElement,
  createContext,
  useContext,
  useEffect,
  useState,
} from 'react';
import type { JsonValue } from '../shared/util_client';
import { getVsCodeApi } from '../shared/webviewApi';
import type { TraceDownMessage, TraceUpMessage } from './messages';
import type { CodeLocation, TraceElement, TraceKind } from './traceUtils';
import {
  type TraceValue,
  type TraceTest,
  formatTraceValue,
  str,
  traceValueEqual,
  traceValueFromRuntime,
  traceVariablesForTest,
  uncapitalize,
} from './traceUtils';
import { FormattedMessage, useIntl, type IntlShape } from 'react-intl';

type Match = 'match' | 'mismatch' | undefined;

type Expected = {
  variables: Map<string, TraceValue | null>;
  output: Map<string, Match>;
};

export type ExpandCommand = { open: boolean; nonce: number };

type Tone = 'scope' | 'branch' | 'error' | 'plain';

type Described = {
  symbol: string;
  label: string;
  detail?: string;
  tone: Tone;
  showsValue: boolean;
};

const ExpectedContext = createContext<Expected | null>(null);
const CwdContext = createContext<string>('');
const ExpandContext = createContext<ExpandCommand | null>(null);

function resolvePath(cwd: string, file: string): string {
  if (!cwd || file.startsWith('/') || /^[a-zA-Z]:[\\/]/.test(file)) {
    return file;
  }
  return `${cwd.replace(/[\\/]+$/, '')}/${file}`;
}

function describe(kind: TraceKind, intl: IntlShape): Described {
  const t = (id: string): string => intl.formatMessage({ id });
  switch (kind.kind) {
    case 'scope_call':
      return {
        symbol: '→',
        label: t('trace.kind.scope'),
        detail: str(kind.name),
        tone: 'scope',
        showsValue: true,
      };
    case 'scope_var': {
      const label =
        kind.input === 'reentrant'
          ? t('trace.kind.scopeContextVariable')
          : kind.input === 'only_input'
            ? t('trace.kind.scopeInputVariable')
            : t('trace.kind.scopeVariable');
      return {
        symbol: '≔',
        label,
        detail: str(kind.name),
        tone: 'plain',
        showsValue: true,
      };
    }
    case 'local_var':
      return {
        symbol: '≔',
        label: t('trace.kind.localVariable'),
        detail: str(kind.name),
        tone: 'plain',
        showsValue: true,
      };
    case 'local_tup':
      return {
        symbol: '≔',
        label: t('trace.kind.localVariables'),
        detail: Array.isArray(kind.names)
          ? (kind.names as unknown[]).map(String).join(', ')
          : undefined,
        tone: 'plain',
        showsValue: true,
      };
    case 'function_call':
      return {
        symbol: '→',
        label: t('trace.kind.function'),
        detail: str(kind.name),
        tone: 'scope',
        showsValue: true,
      };
    case 'branch_condition':
      return {
        symbol: '⊡',
        label: t('trace.kind.condition'),
        tone: 'branch',
        showsValue: true,
      };
    case 'if_branching':
      return {
        symbol: '⊸',
        label: t('trace.kind.branchTaken'),
        tone: 'branch',
        showsValue: false,
      };
    case 'match_branching':
      return {
        symbol: '⊸',
        label: t('trace.kind.branchCase'),
        detail: str(kind.constructor),
        tone: 'branch',
        showsValue: false,
      };
    case 'assertion':
      return {
        symbol: '⊹',
        label: t('trace.kind.assertion'),
        tone: 'plain',
        showsValue: false,
      };
    case 'exception':
      return {
        symbol: '⊕',
        label: t('trace.kind.definition'),
        detail: kind.label !== undefined ? str(kind.label) : undefined,
        tone: 'plain',
        showsValue: false,
      };
    case 'error':
      return {
        symbol: '⨉',
        label: t('trace.kind.error'),
        detail: [str(kind.type), str(kind.message)].filter(Boolean).join(': '),
        tone: 'error',
        showsValue: false,
      };
    default:
      return {
        symbol: '•',
        label: kind.kind,
        tone: 'plain',
        showsValue: false,
      };
  }
}

function toneColor(tone: Tone): string | undefined {
  switch (tone) {
    case 'scope':
      return 'var(--vscode-symbolIcon-functionForeground, var(--vscode-terminal-ansiCyan))';
    case 'branch':
      return 'var(--vscode-symbolIcon-keywordForeground, var(--vscode-terminal-ansiBlue))';
    case 'error':
      return 'var(--vscode-errorForeground)';
    default:
      return undefined;
  }
}

/** Related source locations attached to an `error` element. */
function relatedLocations(kind: TraceKind): CodeLocation[] {
  const rp = kind.related_pos;
  return Array.isArray(rp) ? (rp as unknown as CodeLocation[]) : [];
}

function isSingleLine(pos?: CodeLocation): pos is CodeLocation {
  return !!pos && pos.start.line === pos.end.line;
}

function posText(pos?: CodeLocation): string {
  if (!pos) return '';
  const line = pos.start.line;
  return `${pos.file}:${line}`;
}

/** A clickable link that opens the file at the given position in a new tab. */
function formatPos(
  pos: CodeLocation | undefined,
  inline = false
): ReactElement | null {
  const text = posText(pos);
  if (!pos || !text) {
    return null;
  }
  return <PosLink pos={pos} text={text} inline={inline} />;
}

function PosLink({
  pos,
  text,
  inline = false,
}: {
  pos: CodeLocation;
  text: string;
  inline?: boolean;
}): ReactElement {
  const cwd = useContext(CwdContext);
  const intl = useIntl();
  const onClick = (e: MouseEvent): void => {
    e.stopPropagation();
    const message: TraceUpMessage = {
      kind: 'openLocation',
      file: resolvePath(cwd, pos.file),
      start: pos.start ?? { line: 1, character: 1 },
      end: pos.end ?? pos.start ?? { line: 1, character: 1 },
    };
    getVsCodeApi().postMessage(message);
  };
  return (
    <a
      onClick={onClick}
      title={intl.formatMessage({ id: 'trace.openLocation' }, { target: text })}
      style={inline ? posLinkInlineStyle : posLinkStyle}
    >
      {text}
    </a>
  );
}

// -- Source line extraction (lazy request/response with the extension) --------

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

function SourceLine({
  pos,
  text,
}: {
  pos: CodeLocation;
  text: string;
}): ReactElement {
  const cwd = useContext(CwdContext);
  const intl = useIntl();
  const a = Math.max(0, pos.start.character - 1);
  const b = Math.max(a, pos.end.character - 1);
  const before = text.slice(0, a);
  const mid = text.slice(a, b);
  const after = text.slice(b);
  const onClick = (): void => {
    const message: TraceUpMessage = {
      kind: 'openLocation',
      file: resolvePath(cwd, pos.file),
      start: pos.start,
      end: pos.end,
    };
    getVsCodeApi().postMessage(message);
  };
  return (
    <pre
      style={sourceStyle}
      onClick={onClick}
      title={intl.formatMessage(
        { id: 'trace.openLocation' },
        { target: posText(pos) }
      )}
    >
      {before}
      <mark style={markStyle}>{mid || ' '}</mark>
      {after}
    </pre>
  );
}

function LocationExtract({ pos }: { pos: CodeLocation }): ReactElement | null {
  const cwd = useContext(CwdContext);
  if (pos.start.line !== pos.end.line) {
    return null;
  }
  const line = pos.start.line;
  const [source, setSource] = useState<{ text: string | null; line: number }>({
    text: null,
    line,
  });
  useEffect(() => {
    let cancelled = false;
    void fetchExtract(resolvePath(cwd, pos.file), line).then((text) => {
      if (!cancelled) {
        setSource({ line, text });
      }
    });
    return (): void => {
      cancelled = true;
    };
  }, [cwd, pos]);
  return source.text ? <SourceLine pos={pos} text={source.text} /> : null;
}

function asCodeLocation(v: JsonValue | undefined): CodeLocation | undefined {
  if (
    v !== null &&
    typeof v === 'object' &&
    !Array.isArray(v) &&
    typeof (v as { file?: unknown }).file === 'string'
  ) {
    return v as unknown as CodeLocation;
  }
  return undefined;
}

function filterMatches(
  el: TraceElement,
  filter: string,
  intl: IntlShape
): boolean {
  const { label, detail } = describe(el.element, intl);
  const value = el.value !== undefined ? formatTraceValue(el.value) : undefined;
  const text = [
    label,
    detail ?? '',
    value ?? '',
    posText(el.pos),
    JSON.stringify(el.element),
  ]
    .join(' ')
    .toLowerCase();
  return text.includes(filter);
}

function subtreeMatches(
  el: TraceElement,
  filter: string,
  intl: IntlShape
): boolean {
  if (filterMatches(el, filter, intl)) {
    return true;
  }
  const children = Array.isArray(el.trace) ? el.trace : [];
  return children.some((c) => subtreeMatches(c, filter, intl));
}

function nodeMatchState(
  expected: Expected,
  path: string,
  value: TraceValue
): Match {
  const varExp = expected.variables.get(path);
  if (varExp !== undefined && varExp !== null) {
    return traceValueEqual(varExp, value) ? 'match' : 'mismatch';
  }
  return expected.output.get(path);
}

function subtreeHasMismatch(
  el: TraceElement,
  childPrefix: string,
  expected: Expected
): boolean {
  const newPrefix = (c: TraceElement): string => {
    if (
      (c.element.kind === 'scope_call' ||
        c.element.kind === 'scope_var' ||
        c.element.kind === 'local_var') &&
      typeof c.element.name === 'string'
    ) {
      if (childPrefix) {
        return `${childPrefix}.${c.element.name}`;
      } else {
        return c.element.name;
      }
    } else {
      return childPrefix;
    }
  };
  if (
    (el.element.kind === 'scope_call' ||
      el.element.kind === 'scope_var' ||
      el.element.kind === 'local_var') &&
    el.trace !== undefined
  ) {
    const scopeMismatch = el.trace.some((c) =>
      subtreeHasMismatch(c, newPrefix(c), expected)
    );
    if (scopeMismatch) return true;
  }
  if (
    (el.element.kind === 'scope_var' || el.element.kind === 'local_var') &&
    el.value !== undefined
  ) {
    if (nodeMatchState(expected, childPrefix, el.value) === 'mismatch') {
      return true;
    }
  }
  if (el.trace !== undefined) {
    return el.trace.some((c) => subtreeHasMismatch(c, newPrefix(c), expected));
  }
  return false;
}

// -- Components ---------------------------------------------------------------

export default function TraceTreeView({
  trace,
  filter,
  cwd,
  expand,
  test,
}: {
  trace: TraceElement[];
  filter?: string;
  cwd?: string;
  expand?: ExpandCommand | null;
  test?: TraceTest;
}): ReactElement {
  const intl = useIntl();

  let roots: TraceElement[] = trace;
  if (test !== undefined) {
    const testingScope = trace.find(
      (te) =>
        te.element.kind === 'scope_call' &&
        typeof te.element.name === 'string' &&
        test.testing_scope == te.element.name
    );
    if (testingScope !== undefined) {
      roots = testingScope.trace ?? [];
    }
  }

  if (roots.length === 0) {
    return (
      <p style={{ color: 'var(--vscode-descriptionForeground)' }}>
        <FormattedMessage id="trace.empty" />
      </p>
    );
  }

  const f = (filter ?? '').trim().toLowerCase();
  const anyVisible = f ? roots.some((el) => subtreeMatches(el, f, intl)) : true;
  if (!anyVisible) {
    return (
      <p style={{ color: 'var(--vscode-descriptionForeground)' }}>
        <FormattedMessage id="trace.noMatches" />
      </p>
    );
  }

  let expected: Expected | null = null;
  if (test !== undefined) {
    const [, outputs] = traceVariablesForTest(trace, test.tested_scope.name);
    const output: Map<string, Match> = new Map();
    for (const [name, io] of test.test_outputs.entries()) {
      const exp = io?.value ? traceValueFromRuntime(io.value.value) : undefined;
      const computed = outputs[name];
      if (exp !== undefined && computed !== undefined) {
        const match = traceValueEqual(exp, computed) ? 'match' : 'mismatch';
        output.set(name, match);
      }
    }
    expected = { variables: test.variables, output };
  }

  const testedScope = test ? test.tested_scope.name : undefined;

  return (
    <CwdContext.Provider value={cwd ?? ''}>
      <ExpandContext.Provider value={expand ?? null}>
        <ExpectedContext.Provider value={expected}>
          <ul style={rootListStyle}>
            {roots.map((el, i) => (
              <TraceNode
                key={i}
                te={el}
                depth={0}
                filter={f}
                prefix=""
                tested_scope={testedScope}
              />
            ))}
          </ul>
        </ExpectedContext.Provider>
      </ExpandContext.Provider>
    </CwdContext.Provider>
  );
}

function TraceNode({
  te,
  depth,
  filter,
  prefix,
  tested_scope,
}: {
  te: TraceElement;
  depth: number;
  filter: string;
  prefix: string;
  tested_scope?: string;
}): ReactElement | null {
  if (te.element.kind === 'exception' && depth === 1) return null;

  const filtering = filter.length > 0;
  const expected = useContext(ExpectedContext);
  const intl = useIntl();

  const singleLinePos =
    te.element.kind !== 'scope_var' && isSingleLine(te.pos)
      ? te.pos
      : undefined;

  const fulfilled =
    te.element.kind === 'exception' &&
    te.value?.kind === 'bool' &&
    te.value.value === true;
  const consPos = fulfilled ? asCodeLocation(te.element.cons_pos) : undefined;
  const consSingleLine = isSingleLine(consPos) ? consPos : undefined;

  const [te2, name2, isMerged]: [TraceElement, string, boolean] =
    te.element.kind === 'scope_var' &&
    typeof te.element.name === 'string' &&
    te.trace?.length === 1 &&
    te.trace[0].element.kind === 'scope_call' &&
    typeof te.trace[0].element.name === 'string'
      ? [te.trace[0], `${te.element.name}.${te.trace[0].element.name}`, true]
      : [te, te.element.name as string, false];

  const children = te2.trace ?? [];
  const hasChildren = children.length > 0;
  const containerValue =
    te.element.kind !== 'if_branching' &&
    te.element.kind !== 'scope_call' &&
    te.value !== undefined &&
    formatTraceValue(te.value) === undefined
      ? formatTraceValue(te.value, true)
      : undefined;
  const expandable =
    hasChildren ||
    !!singleLinePos ||
    !!consSingleLine ||
    containerValue !== undefined;
  const onlyContainerValue =
    containerValue !== undefined &&
    !hasChildren &&
    !singleLinePos &&
    !consSingleLine;

  const defaultExpanded =
    te2.element.kind === 'assertion'
      ? hasChildren
      : onlyContainerValue
        ? false
        : depth < 1;
  const [expanded, setExpanded] = useState(defaultExpanded);
  useEffect(() => {
    setExpanded(filtering ? true : defaultExpanded);
  }, [filter, defaultExpanded, filtering]);

  const expandCmd = useContext(ExpandContext);
  useEffect(() => {
    if (expandCmd) {
      setExpanded(expandCmd.open);
    }
  }, [expandCmd]);

  let childPrefix: string = prefix;
  let testedScope: string | undefined = tested_scope;
  if (
    (te2.element.kind === 'scope_call' ||
      te2.element.kind === 'scope_var' ||
      te2.element.kind === 'local_var') &&
    typeof te2.element.name === 'string'
  ) {
    if (te2.element.name == tested_scope) {
      testedScope = undefined;
    } else if (isMerged && prefix) {
      childPrefix = `${prefix}.${name2}`;
    } else if (isMerged) {
      childPrefix = name2;
    } else if (prefix) {
      childPrefix = `${prefix}.${te2.element.name}`;
    } else {
      childPrefix = te2.element.name;
    }
  }

  const hasMismatch =
    expected !== null && subtreeHasMismatch(te2, childPrefix, expected);
  useEffect(() => {
    if (hasMismatch) {
      setExpanded(true);
    }
  }, [hasMismatch]);

  const open = expanded;

  if (filtering && !subtreeMatches(te2, filter, intl)) {
    return null;
  }
  const childFilter =
    filtering && !filterMatches(te2, filter, intl) ? filter : '';

  let matchBackground: string | undefined;
  if (
    expected &&
    te2.value !== undefined &&
    (te2.element.kind === 'scope_var' || te2.element.kind === 'local_var') &&
    typeof te2.element.name === 'string'
  ) {
    const path = prefix
      ? `${prefix}.${uncapitalize(te2.element.name)}`
      : uncapitalize(te2.element.name);
    const state = nodeMatchState(expected, path, te2.value);
    if (state !== undefined) {
      matchBackground =
        state === 'mismatch'
          ? 'var(--vscode-diffEditor-removedTextBackground, rgba(255, 50, 50, 0.2))'
          : 'var(--vscode-diffEditor-insertedTextBackground, rgba(35, 200, 60, 0.2))';
    }
  }

  const described: Described = isMerged
    ? {
        symbol: '→',
        label: intl.formatMessage(
          { id: 'trace.computationOf' },
          { name: `${str(te.element.name)} (${str(te2.element.name)})` }
        ),
        tone: 'scope',
        showsValue: true,
      }
    : describe(te2.element, intl);
  // Assertions are coloured by their result: green when satisfied, red when not.
  const accentColor =
    te2.element.kind === 'assertion'
      ? !te2.trace
        ? 'var(--vscode-testing-iconPassed, var(--vscode-charts-green))'
        : 'var(--vscode-errorForeground)'
      : toneColor(described.tone);
  const related =
    te2.element.kind === 'error' ? relatedLocations(te2.element) : [];

  return (
    <li style={liStyle}>
      <div
        style={{
          ...rowStyle,
          cursor: expandable ? 'pointer' : 'default',
          background: matchBackground,
        }}
        onClick={() => expandable && setExpanded((e) => !e)}
      >
        {expandable ? (
          <span
            className={`codicon codicon-chevron-${open ? 'down' : 'right'}`}
            style={chevronStyle}
          />
        ) : (
          <span style={chevronStyle} />
        )}
        <span style={{ ...symbolStyle, color: accentColor }}>
          {described.symbol}
        </span>
        <span style={{ ...labelStyle, color: accentColor }}>
          {described.label}
        </span>
        {described.detail && (
          <span style={detailStyle}>{described.detail}</span>
        )}
        <ValueView te={te} described={described} />
      </div>
      {open && (
        <div style={openContentStyle}>
          {containerValue !== undefined &&
            (onlyContainerValue ? (
              <pre style={containerValueStyle}>{containerValue}</pre>
            ) : (
              <ContainerValue value={containerValue} />
            ))}
          {singleLinePos && <LocationExtract pos={singleLinePos} />}
          {consSingleLine && (
            <>
              <div
                style={{ ...consequenceLabelStyle, color: toneColor('branch') }}
              >
                {'⊸ '}
                <FormattedMessage id="trace.consequence" />
              </div>
              <LocationExtract pos={consSingleLine} />
            </>
          )}
          {related.length > 0 && (
            <div style={relatedStyle}>
              <span style={{ color: 'var(--vscode-descriptionForeground)' }}>
                <FormattedMessage id="trace.relatedLocations" />
              </span>
              {related.map((r, i) => (
                <span key={i}>{formatPos(r, true)}</span>
              ))}
            </div>
          )}
          {hasChildren && (
            <ul style={childListStyle}>
              {children.map((c, i) => (
                <TraceNode
                  key={i}
                  te={c}
                  depth={depth + 1}
                  filter={childFilter}
                  prefix={childPrefix}
                  tested_scope={testedScope}
                />
              ))}
            </ul>
          )}
        </div>
      )}
    </li>
  );
}

function ContainerValue({ value }: { value: string }): ReactElement {
  const [open, setOpen] = useState(false);
  return (
    <div>
      <div style={containerValueLabelStyle} onClick={() => setOpen((o) => !o)}>
        <span
          className={`codicon codicon-chevron-${open ? 'down' : 'right'}`}
          style={chevronStyle}
        />
        <FormattedMessage id="trace.value" />
      </div>
      {open && <pre style={containerValueStyle}>{value}</pre>}
    </div>
  );
}

function ValueView({
  te,
  described,
}: {
  te: TraceElement;
  described: Described;
}): ReactElement | null {
  const intl = useIntl();
  if (te.element.kind === 'exception') {
    const fulfilled = te.value?.kind === 'bool' && te.value.value === true;
    return (
      <span
        style={{
          fontWeight: 600,
          color: fulfilled
            ? 'var(--vscode-testing-iconPassed, var(--vscode-charts-green))'
            : 'var(--vscode-charts-yellow, var(--vscode-descriptionForeground))',
        }}
      >
        <FormattedMessage
          id={fulfilled ? 'trace.fulfilled' : 'trace.notFulfilled'}
        />
      </span>
    );
  }
  if (!described.showsValue || te.value === undefined) {
    return null;
  }
  if (te.value.kind === 'absent') {
    return (
      <span style={valueStyle}>
        = {intl.formatMessage({ id: 'trace.absent' })}
      </span>
    );
  }
  const fv = formatTraceValue(te.value);
  if (fv === undefined) {
    return null;
  }
  return <span style={valueStyle}>= {fv}</span>;
}

// -- Styles -------------------------------------------------------------------

const rootListStyle: CSSProperties = {
  listStyle: 'none',
  margin: 0,
  padding: 0,
  fontFamily: 'var(--vscode-editor-font-family, monospace)',
  fontSize: 'var(--vscode-editor-font-size, 13px)',
  maxHeight: '70vh',
  overflow: 'auto',
};

const childListStyle: CSSProperties = {
  listStyle: 'none',
  margin: 0,
  paddingLeft: 16,
  borderLeft: '1px solid var(--vscode-panel-border, transparent)',
};

const liStyle: CSSProperties = {
  margin: 0,
};

const rowStyle: CSSProperties = {
  display: 'flex',
  alignItems: 'center',
  gap: 6,
  padding: '1px 0',
  whiteSpace: 'nowrap',
  // Size each row to its content so the tree overflows (and the root list
  // scrolls) horizontally rather than truncating long lines.
  width: 'max-content',
  minWidth: '100%',
};

const openContentStyle: CSSProperties = {
  paddingLeft: 16,
};

const containerValueLabelStyle: CSSProperties = {
  display: 'flex',
  alignItems: 'center',
  gap: 6,
  cursor: 'pointer',
  color: 'var(--vscode-descriptionForeground)',
  fontStyle: 'italic',
};

const containerValueStyle: CSSProperties = {
  margin: '2px 0 4px 22px',
  whiteSpace: 'pre-wrap',
  wordBreak: 'break-word',
  fontFamily: 'var(--vscode-editor-font-family, monospace)',
  color: 'var(--vscode-debugTokenExpression-value, var(--vscode-foreground))',
};

const chevronStyle: CSSProperties = {
  display: 'inline-block',
  width: 16,
  flex: '0 0 auto',
  textAlign: 'center',
};

const symbolStyle: CSSProperties = {
  display: 'inline-block',
  width: '1.1em',
  flex: '0 0 auto',
  textAlign: 'center',
};

const labelStyle: CSSProperties = {
  fontWeight: 600,
};

const detailStyle: CSSProperties = {
  color:
    'var(--vscode-symbolIcon-variableForeground, var(--vscode-foreground))',
};

const valueStyle: CSSProperties = {
  color: 'var(--vscode-debugTokenExpression-value, var(--vscode-foreground))',
  overflow: 'hidden',
  textOverflow: 'ellipsis',
};

const sourceStyle: CSSProperties = {
  margin: '2px 0 4px 22px',
  padding: '2px 6px',
  background:
    'var(--vscode-textCodeBlock-background, var(--vscode-editor-background))',
  border: '1px solid var(--vscode-panel-border, transparent)',
  borderRadius: 2,
  overflowX: 'auto',
  whiteSpace: 'pre',
  cursor: 'pointer',
  fontFamily: 'var(--vscode-editor-font-family, monospace)',
};

const markStyle: CSSProperties = {
  background: 'var(--vscode-editor-findMatchHighlightBackground, yellow)',
  color: 'inherit',
  borderRadius: 2,
};

const relatedStyle: CSSProperties = {
  display: 'flex',
  flexDirection: 'column',
  gap: 2,
  margin: '2px 0 4px 22px',
  fontSize: '0.9em',
};

const consequenceLabelStyle: CSSProperties = {
  margin: '2px 0 0 22px',
  fontWeight: 600,
  fontSize: '0.9em',
};

const posLinkStyle: CSSProperties = {
  color: 'var(--vscode-textLink-foreground)',
  fontSize: '0.85em',
  marginLeft: 'auto',
  paddingLeft: 12,
  cursor: 'pointer',
  textDecoration: 'none',
};

const posLinkInlineStyle: CSSProperties = {
  color: 'var(--vscode-textLink-foreground)',
  fontSize: '0.85em',
  cursor: 'pointer',
  textDecoration: 'none',
};
