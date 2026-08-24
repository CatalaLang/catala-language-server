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
import { splitOnTerms } from '../shared/util';
import { getVsCodeApi } from '../shared/webviewApi';
import type { TraceDownMessage, TraceUpMessage } from './messages';
import type { CodeLocation, TraceElement, TraceKind } from './traceUtils';
import {
  type TraceValue,
  type TraceTest,
  formatTraceValue,
  traceValueEqual,
  traceValueFromRuntime,
  traceVariablesForTest,
  stepIndexMap,
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
const IndexContext = createContext<Map<TraceElement, number>>(new Map());
const CwdContext = createContext<string>('');
const ExpandContext = createContext<ExpandCommand | null>(null);
/**
 * The filters the user typed or saved, for highlighting purposes only.
 * `TraceNode` cannot use its own `filters` prop for that: it hands an empty list
 * to the children of a node that matches (so that a matching subtree is shown
 * whole), which would leave those children unhighlighted.
 */
const FilterContext = createContext<string[]>([]);

/**
 * Renders `text` with the parts matched by the filters highlighted, so that the
 * user sees what kept an entry in the tree.
 */
function Highlight({ text }: { text: string }): ReactElement {
  const filters = useContext(FilterContext);
  return (
    <>
      {splitOnTerms(text, filters).map((chunk, index) =>
        chunk.match ? (
          <mark key={index} style={filterMatchStyle}>
            {chunk.text}
          </mark>
        ) : (
          chunk.text
        )
      )}
    </>
  );
}

function resolvePath(cwd: string, file: string): string {
  if (!cwd || file.startsWith('/') || /^[a-zA-Z]:[\\/]/.test(file)) {
    return file;
  }
  return `${cwd.replace(/[\\/]+$/, '')}/${file}`;
}

function detail(x: JsonValue): string {
  return typeof x === 'string' ? x : '';
}

function describe(kind: TraceKind, intl: IntlShape): Described {
  const t = (id: string): string => intl.formatMessage({ id });
  switch (kind.kind) {
    case 'scope_call':
      return {
        symbol: '→',
        label: t('trace.kind.scope'),
        detail: detail(kind.name),
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
        detail: detail(kind.name),
        tone: 'plain',
        showsValue: true,
      };
    }
    case 'local_var':
      return {
        symbol: '≔',
        label: t('trace.kind.localVariable'),
        detail: detail(kind.name),
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
        detail: detail(kind.name),
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
        detail: detail(kind.constructor as unknown as JsonValue),
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
        detail: kind.label !== undefined ? detail(kind.label) : undefined,
        tone: 'plain',
        showsValue: false,
      };
    case 'error':
      return {
        symbol: '⨉',
        label: t('trace.kind.error'),
        detail: [detail(kind.type), detail(kind.message)]
          .filter(Boolean)
          .join(': '),
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
      <Highlight text={text} />
    </a>
  );
}

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

/**
 * An entry matches when **every** term is found in its own text: the terms
 * accumulate, exactly like the saved filters of the general test list.
 */
function filterMatches(
  el: TraceElement,
  filters: string[],
  intl: IntlShape
): boolean {
  const { label, detail } = describe(el.element, intl);
  const value =
    el.value !== undefined ? formatTraceValue(el.value, intl) : undefined;
  const text = [
    label,
    detail ?? '',
    value ?? '',
    posText(el.pos),
    JSON.stringify(el.element),
  ]
    .join(' ')
    .toLowerCase();
  return filters.every((filter) => text.includes(filter));
}

function subtreeMatches(
  el: TraceElement,
  filters: string[],
  intl: IntlShape
): boolean {
  if (filterMatches(el, filters, intl)) {
    return true;
  }
  const children = Array.isArray(el.trace) ? el.trace : [];
  return children.some((c) => subtreeMatches(c, filters, intl));
}

function indexedSegment(
  el: TraceElement,
  name: string,
  stepIndices: Map<TraceElement, number>
): string {
  const index = stepIndices.get(el);
  return index !== undefined ? `${name}[${index}]` : name;
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
  expected: Expected,
  stepIndices: Map<TraceElement, number>
): boolean {
  const newPrefix = (c: TraceElement): string => {
    if (
      (c.element.kind === 'scope_call' ||
        c.element.kind === 'scope_var' ||
        c.element.kind === 'local_var') &&
      typeof c.element.name === 'string'
    ) {
      const segment = indexedSegment(c, c.element.name, stepIndices);
      return childPrefix ? `${childPrefix}.${segment}` : segment;
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
      subtreeHasMismatch(c, newPrefix(c), expected, stepIndices)
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
    return el.trace.some((c) =>
      subtreeHasMismatch(c, newPrefix(c), expected, stepIndices)
    );
  }
  return false;
}

// -- Components ---------------------------------------------------------------

export default function TraceTreeView({
  trace,
  filters,
  cwd,
  expand,
  test,
}: {
  trace: TraceElement[];
  filters?: string[];
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

  // Normalised once here: the matching is case insensitive, and a blank term
  // would match everything, so it is dropped rather than kept as a no-op.
  const f = (filters ?? [])
    .map((filter) => filter.trim().toLowerCase())
    .filter((filter) => filter.length > 0);
  const anyVisible =
    f.length > 0 ? roots.some((el) => subtreeMatches(el, f, intl)) : true;
  if (!anyVisible) {
    return (
      <p style={{ color: 'var(--vscode-descriptionForeground)' }}>
        <FormattedMessage id="trace.noMatches" />
      </p>
    );
  }

  let expected: Expected | null = null;
  let stepIndices: Map<TraceElement, number> = new Map();
  if (test !== undefined) {
    stepIndices = stepIndexMap(trace);
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
          <IndexContext.Provider value={stepIndices}>
            <FilterContext.Provider value={f}>
              <ul style={rootListStyle}>
                {roots.map((el, i) => (
                  <TraceNode
                    key={i}
                    te={el}
                    depth={0}
                    filters={f}
                    prefix=""
                    tested_scope={testedScope}
                  />
                ))}
              </ul>
            </FilterContext.Provider>
          </IndexContext.Provider>
        </ExpectedContext.Provider>
      </ExpandContext.Provider>
    </CwdContext.Provider>
  );
}

function TraceNode({
  te,
  depth,
  filters,
  prefix,
  tested_scope,
}: {
  te: TraceElement;
  depth: number;
  filters: string[];
  prefix: string;
  tested_scope?: string;
}): ReactElement | null {
  if (te.element.kind === 'exception' && depth === 1) return null;

  const filtering = filters.length > 0;
  // `filters` is a fresh array on every render, so it cannot be used as an
  // effect dependency: the effect below would re-run each time and keep
  // resetting the manual expand/collapse state. This joined key compares by
  // value instead. The separator is a character no filter can contain.
  const filterKey = filters.join('\n');
  const expected = useContext(ExpectedContext);
  const stepIndices = useContext(IndexContext);
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

  const [node, displayName, isMerged]: [TraceElement, string, boolean] =
    te.element.kind === 'scope_var' &&
    typeof te.element.name === 'string' &&
    te.trace?.length === 1 &&
    te.trace[0].element.kind === 'scope_call' &&
    typeof te.trace[0].element.name === 'string'
      ? [te.trace[0], `${te.element.name}.${te.trace[0].element.name}`, true]
      : [te, te.element.name as string, false];

  const children = node.trace ?? [];
  const hasChildren = children.length > 0;
  const containerValue =
    te.element.kind !== 'if_branching' &&
    te.element.kind !== 'scope_call' &&
    te.value !== undefined &&
    formatTraceValue(te.value, intl) === undefined
      ? formatTraceValue(te.value, intl, 'en', true)
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
    node.element.kind === 'assertion'
      ? hasChildren
      : onlyContainerValue
        ? false
        : depth < 1;
  const [expanded, setExpanded] = useState(defaultExpanded);
  useEffect(() => {
    setExpanded(filtering ? true : defaultExpanded);
  }, [filterKey, defaultExpanded, filtering]);

  const expandCmd = useContext(ExpandContext);
  useEffect(() => {
    if (expandCmd) {
      setExpanded(expandCmd.open);
    }
  }, [expandCmd]);

  let childPrefix: string = prefix;
  let testedScope: string | undefined = tested_scope;
  if (
    (node.element.kind === 'scope_call' ||
      node.element.kind === 'scope_var' ||
      node.element.kind === 'local_var') &&
    typeof node.element.name === 'string'
  ) {
    if (node.element.name == tested_scope) {
      testedScope = undefined;
    } else {
      const segment = indexedSegment(node, displayName, stepIndices);
      childPrefix = prefix ? `${prefix}.${segment}` : segment;
    }
  }

  const hasMismatch =
    expected !== null &&
    subtreeHasMismatch(node, childPrefix, expected, stepIndices);
  useEffect(() => {
    if (hasMismatch) {
      setExpanded(true);
    }
  }, [hasMismatch]);

  const open = expanded;

  if (filtering && !subtreeMatches(node, filters, intl)) {
    return null;
  }
  const childFilters =
    filtering && !filterMatches(node, filters, intl) ? filters : [];

  let matchBackground: string | undefined;
  if (
    expected &&
    node.value !== undefined &&
    (node.element.kind === 'scope_var' || node.element.kind === 'local_var') &&
    typeof node.element.name === 'string'
  ) {
    const path = prefix ? `${prefix}.${node.element.name}` : node.element.name;
    const state = nodeMatchState(expected, path, node.value);
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
          { name: `${detail(te.element.name)} (${detail(node.element.name)})` }
        ),
        tone: 'scope',
        showsValue: true,
      }
    : describe(node.element, intl);
  const accentColor =
    node.element.kind === 'assertion'
      ? !node.trace
        ? 'var(--vscode-testing-iconPassed, var(--vscode-charts-green))'
        : 'var(--vscode-errorForeground)'
      : toneColor(described.tone);
  const related =
    node.element.kind === 'error' ? relatedLocations(node.element) : [];

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
          <Highlight text={described.label} />
        </span>
        {described.detail && (
          <span style={detailStyle}>
            <Highlight text={described.detail} />
          </span>
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
                  filters={childFilters}
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
        = <Highlight text={intl.formatMessage({ id: 'trace.absent' })} />
      </span>
    );
  }
  const fv = formatTraceValue(te.value, intl);
  if (fv === undefined) {
    return null;
  }
  return (
    <span style={valueStyle}>
      = <Highlight text={fv} />
    </span>
  );
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

// Same colors as the editor's search highlight. `color: inherit` cancels the
// black on yellow a browser applies to <mark> by default, which would fight
// with the tone colors of the tree, and nothing here may alter the metrics of
// the text: the rows are laid out on a single line.
const filterMatchStyle: CSSProperties = {
  backgroundColor:
    'var(--vscode-editor-findMatchHighlightBackground, rgba(234, 92, 0, 0.33))',
  color: 'inherit',
  borderRadius: 2,
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
