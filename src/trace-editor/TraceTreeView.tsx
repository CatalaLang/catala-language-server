import {
  type CSSProperties,
  type MouseEvent,
  type ReactElement,
  type ReactNode,
  createContext,
  useCallback,
  useContext,
  useEffect,
  useRef,
  useState,
} from 'react';
import {
  VscodeButton,
  VscodeRadio,
  VscodeRadioGroup,
  VscodeTextfield,
} from '@vscode-elements/react-elements';
import type { JsonValue } from '../shared/util_client';
import type { Filter } from '../shared/util';
import {
  pinBackground,
  splitOnTerms,
  switchFilter,
  titlePin,
} from '../shared/util';
import { getVsCodeApi } from '../shared/webviewApi';
import type { TraceUpMessage } from './messages';
import {
  CwdContext,
  LocationSnippet,
  SpawnPanelContext,
  resolvePath,
} from './LocationSnippet';
import type { CodeLocation, TraceElement, TraceKind } from './traceUtils';
import {
  type TraceValue,
  type TraceTest,
  PANEL_HEIGHT_VAR,
  fieldValue,
  formatTraceValue,
  posText,
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
export type FilterCommand = { filter: string; nonce: number };

type OutputView = 'tree' | 'json';

type Tone = 'scope' | 'branch' | 'error' | 'plain';

type Described = {
  symbol: string;
  label: string;
  detail?: string;
  tone: Tone;
  showsValue: boolean;
  showsCode: boolean;
};

const ExpectedContext = createContext<Expected | null>(null);
const IndexContext = createContext<Map<TraceElement, number>>(new Map());
const ExpandContext = createContext<ExpandCommand | null>(null);
const FilterContext = createContext<Filter[]>([]);

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
        showsCode: true,
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
        showsCode: kind.input !== 'only_input',
      };
    }
    case 'local_var':
      return {
        symbol: '≔',
        label: t('trace.kind.localVariable'),
        detail: detail(kind.name),
        tone: 'plain',
        showsValue: true,
        showsCode: true,
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
        showsCode: true,
      };
    case 'function_call':
      return {
        symbol: '→',
        label: t('trace.kind.function'),
        detail: detail(kind.name),
        tone: 'scope',
        showsValue: true,
        showsCode: true,
      };
    case 'branch_condition':
      return {
        symbol: '⊡',
        label: t('trace.kind.condition'),
        tone: 'branch',
        showsValue: true,
        showsCode: true,
      };
    case 'if_branching':
      return {
        symbol: '⊸',
        label: t('trace.kind.branchTaken'),
        tone: 'branch',
        showsValue: false,
        showsCode: true,
      };
    case 'match_branching':
      return {
        symbol: '⊸',
        label: t('trace.kind.branchCase'),
        detail: detail(kind.constructor as unknown as JsonValue),
        tone: 'branch',
        showsValue: false,
        showsCode: true,
      };
    case 'assertion':
      return {
        symbol: '⊹',
        label: t('trace.kind.assertion'),
        tone: 'plain',
        showsValue: false,
        showsCode: true,
      };
    case 'exception':
      return {
        symbol: '⊕',
        label: t('trace.kind.definition'),
        detail: kind.label !== undefined ? detail(kind.label) : undefined,
        tone: 'plain',
        showsValue: false,
        showsCode: true,
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
        showsCode: true,
      };
    default:
      return {
        symbol: '•',
        label: kind.kind,
        tone: 'plain',
        showsValue: false,
        showsCode: true,
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

function Pill({
  labelId,
  active,
  onToggle,
}: {
  labelId: string;
  active: boolean;
  onToggle: () => void;
}): ReactElement {
  return (
    <span
      onClick={(e) => {
        e.stopPropagation();
        onToggle();
      }}
      style={active ? { ...pillStyle, ...pillActiveStyle } : pillStyle}
    >
      <FormattedMessage id={labelId} />
    </span>
  );
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
  filters: Filter[],
  intl: IntlShape
): [Filter[], boolean] {
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
  let remaining_filters = [];
  let exclusion = false;
  for (let filter of filters) {
    if (text.includes(filter.filter) && filter.option == 'include') {
      continue;
    } else if (filter.option == 'ignore') {
      continue;
    } else if (text.includes(filter.filter) && filter.option == 'exclude') {
      exclusion = true;
    } else {
      remaining_filters.push(filter);
    }
  }
  return [remaining_filters, exclusion];
}

function subtreeMatches(
  el: TraceElement,
  filters: Filter[],
  intl: IntlShape
): boolean {
  let [remaining_filters, forbidden] = filterMatches(el, filters, intl);
  if (forbidden) {
    return false;
  }
  let without_exclude = remaining_filters.filter((f) => f.option != 'exclude');
  const children = Array.isArray(el.trace) ? el.trace : [];
  if (without_exclude.length == 0 && children.length == 0) {
    return true;
  }
  return children.some((c) => subtreeMatches(c, remaining_filters, intl));
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

function stepInto(
  te: TraceElement,
  prefix: string,
  testedScope: string | undefined,
  stepIndices: Map<TraceElement, number>
): {
  children: TraceElement[];
  prefix: string;
  testedScope: string | undefined;
} {
  const merged =
    te.element.kind === 'scope_var' &&
    typeof te.element.name === 'string' &&
    te.trace?.length === 1 &&
    te.trace[0].element.kind === 'scope_call' &&
    typeof te.trace[0].element.name === 'string';
  const node = merged && te.trace ? te.trace[0] : te;
  const displayName = merged
    ? `${te.element.name as string}.${node.element.name as string}`
    : (te.element.name as string);
  let childPrefix = prefix;
  let nextTestedScope = testedScope;
  if (
    (node.element.kind === 'scope_call' ||
      node.element.kind === 'scope_var' ||
      node.element.kind === 'local_var') &&
    typeof node.element.name === 'string'
  ) {
    if (node.element.name === testedScope) {
      nextTestedScope = undefined;
    } else {
      const segment = indexedSegment(node, displayName, stepIndices);
      childPrefix = prefix ? `${prefix}.${segment}` : segment;
    }
  }
  return {
    children: node.trace ?? [],
    prefix: childPrefix,
    testedScope: nextTestedScope,
  };
}

function closestFilterMatch(
  roots: TraceElement[],
  filters: Filter[],
  intl: IntlShape,
  stepIndices: Map<TraceElement, number>,
  testedScope: string | undefined
): {
  roots: TraceElement[];
  prefix: string;
  testedScope: string | undefined;
} {
  let level = roots;
  let prefix = '';
  let scope = testedScope;
  let active = filters;
  let closest = { roots, prefix, testedScope: scope };
  for (;;) {
    const matching = level.filter((el) => subtreeMatches(el, active, intl));
    if (matching.length !== 1) {
      return closest;
    }
    const [only] = matching;
    closest = { roots: [only], prefix, testedScope: scope };
    const [remaining, excluded] = filterMatches(only, active, intl);
    if (excluded || remaining.every((pin) => pin.option === 'exclude')) {
      return closest;
    }
    const stepped = stepInto(only, prefix, scope, stepIndices);
    if (stepped.children.length === 0) {
      return closest;
    }
    level = stepped.children;
    active = remaining;
    prefix = stepped.prefix;
    scope = stepped.testedScope;
  }
}

// -- Components ---------------------------------------------------------------

function asPin(filter: string | undefined): Filter[] {
  const trimmed = filter?.trim() ?? '';
  return trimmed === '' ? [] : [{ filter: trimmed, option: 'include' }];
}

export function TracePanel({
  trace,
  cwd,
  test,
  label,
  filterRequest,
  initialFilter,
  fromClosestMatch,
  onClose,
}: {
  trace: TraceElement[];
  cwd?: string;
  test?: TraceTest;
  label?: ReactNode;
  filterRequest?: FilterCommand | null;
  initialFilter?: string;
  fromClosestMatch?: boolean;
  onClose?: () => void;
}): ReactElement {
  const intl = useIntl();
  const [view, setView] = useState<OutputView>('tree');
  const [expand, setExpand] = useState<ExpandCommand | null>(null);
  const [filter, setFilter] = useState('');
  const [savedFilters, setSavedFilters] = useState<Filter[]>(() =>
    asPin(initialFilter)
  );
  const [derived, setDerived] = useState<{ id: number; filter: string }[]>([]);
  const nextDerivedId = useRef(1);

  const expandAll = (open: boolean): void =>
    setExpand((prev) => ({ open, nonce: (prev?.nonce ?? 0) + 1 }));

  const spawnPanel = useCallback((spawnFilter: string): void => {
    const id = nextDerivedId.current++;
    setDerived((old) => [...old, { id, filter: spawnFilter }]);
  }, []);

  const saveFilter = (newFilter: string): void => {
    const trimmed = newFilter.trim();
    if (trimmed === '') {
      return;
    }
    setSavedFilters((old) =>
      old.some((pin) => pin.filter === trimmed)
        ? old
        : [...old, { filter: trimmed, option: 'include' }]
    );
  };

  const onClickFilter = (clicked: string): void => {
    setSavedFilters((old) =>
      old.map((pin) => (pin.filter === clicked ? switchFilter(pin) : pin))
    );
  };

  const removeFilter = (toRemove: string): void => {
    setSavedFilters((old) => old.filter((pin) => pin.filter !== toRemove));
  };

  useEffect(() => {
    if (filterRequest) {
      saveFilter(filterRequest.filter);
    }
  }, [filterRequest]);

  return (
    <div>
      <div style={panelHeaderStyle}>
        <span style={{ fontWeight: 600 }}>
          {label ?? <FormattedMessage id="trace.label" />}
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
        {onClose && (
          <span
            className="codicon codicon-close"
            role="button"
            title={intl.formatMessage({ id: 'trace.closePanel' })}
            style={{ marginLeft: 'auto', cursor: 'pointer' }}
            onClick={onClose}
          />
        )}
      </div>
      {view === 'tree' ? (
        <>
          <div style={panelToolbarStyle}>
            <VscodeTextfield
              placeholder={intl.formatMessage({
                id: 'trace.filterPlaceholder',
              })}
              value={filter}
              onInput={(e) => setFilter(fieldValue(e))}
              onKeyDown={(e) => {
                if (e.key === 'Enter') {
                  e.preventDefault();
                  saveFilter(filter);
                  setFilter('');
                }
              }}
              style={{ flex: 1 }}
            >
              <span
                className="codicon codicon-save"
                slot="content-after"
                title={intl.formatMessage({ id: 'trace.saveFilter' })}
                style={{ cursor: 'pointer' }}
                onMouseDown={(e) => e.preventDefault()}
                onClick={() => {
                  saveFilter(filter);
                  setFilter('');
                }}
              />
            </VscodeTextfield>
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
          <FilterPins
            filters={savedFilters}
            removeFilter={removeFilter}
            onClickFilter={onClickFilter}
          />
          <SpawnPanelContext.Provider value={spawnPanel}>
            <TraceTreeView
              trace={trace}
              filters={savedFilters}
              cwd={cwd}
              expand={expand}
              test={test}
              fromClosestMatch={fromClosestMatch}
            />
          </SpawnPanelContext.Provider>
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
                  JSON.stringify(trace, null, 2)
                );
              }}
            >
              <FormattedMessage id="trace.copyJson" />
            </VscodeButton>
          </div>
          <pre style={codeBlockStyle}>{JSON.stringify(trace, null, 2)}</pre>
        </>
      )}
      {derived.map((d) => (
        <div key={d.id} style={derivedPanelStyle}>
          <TracePanel
            trace={trace}
            cwd={cwd}
            test={test}
            initialFilter={d.filter}
            fromClosestMatch
            label={
              <FormattedMessage
                id="trace.filteredView"
                values={{ filter: d.filter }}
              />
            }
            onClose={() =>
              setDerived((old) => old.filter((o) => o.id !== d.id))
            }
          />
        </div>
      ))}
    </div>
  );
}

function FilterPins({
  filters,
  removeFilter,
  onClickFilter,
}: {
  filters: Filter[];
  removeFilter: (filter: string) => void;
  onClickFilter: (filter: string) => void;
}): ReactElement | null {
  const intl = useIntl();
  if (filters.length === 0) {
    return null;
  }
  return (
    <div style={pinsStyle}>
      {filters.map((filter) => (
        <span
          key={filter.filter}
          onClick={(e) => {
            e.preventDefault();
            onClickFilter(filter.filter);
          }}
          style={{ ...pinStyle, ...pinBackground(filter.option) }}
          title={titlePin(intl, filter)}
        >
          <span>{filter.filter}</span>
          <span
            className="codicon codicon-close"
            title={intl.formatMessage({ id: 'trace.removeFilter' })}
            style={{ cursor: 'pointer' }}
            onClick={() => removeFilter(filter.filter)}
          />
        </span>
      ))}
    </div>
  );
}

function TraceTreeView({
  trace,
  filters,
  cwd,
  expand,
  test,
  fromClosestMatch = false,
}: {
  trace: TraceElement[];
  filters?: Filter[];
  cwd?: string;
  expand?: ExpandCommand | null;
  test?: TraceTest;
  fromClosestMatch?: boolean;
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
      <p style={treeMessageStyle}>
        <FormattedMessage id="trace.empty" />
      </p>
    );
  }

  const f = (filters ?? [])
    .map((filter) => {
      return {
        filter: filter.filter.trim().toLowerCase(),
        option: filter.option,
      };
    })
    .filter((filter) => filter.filter.length > 0);
  const anyVisible =
    f.length > 0 ? roots.some((el) => subtreeMatches(el, f, intl)) : true;
  if (!anyVisible) {
    return (
      <p style={treeMessageStyle}>
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

  let testedScope = test ? test.tested_scope.name : undefined;
  let rootPrefix = '';
  if (fromClosestMatch && f.length > 0) {
    const closest = closestFilterMatch(
      roots,
      f,
      intl,
      stepIndices,
      testedScope
    );
    roots = closest.roots;
    rootPrefix = closest.prefix;
    testedScope = closest.testedScope;
  }

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
                    prefix={rootPrefix}
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
  filters: Filter[];
  prefix: string;
  tested_scope?: string;
}): ReactElement | null {
  if (te.element.kind === 'exception' && depth === 1) return null;

  const filtering = filters.length > 0;
  const filterKey = filters.map((f) => `${f.option}:${f.filter}`).join('\n');
  const expected = useContext(ExpectedContext);
  const stepIndices = useContext(IndexContext);
  const intl = useIntl();

  const fulfilled =
    te.element.kind === 'exception' &&
    te.value?.kind === 'bool' &&
    te.value.value === true;
  const consPos = fulfilled ? asCodeLocation(te.element.cons_pos) : undefined;

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
  const related =
    node.element.kind === 'error' ? relatedLocations(node.element) : [];
  const expandable = hasChildren || !!consPos || related.length > 0;

  const defaultExpanded =
    node.element.kind === 'assertion' ? hasChildren : depth < 1;
  const [expanded, setExpanded] = useState(defaultExpanded);
  const [showValue, setShowValue] = useState(false);
  const [showCode, setShowCode] = useState(false);
  useEffect(() => {
    setExpanded(
      filters.some((f) => f.option == 'include') ? true : defaultExpanded
    );
  }, [filterKey, defaultExpanded, filtering]);

  const expandCmd = useContext(ExpandContext);
  useEffect(() => {
    if (expandCmd) {
      setExpanded(expandCmd.open);
      setShowValue(expandCmd.open);
      setShowCode(expandCmd.open);
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
  const [childFilters] = filtering ? filterMatches(node, filters, intl) : [[]];
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
        showsCode: true,
      }
    : describe(node.element, intl);
  const snippetPos = described.showsCode ? te.pos : undefined;
  const accentColor =
    node.element.kind === 'assertion'
      ? !node.trace
        ? 'var(--vscode-testing-iconPassed, var(--vscode-charts-green))'
        : 'var(--vscode-errorForeground)'
      : toneColor(described.tone);

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
        {(containerValue !== undefined || snippetPos) && (
          <span style={pillsStyle}>
            {containerValue !== undefined && (
              <Pill
                labelId="trace.value"
                active={showValue}
                onToggle={() => setShowValue((v) => !v)}
              />
            )}
            {snippetPos && (
              <Pill
                labelId="trace.code"
                active={showCode}
                onToggle={() => setShowCode((v) => !v)}
              />
            )}
          </span>
        )}
      </div>
      {showValue && containerValue !== undefined && (
        <div style={openContentStyle}>
          <pre style={containerValueStyle}>{containerValue}</pre>
        </div>
      )}
      {showCode && snippetPos && (
        <div style={openContentStyle}>
          <LocationSnippet pos={snippetPos} />
        </div>
      )}
      {open && (
        <div style={openContentStyle}>
          {consPos && (
            <>
              <div
                style={{ ...consequenceLabelStyle, color: toneColor('branch') }}
              >
                {'⊸ '}
                <FormattedMessage id="trace.consequence" />
              </div>
              <LocationSnippet pos={consPos} />
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

const panelHeaderStyle: CSSProperties = {
  display: 'flex',
  gap: 16,
  alignItems: 'center',
  margin: 0,
};

const derivedPanelStyle: CSSProperties = {
  marginTop: 12,
  paddingTop: 8,
  borderTop: '1px solid var(--vscode-panel-border, transparent)',
};

const panelToolbarStyle: CSSProperties = {
  display: 'flex',
  gap: 8,
  alignItems: 'center',
  margin: '8px 0',
};

const pinsStyle: CSSProperties = {
  display: 'flex',
  flexWrap: 'wrap',
  gap: 6,
  margin: '0 0 8px 0',
};

const pinStyle: CSSProperties = {
  display: 'inline-flex',
  alignItems: 'center',
  gap: 4,
  padding: '2px 6px',
  borderRadius: 4,
  color: 'var(--vscode-badge-foreground)',
  fontFamily: 'var(--vscode-editor-font-family, monospace)',
  cursor: 'pointer',
};

export const codeBlockStyle: CSSProperties = {
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

const treeFrameStyle: CSSProperties = {
  border: '1px solid var(--vscode-panel-border, var(--vscode-contrastBorder))',
  borderRadius: 4,
  padding: 6,
};

const rootListStyle: CSSProperties = {
  ...treeFrameStyle,
  listStyle: 'none',
  margin: 0,
  fontFamily: 'var(--vscode-editor-font-family, monospace)',
  fontSize: 'var(--vscode-editor-font-size, 13px)',
  maxHeight: `var(${PANEL_HEIGHT_VAR}, 70vh)`,
  overflow: 'auto',
};

const treeMessageStyle: CSSProperties = {
  ...treeFrameStyle,
  margin: 0,
  color: 'var(--vscode-descriptionForeground)',
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

const pillsStyle: CSSProperties = {
  display: 'inline-flex',
  gap: 4,
  marginLeft: 16,
};

const pillStyle: CSSProperties = {
  cursor: 'pointer',
  userSelect: 'none',
  fontSize: '0.8em',
  padding: '0 6px',
  borderRadius: 8,
  border: '1px solid currentColor',
  color: 'var(--vscode-descriptionForeground)',
};

const pillActiveStyle: CSSProperties = {
  background: 'var(--vscode-badge-background)',
  color: 'var(--vscode-badge-foreground)',
  borderColor: 'transparent',
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

const filterMatchStyle: CSSProperties = {
  backgroundColor:
    'var(--vscode-editor-findMatchHighlightBackground, rgba(234, 92, 0, 0.33))',
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
