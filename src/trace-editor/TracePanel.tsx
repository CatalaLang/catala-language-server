import type { ReactNode } from 'react';
import {
  type CSSProperties,
  type ReactElement,
  useCallback,
  useEffect,
  useMemo,
  useRef,
  useState,
} from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import { VscodeButton, VscodeTextfield } from '@vscode-elements/react-elements';
import type { Expected, Match, TraceElement, TraceTest } from './traceUtils';
import {
  PANEL_HEIGHT_VAR,
  closestFilterMatch,
  fieldValue,
  flattenHiddenKinds,
  withCodeSpans,
  stepIndexMap,
  subtreeMatches,
  traceValueEqual,
  traceValueFromRuntime,
  traceVariablesForTest,
} from './traceUtils';
import {
  CwdContext,
  ExpandContext,
  ExpectedContext,
  IndexContext,
} from './traceContexts';
import type { AddFilter } from './traceMenu';
import { useTraceMenu } from './traceMenu';
import { FilterPins, type Filter } from '../FilterPin';
import TraceNode from './TraceNode';
import { useCatalaSettings } from '../shared/useSettings';
import { showsTraceKind } from '../shared/settings';

type SetFilter = React.Dispatch<React.SetStateAction<Filter[]>>;

/** A filter pushed into a panel from the outside (data panel, context menu). */
export type FilterCommand = { filter: string; nonce: number };

function createAddFilter(setFilters: SetFilter): AddFilter {
  const addFilter: AddFilter = (filter) => {
    let filterToAdd = filter.trim();
    setFilters((savedFilters) => {
      if (
        filterToAdd == '' ||
        savedFilters.some((elt: Filter) => elt.filter == filterToAdd)
      ) {
        return savedFilters;
      } else {
        return [...savedFilters, { filter: filterToAdd, option: 'include' }];
      }
    });
  };
  return addFilter;
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
  expand?: boolean | null;
  test?: TraceTest;
  fromClosestMatch?: boolean;
}): ReactElement {
  const intl = useIntl();
  const settings = useCatalaSettings();

  trace = withCodeSpans(trace);
  trace = flattenHiddenKinds(trace, (kind) => showsTraceKind(settings, kind));

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

  const f = (filters ?? [])
    .map((filter) => {
      return {
        filter: filter.filter.trim().toLowerCase(),
        option: filter.option,
      };
    })
    .filter((filter) => filter.filter.length > 0);
  const anyVisible = f ? roots.some((el) => subtreeMatches(el, f, intl)) : true;
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
          </IndexContext.Provider>
        </ExpectedContext.Provider>
      </ExpandContext.Provider>
    </CwdContext.Provider>
  );
}

function asPin(filter: string | undefined): Filter[] {
  const trimmed = filter?.trim() ?? '';
  return trimmed === '' ? [] : [{ filter: trimmed, option: 'include' }];
}

export default function TracePanel({
  trace,
  cwd,
  test,
  label,
  filterRequest,
  initialFilter,
  onClose,
  focusOnMount,
  fromClosestMatch,
}: {
  trace: TraceElement[];
  cwd: string;
  test?: TraceTest;
  label?: ReactNode;
  filterRequest?: FilterCommand | null;
  initialFilter?: string;
  onClose?: () => void;
  focusOnMount?: boolean;
  fromClosestMatch?: boolean;
}): ReactElement {
  const intl = useIntl();
  const [expand, setExpand] = useState<boolean | null>(null);
  const [filter, setFilter] = useState<string>('');

  const [savedFilters, setSavedFilters] = useState<Filter[]>(() =>
    asPin(initialFilter)
  );
  const addFilter = createAddFilter(setSavedFilters);

  // Derived panel spawned by viewWithFilter command are stored here
  const [derived, setDerived] = useState<{ id: number; filter: string }[]>([]);
  const nextDerivedId = useRef(1);

  const spawnPanel = useCallback((spawnFilter: string): void => {
    const id = nextDerivedId.current++;
    setDerived((old) => [...old, { id, filter: spawnFilter }]);
  }, []);

  const menuProps = useTraceMenu(
    useMemo(() => ({ spawnPanel, addFilter }), [addFilter])
  );

  useEffect(() => {
    if (filterRequest) {
      addFilter(filterRequest.filter);
    }
  }, [filterRequest]);

  const rootRef = useRef<HTMLDivElement>(null);
  const treeRef = useRef<HTMLUListElement>(null);
  useEffect(() => {
    const target = treeRef.current ?? rootRef.current;
    if (focusOnMount !== true || target === null) {
      return;
    }
    const timer = setTimeout(() => {
      target.focus();
      target.scrollIntoView({ behavior: 'smooth', block: 'nearest' });
    }, 0);
    return (): void => clearTimeout(timer);
  }, [focusOnMount]);

  return (
    <div
      {...menuProps}
      ref={rootRef}
      tabIndex={focusOnMount === true ? -1 : undefined}
    >
      <div
        style={{
          display: 'flex',
          gap: 16,
          alignItems: 'center',
          margin: 0,
        }}
      >
        {label && <span style={{ fontWeight: 600 }}>{label}</span>}
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
          onKeyDown={(e) => {
            if (e.key === 'Enter') {
              e.preventDefault();
              addFilter(filter);
              setFilter('');
            }
          }}
          style={{ flex: 1 }}
        >
          <span
            style={{ cursor: 'pointer' }}
            onClick={(e) => {
              e.preventDefault();
              addFilter(filter);
              setFilter('');
            }}
            className="codicon codicon-save"
            slot="content-after"
          />
        </VscodeTextfield>
        <VscodeButton
          icon="expand-all"
          secondary
          title={intl.formatMessage({ id: 'trace.expandAllTitle' })}
          onClick={() => setExpand(true)}
        >
          <FormattedMessage id="trace.expandAll" />
        </VscodeButton>
        <VscodeButton
          icon="collapse-all"
          secondary
          title={intl.formatMessage({ id: 'trace.collapseAllTitle' })}
          onClick={() => setExpand(false)}
        >
          <FormattedMessage id="trace.collapseAll" />
        </VscodeButton>
      </div>
      <FilterPins filters={savedFilters} setFilters={setSavedFilters} />
      <TraceTreeView
        trace={trace}
        filters={savedFilters}
        cwd={cwd}
        expand={expand}
        fromClosestMatch={fromClosestMatch}
        test={test}
      />
      {derived.map((d) => (
        <div key={d.id} style={derivedPanelStyle}>
          <TracePanel
            trace={trace}
            cwd={cwd}
            test={test}
            fromClosestMatch
            focusOnMount
            initialFilter={d.filter}
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

// -- Styles -------------------------------------------------------------------

const rootListStyle: CSSProperties = {
  listStyle: 'none',
  margin: 0,
  padding: 0,
  fontFamily: 'var(--vscode-editor-font-family, monospace)',
  fontSize: 'var(--vscode-editor-font-size, 13px)',
  maxHeight: `var(${PANEL_HEIGHT_VAR}, 70vh)`,
  overflow: 'auto',
};

const derivedPanelStyle: React.CSSProperties = {
  marginTop: 12,
  paddingTop: 8,
  borderTop: '1px solid var(--vscode-panel-border, transparent)',
};

/** Preformatted block, shared with the error report in `TraceEditor`. */
export const preStyle: React.CSSProperties = {
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
