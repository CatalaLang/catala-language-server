import {
  type CSSProperties,
  type ReactElement,
  createContext,
  useContext,
  useEffect,
  useState,
} from 'react';
import type { IntlShape } from 'react-intl';
import { FormattedMessage } from 'react-intl';
import { VscodeButton } from '@vscode-elements/react-elements';
import type { TestIo } from '../generated/catala_types';
import type { ExpandCommand } from './TraceTreeView';
import {
  type TraceElement,
  type TraceTest,
  type TraceValue,
  type TraceVariable,
  PANEL_HEIGHT_VAR,
  traceVariablesForTest,
  formatTraceValue,
  traceValueFromRuntime,
  variablePath,
  variableSegment,
} from './traceUtils';

const ExpandContext = createContext<ExpandCommand | null>(null);

function useExpandAll(setOpen: (open: boolean) => void): void {
  const command = useContext(ExpandContext);
  useEffect(() => {
    if (command) {
      setOpen(command.open);
    }
  }, [command, setOpen]);
}

type DataNode = {
  label: string;
  path: string;
  kind?: string;
  expected?: string;
  value?: string;
  children?: DataNode[];
  missing?: boolean;
};

function isFoldable(node: DataNode): boolean {
  return node.children !== undefined && node.children.length > 0;
}

function leavesFirst(nodes: DataNode[]): DataNode[] {
  return [...nodes.filter((n) => !isFoldable(n)), ...nodes.filter(isFoldable)];
}

function pathSegments(name: string): string[] {
  return name.split('.').filter((segment) => segment !== '');
}

function insertAt(
  nodes: DataNode[],
  segments: string[],
  prefix: string,
  leaf: DataNode
): DataNode[] {
  const [head, ...rest] = segments;
  if (head === undefined) {
    return nodes;
  }
  const path = prefix ? `${prefix}.${head}` : head;
  if (rest.length === 0) {
    return [...nodes, { ...leaf, label: head, path }];
  }
  const index = nodes.findIndex((node) => node.path === path);
  if (index === -1) {
    return [
      ...nodes,
      { label: head, path, children: insertAt([], rest, path, leaf) },
    ];
  }
  const parent = nodes[index];
  return nodes.with(index, {
    ...parent,
    children: insertAt(parent.children ?? [], rest, path, leaf),
  });
}

function sortTree(nodes: DataNode[]): DataNode[] {
  return leavesFirst(
    nodes.map((node) =>
      node.children === undefined
        ? node
        : { ...node, children: sortTree(node.children) }
    )
  );
}

function isContainer(node: DataNode): boolean {
  return node.kind === 'struct' || node.kind === 'array';
}

function nodesFromTrace(
  variables: TraceVariable[],
  prefix: string,
  expected: Map<string, TraceValue | null>,
  matched: Set<string>,
  showContainers: boolean,
  intl: IntlShape
): DataNode[] {
  return leavesFirst(
    variables.flatMap((variable): DataNode[] => {
      const label = variableSegment(variable);
      const path = variablePath(prefix, variable);
      if (expected.has(path)) {
        matched.add(path);
      }
      if (variable.kind === 'step') {
        return [
          {
            label,
            path,
            children: nodesFromTrace(
              variable.variables,
              path,
              expected,
              matched,
              showContainers,
              intl
            ),
          },
        ];
      }
      const node = buildNode(
        label,
        path,
        expected.get(path) ?? undefined,
        variable.value,
        intl
      );
      return showContainers || !isContainer(node) ? [node] : [];
    })
  );
}

function isStruct(
  v?: TraceValue
): v is Extract<TraceValue, { kind: 'struct' }> {
  return v?.kind === 'struct';
}

function isArray(v?: TraceValue): v is Extract<TraceValue, { kind: 'array' }> {
  return v?.kind === 'array';
}

function isWrapper(
  v?: TraceValue
): v is Extract<TraceValue, { kind: 'enum' }> & { value: TraceValue } {
  return v?.kind === 'enum' && v.value !== undefined;
}

function buildNode(
  label: string,
  path: string,
  expected: TraceValue | undefined,
  computed: TraceValue | undefined,
  intl: IntlShape
): DataNode {
  const shape = expected ?? computed;

  if (isStruct(expected) || isStruct(computed)) {
    const fields = [
      ...new Set([
        ...(isStruct(expected) ? Object.keys(expected.fields) : []),
        ...(isStruct(computed) ? Object.keys(computed.fields) : []),
      ]),
    ];
    if (fields.length === 0) {
      return { label, path, kind: 'struct', value: '{}' };
    }
    return {
      label,
      path,
      kind: 'struct',
      children: leavesFirst(
        fields.map((field) =>
          buildNode(
            field,
            `${path}.${field}`,
            isStruct(expected) ? expected.fields[field] : undefined,
            isStruct(computed) ? computed.fields[field] : undefined,
            intl
          )
        )
      ),
    };
  }

  if (isArray(expected) || isArray(computed)) {
    const exp = isArray(expected) ? expected.values : [];
    const comp = isArray(computed) ? computed.values : [];
    const length = Math.max(exp.length, comp.length);
    if (length === 0) {
      return { label, path, kind: 'array', value: '[]' };
    }
    return {
      label,
      path,
      kind: 'array',
      children: Array.from({ length }, (_, i) => {
        const item = exp[i]?.[1] ?? comp[i]?.[1] ?? String(i);
        return buildNode(
          `[${item}]`,
          `${path}[${item}]`,
          exp[i]?.[0],
          comp[i]?.[0],
          intl
        );
      }),
    };
  }

  const sameCtor =
    !isWrapper(expected) ||
    !isWrapper(computed) ||
    expected.ctor === computed.ctor;
  if (sameCtor && (isWrapper(expected) || isWrapper(computed))) {
    const ctor = isWrapper(expected)
      ? expected.ctor
      : (computed as { ctor: string }).ctor;
    return {
      label,
      path,
      kind: 'enum',
      children: [
        buildNode(
          ctor,
          `${path}.${ctor}`,
          isWrapper(expected) ? expected.value : undefined,
          isWrapper(computed) ? computed.value : undefined,
          intl
        ),
      ],
    };
  }

  return {
    label,
    path,
    kind: shape?.kind,
    expected: expected !== undefined ? leafText(expected, intl) : undefined,
    value: computed !== undefined ? leafText(computed, intl) : undefined,
  };
}

function leafText(v: TraceValue, intl: IntlShape): string | undefined {
  return v.kind === 'enum' && v.value !== undefined
    ? v.ctor
    : formatTraceValue(v, intl);
}

function ioValue(io: TestIo | undefined): TraceValue | undefined {
  return io?.value ? traceValueFromRuntime(io.value.value) : undefined;
}

// -- Type icons ---------------------------------------------------------------

const TYPE_ICON: Record<string, string> = {
  money: '$',
  integer: '#',
  decimal: '≈',
  bool: '✓',
  date: '▦',
  duration: '⧖',
  struct: '{}',
  array: '[]',
  enum: '◆',
};

function typeIcon(kind?: string): string {
  return (kind !== undefined ? TYPE_ICON[kind] : undefined) ?? '·';
}

// -- Components ----------------------------------------------------------------

type SetFilter = (filter: string) => void;

export function DataPanel({
  test,
  setFilter,
  trace,
  intl,
  showContainers = false,
}: {
  test: TraceTest;
  setFilter: SetFilter;
  trace?: TraceElement[];
  intl: IntlShape;
  showContainers?: boolean;
}): ReactElement {
  const [trVariables, trOutputs] = traceVariablesForTest(
    trace ?? [],
    test.tested_scope.name
  );

  const inputNodes = leavesFirst(
    [...test.test_inputs.entries()].map(([name, io]) =>
      buildNode(name, name, undefined, ioValue(io), intl)
    )
  );

  const hasTraceVars = trVariables.length > 0;
  const matched = new Set<string>();
  const auxiliary = trVariables.filter(
    (variable) =>
      variable.kind === 'step' || trOutputs[variable.name] === undefined
  );
  let internalNodes = nodesFromTrace(
    auxiliary,
    '',
    test.variables,
    matched,
    showContainers,
    intl
  );
  for (const [name, expected] of test.variables) {
    if (matched.has(name)) {
      continue;
    }
    const leaf = {
      ...buildNode(name, name, expected ?? undefined, undefined, intl),
      missing: hasTraceVars,
    };
    if (!showContainers && isContainer(leaf)) {
      continue;
    }
    internalNodes = insertAt(internalNodes, pathSegments(name), '', leaf);
  }
  internalNodes = sortTree(internalNodes);

  const outputNodes = leavesFirst(
    [...test.test_outputs.entries()].map(([name, io]) =>
      buildNode(name, name, ioValue(io), trOutputs[name], intl)
    )
  );

  return (
    <div style={ioPanelStyle}>
      <table style={tableStyle}>
        <thead>
          <tr>
            <th style={nameThStyle}>
              <FormattedMessage id="trace.col.name" />
            </th>
            <th style={thStyle}>
              <FormattedMessage id="trace.col.expected" />
            </th>
            <th style={thStyle}>
              <FormattedMessage id="trace.col.value" />
            </th>
          </tr>
        </thead>
        <tbody>
          <Section id="trace.section.inputs" intl={intl} first>
            {inputNodes.map((node, i) => (
              <NodeRow
                key={`in-${node.path}-${i}`}
                node={node}
                crumbs={[]}
                noExpected
                setFilter={setFilter}
              />
            ))}
          </Section>
          <Section id="trace.section.internal" intl={intl}>
            {internalNodes.map((node, i) => (
              <NodeRow
                key={`int-${node.path}-${i}`}
                node={node}
                crumbs={[]}
                setFilter={setFilter}
              />
            ))}
          </Section>
          <Section id="trace.section.outputs" intl={intl}>
            {outputNodes.map((node, i) => (
              <NodeRow
                key={`out-${node.path}-${i}`}
                node={node}
                crumbs={[]}
                setFilter={setFilter}
              />
            ))}
          </Section>
        </tbody>
      </table>
    </div>
  );
}

function Section({
  id,
  intl,
  first,
  children,
}: {
  id: string;
  intl: IntlShape;
  first?: boolean;
  children: ReactElement[];
}): ReactElement {
  const [open, setOpen] = useState(true);
  const [expand, setExpand] = useState<ExpandCommand | null>(null);
  const expandAll = (all: boolean): void => {
    if (all) {
      setOpen(true);
    }
    setExpand((prev) => ({ open: all, nonce: (prev?.nonce ?? 0) + 1 }));
  };

  return (
    <>
      {!first && (
        <tr aria-hidden>
          <td colSpan={3} style={sectionGapStyle} />
        </tr>
      )}
      <tr style={{ cursor: 'pointer' }} onClick={() => setOpen((o) => !o)}>
        <td colSpan={3} style={sectionStyle}>
          <div style={sectionHeaderStyle}>
            <span style={nameCellStyle}>
              <span
                style={chevronStyle}
                className={`codicon codicon-chevron-${open ? 'down' : 'right'}`}
              />
              <FormattedMessage id={id} />
            </span>
            <span
              style={sectionActionsStyle}
              onClick={(e) => e.stopPropagation()}
            >
              <VscodeButton
                icon="expand-all"
                secondary
                title={intl.formatMessage({ id: 'trace.expandAllTitle' })}
                onClick={() => expandAll(true)}
              />
              <VscodeButton
                icon="collapse-all"
                secondary
                title={intl.formatMessage({ id: 'trace.collapseAllTitle' })}
                onClick={() => expandAll(false)}
              />
            </span>
          </div>
        </td>
      </tr>
      {open && (
        <ExpandContext.Provider value={expand}>
          {children}
        </ExpandContext.Provider>
      )}
    </>
  );
}

function Breadcrumb({ crumbs }: { crumbs: string[] }): ReactElement {
  const last = crumbs.length - 1;
  return (
    <>
      {crumbs.map((crumb, i) => (
        <span key={i}>
          {i > 0 && !crumb.startsWith('[') && (
            <span style={crumbSeparatorStyle}>/</span>
          )}
          <span style={i === last ? crumbLastStyle : crumbStyle}>{crumb}</span>
        </span>
      ))}
    </>
  );
}

function NodeRow({
  node,
  crumbs,
  noExpected,
  setFilter,
}: {
  node: DataNode;
  crumbs: string[];
  noExpected?: boolean;
  setFilter: SetFilter;
}): ReactElement {
  const [open, setOpen] = useState(false);
  useExpandAll(setOpen);
  const children = node.children;
  const missing = node.missing;
  const selfCrumbs = [...crumbs, node.label];
  const warning =
    'var(--vscode-inputValidation-warningBackground, rgba(255, 200, 0, 0.2))';

  if (children !== undefined && children.length > 0) {
    return (
      <>
        <tr
          style={{
            cursor: 'pointer',
            background: missing ? warning : undefined,
          }}
          onClick={() => setOpen((o) => !o)}
        >
          <td colSpan={3} style={pathRowStyle}>
            <span style={nameCellStyle}>
              <span
                style={chevronStyle}
                className={`codicon codicon-chevron-${open ? 'down' : 'right'}`}
              />
              <span style={typeIconStyle} title={node.kind}>
                {typeIcon(node.kind)}
              </span>
              <span>
                <Breadcrumb crumbs={selfCrumbs} />
              </span>
            </span>
          </td>
        </tr>
        {open &&
          children.map((child, i) => (
            <NodeRow
              key={`${child.path}-${i}`}
              node={child}
              crumbs={selfCrumbs}
              noExpected={noExpected}
              setFilter={setFilter}
            />
          ))}
      </>
    );
  }

  const comparable =
    !noExpected && node.expected !== undefined && node.value !== undefined;
  const background = missing
    ? warning
    : !comparable
      ? undefined
      : node.expected === node.value
        ? 'var(--vscode-diffEditor-insertedTextBackground, rgba(35, 200, 60, 0.2))'
        : 'var(--vscode-diffEditor-removedTextBackground, rgba(255, 50, 50, 0.2))';

  return (
    <tr style={{ background }}>
      <td style={nameTdStyle}>
        <span style={nameCellStyle}>
          <span style={chevronStyle} />
          <span style={typeIconStyle} title={node.kind}>
            {typeIcon(node.kind)}
          </span>
          <span
            style={{ cursor: 'pointer' }}
            onClick={(e) => {
              e.preventDefault();
              setFilter(node.label);
            }}
          >
            {node.label}
          </span>
        </span>
      </td>
      {noExpected || node.expected === undefined ? (
        <td style={disabledCellStyle}>—</td>
      ) : (
        <td
          style={tdStyle}
          onClick={(e) => {
            e.preventDefault();
            setFilter(node.expected ?? '');
          }}
        >
          {node.expected}
        </td>
      )}
      <td
        style={tdStyle}
        onClick={(e) => {
          e.preventDefault();
          setFilter(node.value ?? '');
        }}
      >
        {node.value ?? ''}
      </td>
    </tr>
  );
}

// -- Styles --------------------------------------------------------------------

const ioPanelStyle: CSSProperties = {
  width: '100%',
  boxSizing: 'border-box',
  border: '1px solid var(--vscode-panel-border, transparent)',
  borderRadius: 2,
  fontSize: '0.9em',
  maxHeight: `var(${PANEL_HEIGHT_VAR}, 70vh)`,
  overflow: 'auto',
};

const tableStyle: CSSProperties = {
  borderCollapse: 'collapse',
  width: '100%',
};

const thStyle: CSSProperties = {
  textAlign: 'left',
  padding: '1px 8px 1px 0',
  color: 'var(--vscode-descriptionForeground)',
  fontWeight: 400,
  borderBottom: '1px solid var(--vscode-panel-border, transparent)',
};

const tdStyle: CSSProperties = {
  cursor: 'pointer',
  textAlign: 'left',
  padding: '1px 8px 1px 0',
  verticalAlign: 'middle',
};

const ROW_INSET = 8;

const nameTdStyle: CSSProperties = {
  ...tdStyle,
  fontWeight: 600,
  paddingLeft: ROW_INSET,
};

const nameThStyle: CSSProperties = {
  ...thStyle,
  paddingLeft: ROW_INSET,
};

const sectionStyle: CSSProperties = {
  fontWeight: 700,
  textTransform: 'uppercase',
  letterSpacing: '0.06em',
  fontSize: '0.9em',
  padding: `6px 6px 5px ${ROW_INSET}px`,
  background: 'var(--vscode-editorGroupHeader-tabsBackground, transparent)',
  color: 'var(--vscode-foreground)',
  borderTop: '1px solid var(--vscode-panel-border, transparent)',
  borderBottom: '1px solid var(--vscode-panel-border, transparent)',
};

const sectionHeaderStyle: CSSProperties = {
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'space-between',
  gap: 8,
};

const sectionActionsStyle: CSSProperties = {
  display: 'flex',
  flex: '0 0 auto',
  gap: 2,
  cursor: 'default',
};

const sectionGapStyle: CSSProperties = {
  height: 16,
  padding: 0,
};

const pathRowStyle: CSSProperties = {
  padding: `2px 6px 2px ${ROW_INSET}px`,
  background: 'var(--vscode-sideBarSectionHeader-background)',
};

const crumbStyle: CSSProperties = {
  color: 'var(--vscode-descriptionForeground)',
  fontWeight: 400,
};

const crumbLastStyle: CSSProperties = {
  fontWeight: 600,
};

const crumbSeparatorStyle: CSSProperties = {
  color: 'var(--vscode-descriptionForeground)',
  margin: '0 0.4em',
};

const nameCellStyle: CSSProperties = {
  display: 'flex',
  alignItems: 'center',
  gap: 4,
};

const chevronStyle: CSSProperties = {
  flex: '0 0 auto',
  width: 16,
  marginRight: 4,
  textAlign: 'center',
  cursor: 'pointer',
};

const typeIconStyle: CSSProperties = {
  flex: '0 0 auto',
  width: '1.1em',
  textAlign: 'center',
  color: 'var(--vscode-descriptionForeground)',
  fontWeight: 400,
};

const disabledCellStyle: CSSProperties = {
  ...tdStyle,
  color: 'var(--vscode-descriptionForeground)',
  opacity: 0.5,
};
