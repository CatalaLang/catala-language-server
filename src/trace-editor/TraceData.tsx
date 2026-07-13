import type { CSSProperties, ReactElement } from 'react';
import { FormattedMessage } from 'react-intl';
import type { TestIo } from '../generated/catala_types';
import {
  type TraceElement,
  type TraceTest,
  type TraceValue,
  findTraceValue,
  traceVariablesForTest,
  formatTraceValue,
  traceValueFromRuntime,
} from './traceUtils';

// -- Value formatting ---------------------------------------------------------

type Leaf = { path: string; kind: string; value?: string };

function flattenValue(path: string, tv: TraceValue): Leaf[] {
  switch (tv.kind) {
    case 'struct':
      if (Object.keys(tv.fields).length === 0)
        return [{ path, kind: 'struct' }];
      return Object.entries(tv.fields).flatMap(([field, v]) =>
        flattenValue(`${path}.${field}`, v)
      );
    case 'array':
      if (tv.values.length === 0) return [{ path, kind: 'array' }];
      return tv.values.flatMap(([v, label], i) =>
        flattenValue(`${path}[${label ?? i}]`, v)
      );
    case 'enum':
      return tv.value === undefined
        ? [{ path, kind: 'enum', value: tv.ctor }]
        : flattenValue(`${path}.${tv.ctor}`, tv.value);
    default:
      return [{ path, kind: tv.kind, value: formatTraceValue(tv) }];
  }
}

function flattenIo(name: string, io: TestIo): Leaf[] {
  if (!io.value) return [];
  const tv = traceValueFromRuntime(io.value.value);
  return tv === undefined ? [] : flattenValue(name, tv);
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

type VarRow = {
  name: string;
  expected?: string;
  value?: string;
  noExpected?: boolean;
  kind?: string;
};

export function DataPanel({
  test,
  trace,
}: {
  test: TraceTest;
  trace?: TraceElement[];
}): ReactElement {
  const [trVariables, trOutputs] = traceVariablesForTest(
    trace ?? [],
    test.tested_scope.name
  );

  const inputRows: VarRow[] = [...test.test_inputs.entries()].flatMap(
    ([name, io]) =>
      flattenIo(name, io).map((leaf) => ({
        name: leaf.path,
        value: leaf.value,
        noExpected: true,
        kind: leaf.kind,
      }))
  );

  const internalRows: VarRow[] = [...test.variables.entries()].map(
    ([name, expected]) => {
      const computed = findTraceValue(name, trVariables);
      return {
        name,
        expected: expected ? formatTraceValue(expected) : undefined,
        value: computed ? formatTraceValue(computed) : undefined,
        kind: expected ? expected.kind : undefined,
      };
    }
  );

  const outputRows: VarRow[] = [...test.test_outputs.entries()].map(
    ([name, io]) => {
      const tv =
        io.value !== undefined
          ? traceValueFromRuntime(io.value.value)
          : undefined;
      const computed = trOutputs[name];
      return {
        name,
        expected: tv !== undefined ? formatTraceValue(tv) : undefined,
        value: computed !== undefined ? formatTraceValue(computed) : undefined,
        kind: io.value?.value.value.kind,
      };
    }
  );

  return (
    <div style={ioPanelStyle}>
      <table style={tableStyle}>
        <thead>
          <tr>
            <th style={thStyle}>
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
          <SectionRow id="trace.section.inputs" />
          {inputRows.map((r, i) => (
            <VarRowView key={`in-${r.name}-${i}`} row={r} />
          ))}
          <SectionRow id="trace.section.internal" />
          {internalRows.map((r, i) => (
            <VarRowView key={`int-${r.name}-${i}`} row={r} />
          ))}
          <SectionRow id="trace.section.outputs" />
          {outputRows.map((r, i) => (
            <VarRowView key={`out-${r.name}-${i}`} row={r} />
          ))}
        </tbody>
      </table>
    </div>
  );
}

function SectionRow({ id }: { id: string }): ReactElement {
  return (
    <tr>
      <td colSpan={3} style={sectionStyle}>
        <FormattedMessage id={id} />
      </td>
    </tr>
  );
}

function VarRowView({ row }: { row: VarRow }): ReactElement {
  const comparable =
    !row.noExpected && row.expected !== undefined && row.value !== undefined;
  const background = !comparable
    ? undefined
    : row.expected === row.value
      ? 'var(--vscode-diffEditor-insertedTextBackground, rgba(35, 200, 60, 0.2))'
      : 'var(--vscode-diffEditor-removedTextBackground, rgba(255, 50, 50, 0.2))';
  return (
    <tr style={{ background }}>
      <td style={{ ...tdStyle, fontWeight: 600 }}>
        <span style={nameCellStyle}>
          <span style={typeIconStyle} title={row.kind}>
            {typeIcon(row.kind)}
          </span>
          <span>{row.name}</span>
        </span>
      </td>
      {row.noExpected ? (
        <td style={disabledCellStyle}>—</td>
      ) : (
        <td style={tdStyle}>{row.expected ?? ''}</td>
      )}
      <td style={tdStyle}>{row.value ?? ''}</td>
    </tr>
  );
}

const ioPanelStyle: CSSProperties = {
  width: '100%',
  boxSizing: 'border-box',
  padding: '8px 12px',
  border: '1px solid var(--vscode-panel-border, transparent)',
  borderRadius: 2,
  fontSize: '0.9em',
  maxHeight: '70vh',
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
  textAlign: 'left',
  padding: '1px 8px 1px 0',
  verticalAlign: 'top',
};

const sectionStyle: CSSProperties = {
  fontWeight: 600,
  padding: '10px 0 2px',
  borderBottom: '1px solid var(--vscode-panel-border, transparent)',
};

const nameCellStyle: CSSProperties = {
  display: 'inline-flex',
  alignItems: 'baseline',
  gap: 4,
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
  textAlign: 'center',
};
