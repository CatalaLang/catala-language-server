import { type ReactElement, useState } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import { VscodeButton, VscodeTextfield } from '@vscode-elements/react-elements';
import {
  type TraceElement,
  type TraceVariable,
  type TraceValue,
  findTraceValue,
  formatTraceValue,
  traceVariablesForTest,
  traceValueEqual,
  traceValueFromRuntime,
  variablePath,
  variableSegment,
} from '../trace-editor/traceUtils';
import type { Test } from '../generated/catala_types';

type Props = {
  test: Test;
  trace?: TraceElement[];
  runTrace?: boolean;
  onChange(next: Map<string, TraceValue | null>): void;
};

function parseAs(kind: string, s: string): TraceValue | undefined {
  if (s.toLowerCase() === 'absent') return { kind: 'absent' };
  switch (kind) {
    case 'bool':
      if (s === 'true') return { kind: 'bool', value: true };
      if (s === 'false') return { kind: 'bool', value: false };
      return undefined;
    case 'money':
      if (!/^-?\d+(\.\d+)?$/.test(s)) return undefined;
      return { kind: 'money', value: s };
    case 'integer':
      if (!/^-?\d+$/.test(s)) return undefined;
      return { kind: 'integer', value: parseInt(s, 10) };
    case 'decimal': {
      const rat = /^(-?\d+)\/(\d+)$/.exec(s);
      if (rat) {
        return { kind: 'decimal', value: Number(rat[1]) / Number(rat[2]) };
      }
      if (!/^-?\d+(\.\d+)?$/.test(s)) return undefined;
      return { kind: 'decimal', value: parseFloat(s) };
    }
    case 'date': {
      const m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(s);
      if (!m) return undefined;
      return {
        kind: 'date',
        value: { year: +m[1], month: +m[2], day: +m[3] },
      };
    }
    case 'duration': {
      const m = /^(-?\d+)y\s+(-?\d+)m\s+(-?\d+)d$/.exec(s);
      if (!m) return undefined;
      return {
        kind: 'duration',
        value: { years: +m[1], months: +m[2], days: +m[3] },
      };
    }
    case 'enum': {
      return {
        kind: 'enum',
        ctor: s,
      };
    }
    default:
      return undefined;
  }
}

function filterExpectedVariables(
  variables: TraceVariable[],
  outputs: Record<string, TraceValue>,
  testVariables: Map<string, TraceValue | null>,
  prefix = ''
): TraceVariable[] {
  const out: TraceVariable[] = [];
  for (const tv of variables) {
    const pr = variablePath(prefix, tv);
    if (tv.kind === 'step') {
      variables = filterExpectedVariables(tv.variables, {}, testVariables, pr);
      out.push({ ...tv, variables });
    } else if (
      outputs[tv.name] === undefined &&
      testVariables.get(pr) === undefined
    ) {
      out.push(tv);
    }
  }
  return out;
}

export default function ExpectedVariablesEditor({
  test,
  trace,
  runTrace,
  onChange,
}: Props): ReactElement {
  const [showCatalog, setShowCatalog] = useState(false);
  const testVariables: Map<string, TraceValue | null> = new Map();
  test.variables.forEach((rv, name) => {
    const value = rv !== null ? traceValueFromRuntime(rv.value) : null;
    if (value !== undefined) {
      testVariables.set(name, value);
    }
  });

  const [trVariablesAux, outputs] = traceVariablesForTest(
    trace ?? [],
    test.tested_scope.name
  );
  const trVariables = filterExpectedVariables(
    trVariablesAux,
    outputs,
    testVariables
  );

  function computedOf(path: string): TraceValue | undefined {
    return findTraceValue(path, trVariablesAux);
  }

  function setVar(path: string, tv: TraceValue | null): void {
    const next = new Map(testVariables);
    next.set(path, tv);
    onChange(next);
  }

  function remove(path: string): void {
    const next = new Map(testVariables);
    next.delete(path);
    onChange(next);
  }

  return (
    <div className="test-section">
      <h2 className="test-section-title heading-h2">
        <FormattedMessage id="testEditor.variables" />
      </h2>
      <div className="test-inputs data-card">
        <div className="composite-editor">
          {testVariables.size > 0 && (
            <div className="simple-items-vertical">
              {[...testVariables.entries()].map(([path, tv]) => (
                <VariableRow
                  key={path}
                  name={path}
                  expected={tv}
                  computed={computedOf(path)}
                  onSet={setVar}
                  onRemove={remove}
                />
              ))}
            </div>
          )}
          {runTrace !== false && (
            <>
              <div style={{ display: 'flex', justifyContent: 'center' }}>
                <button
                  className="button-action-dvp body-b3"
                  onClick={() => setShowCatalog((s) => !s)}
                >
                  <span
                    className={`codicon codicon-${showCatalog ? 'chevron-down' : 'add'}`}
                  />{' '}
                  <FormattedMessage id="testEditor.addNewVariable" />
                </button>
              </div>
              {showCatalog &&
                (trace === undefined ? (
                  <div
                    style={{
                      display: 'flex',
                      justifyContent: 'center',
                      padding: '1em',
                    }}
                  >
                    <span
                      className="codicon codicon-loading codicon-modifier-spin"
                      style={{ fontSize: '1.5em' }}
                    />
                  </div>
                ) : trVariables.length > 0 ? (
                  <VariableCatalog
                    trVariables={trVariables}
                    outputs={outputs}
                    onAdd={setVar}
                  />
                ) : null)}
            </>
          )}
        </div>
      </div>
    </div>
  );
}

function VariableRow({
  name,
  expected,
  computed,
  onSet,
  onRemove,
}: {
  name: string;
  expected: TraceValue | null;
  computed?: TraceValue;
  onSet(name: string, rv: TraceValue | null): void;
  onRemove(name: string): void;
}): ReactElement {
  const intl = useIntl();

  const [input, setInput] = useState('');
  const expectedStr = expected !== null ? formatTraceValue(expected) : '--';
  const computedStr =
    computed !== undefined ? formatTraceValue(computed) : undefined;

  const mismatch =
    computed !== undefined &&
    expected !== null &&
    !traceValueEqual(expected, computed);

  const kind =
    expected !== null && expected.kind !== 'absent'
      ? expected.kind
      : computed !== undefined
        ? computed.kind
        : undefined;
  const inputEmpty = input.trim() === '';
  const parsedInput = inputEmpty
    ? null
    : kind === undefined
      ? undefined
      : parseAs(kind, input.trim());

  const addDisabled =
    !inputEmpty && kind !== undefined && parsedInput === undefined;

  const applyStr = inputEmpty ? (computedStr ?? '') : input.trim();

  function apply(): void {
    if (parsedInput !== undefined) {
      onSet(name, parsedInput);
      setInput('');
    } else if (computed !== undefined) {
      onSet(name, computed);
      setInput('');
    }
  }

  return (
    <div className="simple-item-vertical atomic-element">
      <label className="item-label body-1" style={{ textTransform: 'none' }}>
        {name}
      </label>
      <div className="expected-variable-row">
        <span
          className="expected-variable-value body-1"
          style={
            mismatch ? { color: 'var(--vscode-errorForeground)' } : undefined
          }
        >
          {expectedStr}
        </span>
        <VscodeTextfield
          value={input}
          placeholder={computedStr ?? expectedStr}
          onInput={(e) =>
            setInput((e.target as { value?: string } | null)?.value ?? '')
          }
          style={{ flex: 1 }}
        />
        <VscodeButton
          secondary
          icon="arrow-left"
          disabled={computed === undefined}
          title={intl.formatMessage(
            { id: 'testEditor.fillComputedVariable' },
            { value: computedStr ?? '' }
          )}
          onClick={() => setInput(computedStr ?? '')}
        />
        <VscodeButton
          secondary
          icon="check"
          disabled={addDisabled}
          title={intl.formatMessage(
            { id: 'testEditor.setVariable' },
            { value: applyStr }
          )}
          onClick={apply}
        />
        <VscodeButton
          secondary
          icon="trash"
          title={intl.formatMessage({ id: 'testEditor.deleteVariable' })}
          onClick={() => onRemove(name)}
        />
      </div>
    </div>
  );
}

function filterByName(vars: TraceVariable[], q: string): TraceVariable[] {
  const out: TraceVariable[] = [];
  for (const v of vars) {
    if (v.name.toLowerCase().includes(q)) {
      out.push(v);
    } else if (v.kind === 'step') {
      const variables = filterByName(v.variables, q);
      if (variables.length > 0) out.push({ ...v, variables });
    }
  }
  return out;
}

function VariableCatalog({
  trVariables,
  outputs,
  onAdd,
}: {
  trVariables: TraceVariable[];
  outputs: Record<string, TraceValue>;
  onAdd(path: string, tv: TraceValue | null): void;
}): ReactElement {
  const intl = useIntl();
  const [query, setQuery] = useState('');
  const q = query.trim().toLowerCase();
  const filtered = q ? filterByName(trVariables, q) : trVariables;
  return (
    <>
      <VscodeTextfield
        value={query}
        placeholder={intl.formatMessage({ id: 'testEditor.filterVariables' })}
        onInput={(e) =>
          setQuery((e.target as { value?: string } | null)?.value ?? '')
        }
        style={{ width: '100%', marginBottom: '0.5em' }}
      />
      <table style={{ borderCollapse: 'collapse', width: '100%' }}>
        <tbody>
          {filtered.map((v, i) =>
            v.kind === 'step' ? (
              <ScopeRow
                key={i}
                node={v}
                prefix=""
                onAdd={onAdd}
                filtering={q !== ''}
              />
            ) : outputs[v.name] === undefined ? (
              <ValueRow key={i} node={v} prefix="" onAdd={onAdd} />
            ) : null
          )}
        </tbody>
      </table>
    </>
  );
}

const firstColStyle = { width: '1.5em' };

function ScopeRow({
  node,
  prefix,
  onAdd,
  filtering,
}: {
  node: Extract<TraceVariable, { kind: 'step' }>;
  prefix: string;
  onAdd(path: string, tv: TraceValue | null): void;
  filtering?: boolean;
}): ReactElement {
  const [open, setOpen] = useState(false);
  const show = open || !!filtering;
  const scopePath = variablePath(prefix, node);
  const cellStyle = {
    background: 'var(--vscode-sideBarSectionHeader-background)',
    paddingBottom: show ? '0.5em' : undefined,
  };
  return (
    <>
      <tr style={{ cursor: 'pointer' }} onClick={() => setOpen((o) => !o)}>
        <td style={{ ...firstColStyle, ...cellStyle }}>
          <span
            className={`codicon codicon-chevron-${show ? 'down' : 'right'}`}
          />
        </td>
        <td style={{ ...cellStyle, fontWeight: 600 }}>
          {variableSegment(node)}
        </td>
        <td
          colSpan={3}
          style={{ ...cellStyle, color: 'var(--vscode-descriptionForeground)' }}
        >
          {scopePath}
        </td>
      </tr>
      {show &&
        node.variables.map((v, i) =>
          v.kind === 'step' ? (
            <ScopeRow
              key={i}
              node={v}
              prefix={scopePath}
              onAdd={onAdd}
              filtering={filtering}
            />
          ) : (
            <ValueRow key={i} node={v} prefix={scopePath} onAdd={onAdd} />
          )
        )}
    </>
  );
}

function ValueRow({
  node,
  prefix,
  onAdd,
}: {
  node: Extract<TraceVariable, { kind: 'value' }>;
  prefix: string;
  onAdd(path: string, tv: TraceValue | null): void;
}): ReactElement | null {
  const intl = useIntl();
  const [input, setInput] = useState('');
  const computed = node.value;
  if (
    computed === undefined ||
    computed.kind === 'struct' ||
    computed.kind === 'array' ||
    (computed.kind === 'enum' && computed.value !== undefined)
  ) {
    return null;
  }
  const path = variablePath(prefix, node);
  const computedStr = formatTraceValue(computed);
  const trimmed = input.trim();
  const addValue = trimmed ? parseAs(computed.kind, trimmed) : null;
  const addDisabled = trimmed !== '' && addValue === undefined;

  return (
    <tr>
      <td style={firstColStyle} />
      <td>{node.name}</td>
      <td>{computedStr ?? ''}</td>
      <td>
        <VscodeTextfield
          value={input}
          placeholder={computedStr ?? ''}
          onInput={(e) =>
            setInput((e.target as { value?: string } | null)?.value ?? '')
          }
          style={{ width: '100%' }}
        />
      </td>
      <td style={{ whiteSpace: 'nowrap' }}>
        <VscodeButton
          secondary
          icon="arrow-left"
          disabled={computedStr === undefined}
          title={intl.formatMessage(
            { id: 'testEditor.fillComputedVariable' },
            { value: computedStr ?? '' }
          )}
          onClick={() => setInput(computedStr ?? '')}
        />
        <VscodeButton
          secondary
          icon="add"
          disabled={addDisabled}
          title={intl.formatMessage({ id: 'testEditor.addVariable' })}
          onClick={() => {
            if (addValue !== undefined) {
              onAdd(path, addValue);
              setInput('');
            }
          }}
        />
      </td>
    </tr>
  );
}
