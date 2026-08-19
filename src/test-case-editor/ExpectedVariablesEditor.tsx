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
import type { Test, VariableFailure } from '../generated/catala_types';

type Props = {
  test: Test;
  trace?: TraceElement[];
  runTrace?: boolean;
  /**
   * Mismatches reported by the compiler for the last run, which checked the
   * expected values against the trace itself. This is the authoritative
   * verdict: the row-level comparison below is only a local hint, computed
   * from the trace the editor happens to hold.
   */
  failures?: VariableFailure[];
  onChange(next: Map<string, TraceValue | null>): void;
};

function isolateStateVariable(
  variables: TraceVariable[]
): [TraceVariable[], Map<string, TraceVariable[]>] {
  let stateVariables = new Map<string, TraceVariable[]>();
  let valueVariables = [];
  for (let nodeVar of variables.filter(
    (v): v is Extract<TraceVariable, { kind: 'value' }> => v.kind === 'value'
  )) {
    if (!nodeVar.name.includes('#')) {
      valueVariables.push(nodeVar);
    } else {
      let stateVar = nodeVar.name.split('#');
      if (stateVar.length != 2) {
        console.log(`Unexpected state variable ${nodeVar.name}`);
        continue;
      }
      let existingState = stateVariables.get(stateVar[0]) ?? [];
      // let stateNodeVar = { ...nodeVar, name: stateVar[1] };
      existingState.push(nodeVar);
      stateVariables.set(stateVar[0], existingState);
    }
  }
  return [valueVariables, stateVariables];
}

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

// Only the presence of a name matters here, hence the value being left opaque.
function filterExpectedVariables(
  variables: TraceVariable[],
  outputs: Record<string, TraceValue>,
  testVariables: Map<string, unknown>,
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
  failures,
  onChange,
}: Props): ReactElement {
  const [showCatalog, setShowCatalog] = useState(false);
  const failureByName = new Map(
    (failures ?? []).map((failure) => [failure.name, failure])
  );
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
                  failure={failureByName.get(path)}
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
  failure,
  onSet,
  onRemove,
}: {
  name: string;
  expected: TraceValue | null;
  computed?: TraceValue;
  failure?: VariableFailure;
  onSet(name: string, rv: TraceValue | null): void;
  onRemove(name: string): void;
}): ReactElement {
  const intl = useIntl();

  const [input, setInput] = useState('');
  const expectedStr = expected !== null ? formatTraceValue(expected) : '--';
  const computedStr =
    computed !== undefined ? formatTraceValue(computed) : undefined;

  // A reported failure wins over the local comparison: it was computed by the
  // compiler against the trace, with the runtime's own formatting rules, so it
  // is right where `traceValueEqual` on re-parsed JSON values may not be.
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
          // The compiler reports both sides as already-rendered strings, so the
          // mismatch can be shown verbatim.
          title={
            failure !== undefined
              ? intl.formatMessage(
                  { id: 'testEditor.variableMismatch' },
                  {
                    expected: failure.expected,
                    actual: failure.current_value ?? '--',
                  }
                )
              : undefined
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

type AddVariable = (path: string, tv: TraceValue | null) => void;

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
  onAdd: AddVariable;
}): ReactElement {
  const intl = useIntl();
  const [query, setQuery] = useState('');
  const q = query.trim().toLowerCase();
  const filtered = q ? filterByName(trVariables, q) : trVariables;
  let [variables, stateVariables] = isolateStateVariable(filtered);
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
          {[...stateVariables.entries()].map(([stateName, nodes]) => (
            <StateRow
              varName={stateName}
              nodes={nodes}
              crumbs={[]}
              onAdd={onAdd}
            />
          ))}
          {variables
            .filter(
              (v): v is Extract<TraceVariable, { kind: 'value' }> =>
                v.kind === 'value' && outputs[v.name] === undefined
            )
            .map((v, i) => (
              <ValueRow key={`v-${i}`} node={v} crumbs={[]} onAdd={onAdd} />
            ))}
          {filtered
            .filter(
              (v): v is Extract<TraceVariable, { kind: 'step' }> =>
                v.kind === 'step'
            )
            .map((v, i) => (
              <StepRow
                key={`s-${i}`}
                node={v}
                crumbs={[]}
                onAdd={onAdd}
                filtering={q !== ''}
              />
            ))}
        </tbody>
      </table>
    </>
  );
}

const firstColStyle = { width: '1.5em' };
const stepBorder = '1px solid var(--vscode-sideBarSectionHeader-background)';

function toDisplayCrumbs(crumbs: string[]): { text: string; index: boolean }[] {
  const out: { text: string; index: boolean }[] = [];
  for (const c of crumbs) {
    const m = /^(.+?)(\[\d+\])$/.exec(c);
    if (m) {
      out.push({ text: m[1], index: false }, { text: m[2], index: true });
    } else {
      out.push({ text: c, index: false });
    }
  }
  return out;
}

function Breadcrumb({ crumbs }: { crumbs: string[] }): ReactElement {
  const display = toDisplayCrumbs(crumbs);
  const boldIdx = display.reduce((acc, c, i) => (c.index ? acc : i), -1);
  return (
    <>
      {display.map((c, i) => (
        <span key={i}>
          {i > 0 && (
            <span
              style={{
                color: 'var(--vscode-descriptionForeground)',
                margin: '0 0.4em',
              }}
            >
              /
            </span>
          )}
          <span
            style={
              i === boldIdx
                ? { fontWeight: 600 }
                : { color: 'var(--vscode-descriptionForeground)' }
            }
          >
            {c.text}
          </span>
        </span>
      ))}
    </>
  );
}

function StepRow({
  node,
  crumbs,
  onAdd,
  filtering,
}: {
  node: Extract<TraceVariable, { kind: 'step' }>;
  crumbs: string[];
  onAdd: AddVariable;
  filtering?: boolean;
}): ReactElement {
  const [open, setOpen] = useState(false);
  const show = open || !!filtering;
  const selfCrumbs = [...crumbs, ...variableSegment(node).split('.')];
  const cellStyle = {
    background: 'var(--vscode-sideBarSectionHeader-background)',
    borderTop: stepBorder,
    borderBottom: show ? undefined : stepBorder,
    paddingBottom: show ? '0.5em' : undefined,
  };

  let [variables, stateVariables] = isolateStateVariable(node.variables);

  return (
    <>
      <tr style={{ cursor: 'pointer' }} onClick={() => setOpen((o) => !o)}>
        <td style={{ ...firstColStyle, ...cellStyle }}>
          <span
            className={`codicon codicon-chevron-${show ? 'down' : 'right'}`}
          />
        </td>
        <td colSpan={4} style={cellStyle}>
          <Breadcrumb crumbs={selfCrumbs} />
        </td>
      </tr>
      {show && (
        <>
          {variables
            .filter(
              (v): v is Extract<TraceVariable, { kind: 'value' }> =>
                v.kind === 'value' && !v.name.includes('#')
            )
            .map((v, i) => (
              <ValueRow
                key={`v-${i}`}
                node={v}
                crumbs={selfCrumbs}
                onAdd={onAdd}
              />
            ))}
          {[...stateVariables.entries()].map(([stateName, nodes]) => (
            <StateRow
              varName={stateName}
              nodes={nodes}
              crumbs={selfCrumbs}
              onAdd={onAdd}
              filtering={filtering}
            />
          ))}
          {node.variables
            .filter(
              (v): v is Extract<TraceVariable, { kind: 'step' }> =>
                v.kind === 'step'
            )
            .map((v, i) => (
              <StepRow
                key={`s-${i}`}
                node={v}
                crumbs={selfCrumbs}
                onAdd={onAdd}
                filtering={filtering}
              />
            ))}
        </>
      )}
    </>
  );
}

function StateRow({
  varName,
  nodes,
  crumbs,
  onAdd,
  filtering,
}: {
  varName: string;
  nodes: TraceVariable[];
  crumbs: string[];
  onAdd: AddVariable;
  filtering?: boolean;
}): ReactElement {
  const [open, setOpen] = useState(false);
  const show = open || !!filtering;
  return (
    <>
      <tr className="state-row" onClick={() => setOpen((o) => !o)}>
        <td style={firstColStyle} />
        {/* The cell must stay a real table cell for `colSpan` to apply, so the
            flex layout lives on an inner element rather than on the `td`
            itself. Same shape as `StepRow` above, which spans correctly. */}
        <td colSpan={4}>
          <span style={{ display: 'flex', alignItems: 'center' }}>
            <span
              style={{ paddingRight: '0.2em' }}
              className={`codicon codicon-chevron-${show ? 'down' : 'right'}`}
            />
            {varName}
          </span>
        </td>
      </tr>
      {show && (
        <>
          {nodes
            .filter(
              (v): v is Extract<TraceVariable, { kind: 'value' }> =>
                v.kind === 'value'
            )
            .map((v, i) => (
              <ValueRow
                padding={true}
                key={`v-${i}`}
                node={v}
                crumbs={crumbs}
                onAdd={onAdd}
              />
            ))}
        </>
      )}
    </>
  );
}

function ValueRow({
  node,
  crumbs,
  padding,
  onAdd,
}: {
  node: Extract<TraceVariable, { kind: 'value' }>;
  crumbs: string[];
  padding?: boolean | undefined;
  onAdd: AddVariable;
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
  const path = variablePath(crumbs.join('.'), node);
  const computedStr = formatTraceValue(computed);
  const trimmed = input.trim();
  const addValue = trimmed ? parseAs(computed.kind, trimmed) : null;
  const addDisabled = trimmed !== '' && addValue === undefined;
  const splittedName = node.name.split('#');
  const prettyName =
    splittedName.length == 1 ? splittedName[0] : splittedName[1];
  return (
    <tr>
      <td
        style={{
          ...firstColStyle,
        }}
      />
      <td>
        {padding && (
          <span
            style={{ paddingRight: '0.2em' }}
            className="codicon codicon-blank"
          />
        )}
        {prettyName}
      </td>
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
      <td style={{ display: 'flex' }}>
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
          style={{ flexGrow: 1 }}
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
