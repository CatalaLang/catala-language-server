import { type ReactElement, useState } from 'react';
import type { IntlShape} from 'react-intl';
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
  traceValueToRuntime,
} from '../trace-editor/traceUtils';
import type {
  RuntimeValue,
  RuntimeValueRaw,
  Test,
  ValueDef,
  VariableFailure,
} from '../generated/catala_types';
import {
  BoolEditor,
  DateEditor,
  DurationEditor,
  IntEditor,
  MoneyEditor,
  RatEditor,
} from '../editors/ValueEditors';

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

function formtatRuntimeValue(
  rv: RuntimeValue | undefined,
  intl: IntlShape
): string | undefined {
  const inputTraceValue = rv ? traceValueFromRuntime(rv) : undefined;
  const inputStr = inputTraceValue
    ? formatTraceValue(inputTraceValue, intl)
    : '';
  return inputStr;
}

function TraceValueEditor({
  input,
  setInput,
  kind,
  intl,
}: {
  input: RuntimeValue | undefined;
  setInput: React.Dispatch<React.SetStateAction<RuntimeValue | undefined>>;
  kind: string;
  intl: IntlShape;
}): ReactElement {
  let rv: ValueDef | undefined = input ? { value: input } : undefined;
  switch (kind) {
    case 'money': {
      return (
        <MoneyEditor valueDef={rv} onValueChange={setInput} editable={true} />
      );
    }
    case 'bool': {
      return (
        <BoolEditor valueDef={rv} onValueChange={setInput} editable={true} />
      );
    }
    case 'integer': {
      return (
        <IntEditor valueDef={rv} onValueChange={setInput} editable={true} />
      );
    }
    case 'decimal':
      return (
        <RatEditor valueDef={rv} onValueChange={setInput} editable={true} />
      );
    case 'date':
      return (
        <DateEditor valueDef={rv} onValueChange={setInput} editable={true} />
      );
    case 'duration':
      return (
        <DurationEditor
          valueDef={rv}
          onValueChange={setInput}
          editable={true}
        />
      );
    case 'absent':
    case 'enum': {
      const inputStr = formtatRuntimeValue(input, intl);
      return (
        <VscodeTextfield
          value={inputStr}
          onInput={(e) => {
            let valueField =
              (e.target as { value?: string } | null)?.value ?? '';
            let traceValue: RuntimeValueRaw | undefined = traceValueToRuntime({
              kind: 'enum',
              ctor: valueField,
            });
            let jsonValue: RuntimeValue | undefined = traceValue
              ? { value: traceValue, attrs: [] }
              : undefined;
            setInput(jsonValue);
          }}
          style={{ flex: 1, width: '100%' }}
        />
      );
    }
    default:
      return <span />;
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

  const [input, setInput] = useState<RuntimeValue | undefined>(undefined);
  const expectedStr =
    expected !== null ? formatTraceValue(expected, intl) : '--';
  const computedStr =
    computed !== undefined ? formatTraceValue(computed, intl) : undefined;

  // A reported failure wins over the local comparison: it was computed by the
  // compiler against the trace, with the runtime's own formatting rules, so it
  // is right where `traceValueEqual` on re-parsed JSON values may not be.
  const mismatch =
    computed !== undefined &&
    expected !== null &&
    !traceValueEqual(expected, computed);

  let comp = computed ? traceValueToRuntime(computed) : undefined;
  let compValu: RuntimeValue | undefined = comp
    ? { value: comp, attrs: [] }
    : undefined;

  function apply(): void {
    if (input !== undefined) {
      const tvInput = traceValueFromRuntime(input);
      onSet(name, tvInput ?? null);
      setInput(undefined);
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
        <TraceValueEditor
          kind={computed ? computed.kind : 'absent'}
          input={input}
          setInput={setInput}
          intl={intl}
        />
        <VscodeButton
          secondary
          icon="arrow-left"
          disabled={computed === undefined}
          title={intl.formatMessage(
            { id: 'testEditor.fillComputedVariable' },
            { value: computedStr ?? '' }
          )}
          onClick={() => {
            setInput(compValu);
          }}
        />
        <VscodeButton
          secondary
          icon="check"
          title={intl.formatMessage(
            { id: 'testEditor.setVariable' },
            { value: 'applyStr' }
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
      <table className="variable-catalog-table">
        {/* The column widths are pinned by this colgroup (see
            `.variable-catalog-table`, which is `table-layout: fixed`): rows
            appear and disappear as the tree is expanded, so widths derived
            from the content would make the whole table jump around while the
            user is reading or typing in it. */}
        <colgroup>
          <col className="variable-catalog-col-chevron" />
          <col />
          <col className="variable-catalog-col-value" />
          <col />
          <col className="variable-catalog-col-actions" />
        </colgroup>
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
        <td style={cellStyle}>
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
        <td />
        {/* The cell must stay a real table cell for `colSpan` to apply, so the
            flex layout lives on an inner element rather than on the `td`
            itself. Same shape as `StepRow` above, which spans correctly. */}
        <td colSpan={3}>
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
  const computed = node.value;
  const runtimeComputed = computed ? traceValueToRuntime(computed) : undefined;
  const computedRuntime: RuntimeValue | undefined = runtimeComputed
    ? { value: runtimeComputed, attrs: [] }
    : undefined;
  const [input, setInput] = useState<RuntimeValue | undefined>(computedRuntime);
  if (
    computed === undefined ||
    computed.kind === 'struct' ||
    computed.kind === 'array' ||
    (computed.kind === 'enum' && computed.value !== undefined)
  ) {
    return null;
  }
  const path = variablePath(crumbs.join('.'), node);
  const computedStr = formatTraceValue(computed, intl);
  const addValue = input !== undefined ? traceValueFromRuntime(input) : null;
  const addDisabled = input !== undefined && addValue === undefined;
  const splittedName = node.name.split('#');
  const prettyName =
    splittedName.length == 1 ? splittedName[0] : splittedName[1];
  return (
    <tr>
      <td />
      <td className="variable-catalog-ellipsis" title={prettyName}>
        {padding && (
          <span
            style={{ paddingRight: '0.2em' }}
            className="codicon codicon-blank"
          />
        )}
        {prettyName}
      </td>
      <td>
        <TraceValueEditor
          kind={computed.kind}
          input={input}
          setInput={setInput}
          intl={intl}
        />
      </td>
      <td/>
      {/* `display: flex` on the `td` itself would take it out of the table's
          column model, so the flex row lives on an inner element. */}
      <td>
        <span className="variable-catalog-actions">
          <VscodeButton
            secondary
            icon="arrow-left"
            disabled={computedStr === undefined}
            title={intl.formatMessage(
              { id: 'testEditor.fillComputedVariable' },
              { value: computedStr ?? '' }
            )}
            onClick={() => onAdd(path, computed)}
          />
          <VscodeButton
            secondary
            icon="add"
            disabled={addDisabled}
            title={intl.formatMessage({ id: 'testEditor.addVariable' })}
            onClick={() => {
              if (addValue !== undefined) {
                onAdd(path, addValue);
                setInput(undefined);
              }
            }}
          />
        </span>
      </td>
    </tr>
  );
}
