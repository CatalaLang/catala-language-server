import { useState, useEffect, useRef, type ReactElement } from 'react';
import type { TraceEvent, VarComputation, SubScopeCall } from './traceTypes';
import type { RuntimeValue } from '../generated/catala_types';

type Props = {
  outputName: string;
  eventsJson: string | null;
  error?: string;
  projectRoot: string;
  expectedValue?: RuntimeValue;
  onClose: () => void;
  onOpenFile: (path: string, line: number) => void;
};

function compactRuntimeValue(rv: RuntimeValue): string {
  const raw = rv.value;
  switch (raw.kind) {
    case 'Money':
      return (raw.value / 100).toLocaleString('fr-FR');
    case 'Integer':
      return raw.value.toLocaleString('fr-FR');
    case 'Decimal':
      return raw.value.toLocaleString('fr-FR');
    case 'Bool':
      return raw.value ? 'vrai' : 'faux';
    case 'Date': {
      const d = raw.value;
      return `${d.year}-${String(d.month).padStart(2, '0')}-${String(d.day).padStart(2, '0')}`;
    }
    case 'Duration': {
      const d = raw.value;
      const parts: string[] = [];
      if (d.years) parts.push(`${d.years}a`);
      if (d.months) parts.push(`${d.months}m`);
      if (d.days) parts.push(`${d.days}j`);
      return parts.length ? parts.join(' ') : '0j';
    }
    case 'Enum':
      return raw.value[1][0];
    case 'Struct': {
      const fields = Array.from(raw.value[1].entries());
      if (fields.length === 1) return compactRuntimeValue(fields[0][1]);
      return raw.value[0].struct_name;
    }
    case 'Array':
      return `[${raw.value.length}]`;
    case 'Unset':
      return '⊘';
    case 'Empty':
      return '∅';
  }
}

function resolveSourcePath(projectRoot: string, filename: string): string {
  // Clerk/ninja puts artifacts at _build/<same-relative-path>, so strip just _build/
  const match = filename.match(/^_build\/(.+)$/);
  const relative = match ? match[1] : filename;
  return projectRoot ? `${projectRoot}/${relative}` : relative;
}

function containsTarget(events: TraceEvent[], targetName: string): boolean {
  return events.some((ev) => {
    if (ev.kind === 'VarComputation') {
      return (ev.name[ev.name.length - 1] ?? '') === targetName;
    }
    return (
      containsTarget(ev.inputs as TraceEvent[], targetName) ||
      containsTarget(ev.body, targetName)
    );
  });
}

function ValueNode({
  value,
  depth = 0,
}: {
  value: unknown;
  depth?: number;
}): ReactElement {
  const [expanded, setExpanded] = useState(depth < 1);

  if (value === null || value === undefined)
    return <span className="explain-value-literal">null</span>;
  if (typeof value === 'boolean')
    return (
      <span className="explain-value-literal">{value ? 'true' : 'false'}</span>
    );
  if (typeof value === 'number')
    return <span className="explain-value-number">{value}</span>;
  if (typeof value === 'string')
    return <span className="explain-value-string">{value}</span>;

  if (typeof value === 'object') {
    const v = value as Record<string, unknown>;

    if (v.kind === 'struct') {
      const fields = Object.entries(v.fields as Record<string, unknown>);
      const typeName = String(v.name).includes('.')
        ? String(v.name).split('.').pop()!
        : String(v.name);
      return (
        <span className="explain-value-struct">
          <button
            className="explain-value-expand"
            onClick={(e) => {
              e.stopPropagation();
              setExpanded((x) => !x);
            }}
          >
            <span
              className={`codicon ${expanded ? 'codicon-chevron-down' : 'codicon-chevron-right'}`}
            />
            <span className="explain-value-type">{typeName}</span>
            {!expanded && (
              <span className="explain-value-hint">({fields.length} champs)</span>
            )}
          </button>
          {expanded && (
            <ul className="explain-value-fields">
              {fields.map(([k, fv]) => (
                <li key={k} className="explain-value-field">
                  <span className="explain-value-field-name">{k}</span>
                  <span className="explain-value-sep">: </span>
                  <ValueNode value={fv} depth={depth + 1} />
                </li>
              ))}
            </ul>
          )}
        </span>
      );
    }

    if (v.kind === 'array') {
      const items = v.value as unknown[];
      return (
        <span className="explain-value-array">
          <button
            className="explain-value-expand"
            onClick={(e) => {
              e.stopPropagation();
              setExpanded((x) => !x);
            }}
          >
            <span
              className={`codicon ${expanded ? 'codicon-chevron-down' : 'codicon-chevron-right'}`}
            />
            <span className="explain-value-hint">
              [{items.length} élément{items.length !== 1 ? 's' : ''}]
            </span>
          </button>
          {expanded && (
            <ul className="explain-value-fields">
              {items.map((item, i) => (
                <li key={i} className="explain-value-field">
                  <span className="explain-value-field-name">{i}</span>
                  <span className="explain-value-sep">: </span>
                  <ValueNode value={item} depth={depth + 1} />
                </li>
              ))}
            </ul>
          )}
        </span>
      );
    }

    if (v.kind === 'enum') {
      const constructor = String(v.constructor);
      return (
        <span className="explain-value-enum">
          {constructor}
          {v.value !== undefined && (
            <>
              {' '}
              <ValueNode value={v.value} depth={depth + 1} />
            </>
          )}
        </span>
      );
    }
  }

  return (
    <span className="explain-value-literal">{JSON.stringify(value, null, 0)}</span>
  );
}

function SourceBlock({
  pos,
  projectRoot,
  onOpenFile,
}: {
  pos: import('./traceTypes').SourcePos;
  projectRoot: string;
  onOpenFile: (path: string, line: number) => void;
}): ReactElement {
  const [expanded, setExpanded] = useState(false);
  const hasContext = !!pos.source_context;
  const text = expanded && pos.source_context ? pos.source_context : pos.source_text;

  return (
    <div className="explain-event-source">
      {pos.law_headings.length > 0 && (
        <div className="explain-event-meta">{pos.law_headings.join(' › ')}</div>
      )}
      {text && (
        <pre
          className={`explain-event-source-text${expanded ? ' expanded' : ''}`}
          onClick={hasContext ? () => setExpanded((v) => !v) : undefined}
          style={hasContext ? { cursor: 'pointer' } : undefined}
          title={hasContext ? (expanded ? 'Cliquer pour réduire' : 'Cliquer pour voir le contexte') : undefined}
        >{text}</pre>
      )}
      <button
        className="explain-event-source-link"
        onClick={() => {
          const abs = resolveSourcePath(projectRoot, pos.filename);
          onOpenFile(abs, pos.start_line);
        }}
      >
        {pos.filename.replace(/^_build\//, '')}:{pos.start_line}
      </button>
    </div>
  );
}

function VarComputationNode({
  event,
  highlight,
  projectRoot,
  onOpenFile,
  expectedValue,
}: {
  event: VarComputation;
  highlight: boolean;
  projectRoot: string;
  onOpenFile: (path: string, line: number) => void;
  expectedValue?: RuntimeValue;
}): ReactElement {
  const rawName = event.name[event.name.length - 1] ?? '';
  const varName =
    event.name[1] === 'direct' && event.name.length === 3
      ? `${event.name[0]} (${rawName})`
      : rawName || event.name.join('.');
  const isInput = event.io.io_input === 'OnlyInput';

  return (
    <div
      className={`explain-event-toggle${highlight ? ' highlight' : ''}`}
      role="listitem"
    >
      <span className="explain-event-icon codicon codicon-symbol-variable" />
      <span className="explain-event-content">
        <span className="explain-event-name">{varName}</span>
        {isInput && (
          <span className="explain-event-tag input">votre saisie</span>
        )}
        {event.io.io_output && (
          <span className="explain-event-tag">sortie</span>
        )}
        <div className="explain-event-value">
          {expectedValue && (
            <span className="explain-expected-value">
              {compactRuntimeValue(expectedValue)}
            </span>
          )}
          <ValueNode value={event.value} depth={0} />
        </div>
        {event.pos && (
          <SourceBlock pos={event.pos} projectRoot={projectRoot} onOpenFile={onOpenFile} />
        )}
      </span>
    </div>
  );
}

function compactValue(value: unknown): string {
  if (value === null || value === undefined) return 'null';
  if (typeof value === 'boolean') return value ? 'vrai' : 'faux';
  if (typeof value === 'number') return value.toLocaleString('fr-FR');
  if (typeof value === 'string') return value;
  if (typeof value === 'object') {
    const v = value as Record<string, unknown>;
    if (v.kind === 'enum') return String(v.constructor);
    if (v.kind === 'struct') {
      const fields = Object.entries(v.fields as Record<string, unknown>);
      if (fields.length === 1) return compactValue(fields[0][1]);
      return String(v.name).split('.').pop() ?? String(v.name);
    }
    if (v.kind === 'array') return `[${(v.value as unknown[]).length}]`;
  }
  return JSON.stringify(value, null, 0);
}

function SubScopeCallNode({
  event,
  targetName,
  autoExpand,
  highlightRef,
  callIndex,
  callCount,
  projectRoot,
  onOpenFile,
  expectedValue,
}: {
  event: SubScopeCall;
  targetName: string;
  autoExpand: boolean;
  highlightRef?: React.RefObject<HTMLDivElement>;
  callIndex?: number;
  callCount?: number;
  projectRoot: string;
  onOpenFile: (path: string, line: number) => void;
  expectedValue?: RuntimeValue;
}): ReactElement {
  const [expanded, setExpanded] = useState(autoExpand);
  // For ["Scope", "direct"] use first element; otherwise last element
  const scopeName =
    event.name[1] === 'direct'
      ? event.name[0]
      : (event.name[event.name.length - 1] ?? event.name.join('.'));

  const outputVars = event.body.filter(
    (ev): ev is VarComputation => ev.kind === 'VarComputation' && ev.io.io_output,
  );
  const outputSummary =
    !expanded && outputVars.length > 0
      ? outputVars.map((v) => compactValue(v.value)).join(', ')
      : null;

  return (
    <div className="explain-event-item">
      <button
        className="explain-event-toggle"
        onClick={() => setExpanded((v) => !v)}
        aria-expanded={expanded}
      >
        <span
          className={`explain-event-icon codicon ${expanded ? 'codicon-chevron-down' : 'codicon-chevron-right'}`}
        />
        <span className="explain-event-content">
          <span className="explain-event-name">{scopeName}</span>
          {callCount !== undefined && callCount > 1 && callIndex !== undefined && (
            <span className="explain-event-tag">{callIndex + 1}/{callCount}</span>
          )}
          <span className="explain-event-tag">champ d'application</span>
          {outputSummary !== null && (
            <span className="explain-event-output-summary">→ {outputSummary}</span>
          )}
        </span>
      </button>
      {expanded && (
        <div className="explain-event-children">
          {event.inputs.length > 0 && (
            <>
              <div className="explain-event-meta">Entrées</div>
              <ul className="explain-event-list">
                {event.inputs.map((inp, i) => (
                  <li key={i} className="explain-event-item">
                    <VarComputationNode event={inp} highlight={false} projectRoot={projectRoot} onOpenFile={onOpenFile} />
                  </li>
                ))}
              </ul>
            </>
          )}
          {event.body.length > 0 && (
            <>
              <div className="explain-event-meta">Calculs</div>
              <EventList
                events={event.body}
                targetName={targetName}
                highlightRef={highlightRef}
                projectRoot={projectRoot}
                onOpenFile={onOpenFile}
                expectedValue={expectedValue}
              />
            </>
          )}
        </div>
      )}
    </div>
  );
}

function scopeKey(ev: SubScopeCall): string {
  return ev.name.join('\x00');
}

function EventList({
  events,
  targetName,
  highlightRef,
  projectRoot,
  onOpenFile,
  expectedValue,
}: {
  events: TraceEvent[];
  targetName: string;
  highlightRef?: React.RefObject<HTMLDivElement>;
  projectRoot: string;
  onOpenFile: (path: string, line: number) => void;
  expectedValue?: RuntimeValue;
}): ReactElement {
  // Count how many times each sub-scope name appears at this level
  const callCounts = new Map<string, number>();
  for (const ev of events) {
    if (ev.kind === 'SubScopeCall') {
      const key = scopeKey(ev);
      callCounts.set(key, (callCounts.get(key) ?? 0) + 1);
    }
  }
  const callIndices = new Map<string, number>();

  return (
    <ul className="explain-event-list">
      {events.map((ev, i) => {
        if (ev.kind === 'VarComputation') {
          const varName = ev.name[ev.name.length - 1] ?? '';
          const highlight = varName === targetName;
          return (
            <li key={i} className="explain-event-item">
              <div ref={highlight ? highlightRef : undefined}>
                <VarComputationNode
                  event={ev}
                  highlight={highlight}
                  projectRoot={projectRoot}
                  onOpenFile={onOpenFile}
                  expectedValue={highlight ? expectedValue : undefined}
                />
              </div>
            </li>
          );
        } else {
          const key = scopeKey(ev);
          const callIndex = callIndices.get(key) ?? 0;
          callIndices.set(key, callIndex + 1);
          const callCount = callCounts.get(key) ?? 1;
          const autoExpand =
            containsTarget(ev.inputs as TraceEvent[], targetName) ||
            containsTarget(ev.body, targetName);
          return (
            <li key={i} className="explain-event-item">
              <SubScopeCallNode
                event={ev}
                targetName={targetName}
                autoExpand={autoExpand}
                highlightRef={highlightRef}
                callIndex={callIndex}
                callCount={callCount}
                projectRoot={projectRoot}
                onOpenFile={onOpenFile}
                expectedValue={expectedValue}
              />
            </li>
          );
        }
      })}
    </ul>
  );
}

export default function ExplainPanel({
  outputName,
  eventsJson,
  error,
  projectRoot,
  expectedValue,
  onClose,
  onOpenFile,
}: Props): ReactElement {
  let events: TraceEvent[] = [];
  let parseError: string | null = null;

  if (eventsJson !== null) {
    try {
      events = JSON.parse(eventsJson) as TraceEvent[];
    } catch (e) {
      parseError = `Failed to parse events: ${e instanceof Error ? e.message : String(e)}`;
    }
  }

  const highlightRef = useRef<HTMLDivElement>(null) as React.RefObject<HTMLDivElement>;

  useEffect(() => {
    if (eventsJson !== null && highlightRef.current) {
      highlightRef.current.scrollIntoView({ behavior: 'smooth', block: 'nearest' });
    }
  }, [eventsJson]);

  return (
    <div className="explain-panel" role="complementary" aria-label="Explain">
      <div className="explain-panel-header">
        <span className="explain-panel-title">Explication : {outputName}</span>
        <button
          className="explain-panel-close"
          onClick={onClose}
          aria-label="Close explain panel"
        >
          <span className="codicon codicon-close" />
        </button>
      </div>
      {error ? (
        <div className="explain-panel-error">
          <strong>Erreur :</strong>
          <pre className="explain-panel-error-detail">{error}</pre>
        </div>
      ) : eventsJson === null ? (
        <div className="explain-panel-loading">
          <span className="codicon codicon-loading codicon-modifier-spin" /> Calcul en cours…
        </div>
      ) : parseError ? (
        <div className="explain-panel-error">{parseError}</div>
      ) : (
        <EventList
          events={events}
          targetName={outputName}
          highlightRef={highlightRef}
          projectRoot={projectRoot}
          onOpenFile={onOpenFile}
          expectedValue={expectedValue}
        />
      )}
    </div>
  );
}
