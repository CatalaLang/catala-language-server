import React, { useEffect, useRef, useState } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import type {
  BrokenNote,
  Recovery,
  CarryIo,
  CarryOutcome,
  ScopeCandidate,
  CarryRecord,
  TestRunResults,
  Test,
  TestIo,
  TestList,
} from '../generated/catala_types';
import ValueEditor from '../editors/ValueEditors';
import { getTypeDisplayName } from '../editors/typeNameUtils';
import {
  hasUnsetInTest,
  scrollToFirstInvalidOrUnset,
} from '../editors/unsetValidation';
import { confirm } from '../messaging/confirm';
import TestInputsEditor from './TestInputsEditor';
import TestOutputsEditor from './TestOutputsEditor';
import RunControl from './RunControl';
import type { TestRunStatus } from './TestFileEditor';

/**
 * Shown when a test no longer fits the scope it targets: the old values on the
 * left, visible and untouchable; a rebuild beside them on the right. The
 * rebuilt pane is the ordinary editor's own input/output components, so
 * context variables and unasserted outputs behave exactly as they do there.
 */

type Props = {
  view: Recovery;
  /** Report the rebuild upward, so saving writes it to the workspace file. */
  onRebuildChange?: (tests: TestList) => void;
  onRun?: (testingScope: string) => void;
  /** The tester chose which scope to rebuild against. */
  onRetarget?: (scope: string) => void;
  /** Write the rebuild over the original. Given the rebuild, so the caller
   *  can check for unset fields. */
  onReplace?: (rebuilt: TestList) => void;
  /** Throw the rebuild away and start over from the original. */
  onDiscard?: () => void;
  runStates?: Record<
    string,
    { status: TestRunStatus; results?: TestRunResults }
  >;
};

/** The per-field outcomes for one test and one side, keyed by field name.
 *  Sides are separate maps: a `context output` variable is both an input and
 *  an output, and one side's outcome must not mask the other's. */
function marksFor(
  carried: CarryRecord[],
  testingScope: string,
  io: CarryIo['kind']
): Map<string, CarryOutcome> {
  return new Map(
    carried
      .filter((c) => c.testing_scope === testingScope && c.io.kind === io)
      .map((c) => [c.field, c.outcome])
  );
}

/** What became of one field. Silent when there was nothing to carry. */
function CarryMark({
  outcome,
}: {
  outcome: CarryOutcome;
}): React.JSX.Element | null {
  const intl = useIntl();
  if (outcome.kind === 'WasUnset') return null;
  if (outcome.kind === 'Fits') return null;
  const carried = outcome.kind === 'Wrap' || outcome.kind === 'Unwrap';
  const id = {
    Wrap: 'broken.markWrapped',
    Unwrap: 'broken.markUnwrapped',
    WasAbsentNowRequired: 'broken.markWasAbsentNowRequired',
    TypeChanged: 'broken.markTypeChanged',
  }[outcome.kind as 'Wrap' | 'Unwrap' | 'WasAbsentNowRequired' | 'TypeChanged'];
  const change =
    outcome.kind === 'TypeChanged'
      ? `${getTypeDisplayName(outcome.value[0], intl)} → ${getTypeDisplayName(
          outcome.value[1],
          intl
        )}`
      : '';
  return (
    <span className={`carry-mark ${carried ? 'carry-done' : 'carry-open'}`}>
      <FormattedMessage id={id} values={{ change }} />
    </span>
  );
}

/** The authored pane's fields: read-only, and only what the test actually
 *  set -- an empty box here would read as data gone missing. The rebuilt pane
 *  uses the ordinary editor's own components instead. */
function Fields({
  record,
}: {
  record: Map<string, TestIo>;
}): React.JSX.Element {
  const entries = [...record.entries()].filter(
    ([, io]) => io.value !== undefined
  );
  return (
    <>
      {entries.map(([name, io]) => (
        <div className="broken-field" key={name}>
          <span className="item-label">{name}</span>
          <ValueEditor
            testIO={io}
            editable={false}
            onValueChange={(): void => {}}
            currentPath={[{ kind: 'StructField', value: name }]}
            diffs={[]}
          />
        </div>
      ))}
    </>
  );
}

/** Why the rebuild could not proceed, in the viewer's language. */
function Note({ note }: { note: BrokenNote }): React.JSX.Element {
  switch (note.kind) {
    case 'ModuleNotFound':
      return (
        <FormattedMessage
          id="broken.noteModuleNotFound"
          values={{ name: <code>{note.value.module_name}</code> }}
        />
      );
    case 'ScopeNotFound':
      return (
        <>
          <FormattedMessage
            id="broken.noteScopeNotFound"
            values={{
              scope: (
                <code>{`${note.value.module_name}.${note.value.scope_name}`}</code>
              ),
            }}
          />
        </>
      );
    case 'ModuleWontCompile':
    case 'Other':
      return (
        <>
          <FormattedMessage
            id={
              note.kind === 'ModuleWontCompile'
                ? 'broken.noteModuleWontCompile'
                : 'broken.noteOther'
            }
            values={{ name: <code>{note.value.name}</code> }}
          />
          {note.value.error !== '' && (
            <pre className="broken-note-error">{note.value.error}</pre>
          )}
        </>
      );
  }
}

const SPLIT_KEY = 'catala.brokenView.split';
const SPLIT_MIN = 20;
const SPLIT_MAX = 80;

function clampSplit(pct: number): number {
  return Math.min(SPLIT_MAX, Math.max(SPLIT_MIN, pct));
}

/* Where the tester last left the divider. A webview is reloaded on every edit
   session, and having to drag it back each time is what makes a split view
   annoying rather than useful. */
function storedSplit(): number {
  try {
    const raw = localStorage.getItem(SPLIT_KEY);
    if (raw === null) return 50;
    const pct = Number(raw);
    return Number.isFinite(pct) ? clampSplit(pct) : 50;
  } catch {
    return 50;
  }
}

function storeSplit(pct: number): void {
  try {
    localStorage.setItem(SPLIT_KEY, String(pct));
  } catch {
    /* the divider still moves, it just will not be remembered */
  }
}

/**
 * The divider between the panes. Pointer capture so the drag survives leaving
 * the element; a real separator so it moves from the keyboard.
 */
function SplitHandle({
  onSplit,
  split,
}: {
  onSplit: (pct: number) => void;
  split: number;
}): React.JSX.Element {
  const intl = useIntl();
  const fromEvent = (e: React.PointerEvent<HTMLDivElement>): void => {
    const panes = e.currentTarget.parentElement;
    if (panes === null) return;
    const box = panes.getBoundingClientRect();
    if (box.width === 0) return;
    onSplit(clampSplit(((e.clientX - box.left) / box.width) * 100));
  };
  return (
    <div
      className="broken-split"
      role="separator"
      aria-orientation="vertical"
      aria-valuenow={Math.round(split)}
      aria-valuemin={SPLIT_MIN}
      aria-valuemax={SPLIT_MAX}
      aria-label={intl.formatMessage({ id: 'broken.resize' })}
      tabIndex={0}
      onPointerDown={(e): void => {
        e.currentTarget.setPointerCapture(e.pointerId);
        e.preventDefault();
      }}
      onPointerMove={(e): void => {
        if (e.currentTarget.hasPointerCapture(e.pointerId)) fromEvent(e);
      }}
      onPointerUp={(e): void => {
        e.currentTarget.releasePointerCapture(e.pointerId);
      }}
      onKeyDown={(e): void => {
        const step = e.shiftKey ? 10 : 2;
        if (e.key === 'ArrowLeft') onSplit(clampSplit(split - step));
        else if (e.key === 'ArrowRight') onSplit(clampSplit(split + step));
        else return;
        e.preventDefault();
      }}
    >
      <span className="codicon codicon-arrow-right" aria-hidden="true"></span>
    </div>
  );
}

/**
 * The scopes a test could be rebuilt against, when the one it names is gone.
 * Best guess first, but the tester picks.
 */
function ScopePicker({
  candidates,
  qualified,
  onRetarget,
}: {
  candidates: ScopeCandidate[];
  /** Candidates come from other modules: say which. */
  qualified: boolean;
  onRetarget?: (scope: string) => void;
}): React.JSX.Element {
  const intl = useIntl();
  const key = (c: ScopeCandidate): string => `${c.module_name}.${c.name}`;
  const label = (c: ScopeCandidate): string => (qualified ? key(c) : c.name);
  // Never preselected: a rename is a guess, and the tester says which.
  const [chosen, setChosen] = useState<string>('');
  const choice = candidates.find((c) => key(c) === chosen);
  return (
    <div className="broken-candidates">
      <FormattedMessage
        id={
          qualified
            ? 'broken.noteModuleNotFoundPick'
            : 'broken.noteScopeNotFoundPick'
        }
      />
      {candidates.length > 1 && (
        <span className="broken-candidates-hint">
          {' '}
          <FormattedMessage id="broken.candidatesRanked" />
        </span>
      )}
      <div className="broken-candidate-row">
        <select
          className="broken-candidate-select"
          value={chosen}
          onChange={(e): void => setChosen(e.target.value)}
          aria-label={intl.formatMessage({ id: 'broken.candidatePlaceholder' })}
        >
          <option value="" disabled>
            {intl.formatMessage({ id: 'broken.candidatePlaceholder' })}
          </option>
          {candidates.map((c) => (
            <option key={key(c)} value={key(c)}>
              {`${label(c)} (${c.shared}/${c.out_of})`}
            </option>
          ))}
        </select>
        <button
          className="broken-candidate"
          onClick={(): void => onRetarget?.(chosen)}
          disabled={onRetarget === undefined || choice === undefined}
        >
          <span className="codicon codicon-arrow-right"></span>{' '}
          <FormattedMessage
            id="broken.candidateRebuild"
            values={{
              scope: <code>{choice === undefined ? '…' : label(choice)}</code>,
            }}
          />
        </button>
      </div>
      {choice !== undefined && (
        <div className="broken-candidate-shared">
          <FormattedMessage
            id="broken.candidateShared"
            values={{ shared: choice.shared, of: choice.out_of }}
          />
        </div>
      )}
    </div>
  );
}

function TestPanes({
  authored,
  rebuilt,
  onChange,
  marksIn,
  marksOut,
  runState,
  onRun,
  split,
  onSplit,
  picker,
}: {
  authored: Test | undefined;
  rebuilt: Test | undefined;
  onChange: (next: Test) => void;
  marksIn: Map<string, CarryOutcome>;
  marksOut: Map<string, CarryOutcome>;
  runState?: { status: TestRunStatus; results?: TestRunResults };
  onRun?: () => void;
  split: number;
  onSplit: (pct: number) => void;
  picker?: React.JSX.Element;
}): React.JSX.Element {
  const meta = authored ?? rebuilt;
  const runDiffs =
    runState?.results?.kind === 'Ok' ? runState.results.value.diffs : [];
  const scope = meta?.tested_scope;
  const rebuiltPaneRef = useRef<HTMLDivElement>(null);
  const markFrom =
    (marks: Map<string, CarryOutcome>) =>
    (name: string): React.JSX.Element | null => {
      const outcome = marks.get(name);
      return outcome === undefined ? null : <CarryMark outcome={outcome} />;
    };
  // Same guard as the ordinary editor: an unset value makes the run fail with
  // an interpreter error, so ask first.
  const runWithUnsetCheck = async (): Promise<void> => {
    if (rebuilt !== undefined && hasUnsetInTest(rebuilt)) {
      scrollToFirstInvalidOrUnset(rebuiltPaneRef.current ?? document);
      if (!(await confirm('RunTestWithUnsetValues'))) return;
    }
    onRun?.();
  };
  return (
    <section className="broken-test">
      <h3 className="broken-test-title">
        {meta !== undefined && meta.title !== ''
          ? meta.title
          : (meta?.testing_scope ?? '')}
      </h3>
      <div className="broken-test-meta">
        {scope !== undefined && (
          <span className="broken-test-scope">
            {scope.module_name}.{scope.name}
          </span>
        )}
        {meta !== undefined && meta.description !== '' && (
          <span className="broken-test-description">{meta.description}</span>
        )}
      </div>
      <div
        className="broken-panes"
        style={{ ['--broken-split']: `${split}%` } as React.CSSProperties}
      >
        <div className="broken-pane broken-pane-authored">
          <h4>
            <span className="codicon codicon-lock"></span>{' '}
            <FormattedMessage id="broken.asAuthored" />
          </h4>
          {authored === undefined ? (
            <p className="broken-empty">
              <FormattedMessage id="broken.nothingRecovered" />
            </p>
          ) : (
            <>
              <Fields record={authored.test_inputs} />
              {authored.test_outputs.size > 0 && (
                <>
                  <h5 className="broken-subhead">
                    <FormattedMessage id="broken.expected" />
                  </h5>
                  <Fields record={authored.test_outputs} />
                </>
              )}
            </>
          )}
        </div>
        <SplitHandle split={split} onSplit={onSplit} />
        <div className="broken-pane broken-pane-rebuilt" ref={rebuiltPaneRef}>
          <h4>
            <span className="codicon codicon-edit"></span>{' '}
            <FormattedMessage id="broken.rebuild" />
            <span className="broken-draft-tag">
              <FormattedMessage id="broken.draftTag" />
            </span>
          </h4>
          {rebuilt === undefined ? (
            <>
              <p className="broken-empty">
                <FormattedMessage id="broken.noSignature" />
              </p>
              {picker}
            </>
          ) : (
            <>
              {/* The ordinary editor's inputs component, so context variables
                  keep their badge and "computed default" placeholder instead
                  of showing as an anonymous empty field. */}
              <TestInputsEditor
                test_inputs={rebuilt.test_inputs}
                tested_scope={rebuilt.tested_scope}
                onTestInputsChange={(inputs): void =>
                  onChange({ ...rebuilt, test_inputs: inputs })
                }
                labelExtra={markFrom(marksIn)}
              />
              {rebuilt.tested_scope.outputs.size > 0 && (
                <>
                  <h5 className="broken-subhead">
                    <FormattedMessage id="broken.expected" />
                  </h5>
                  {/* The ordinary editor's outputs component: an output the
                      test does not assert offers to add the assertion, rather
                      than showing a blank value that reads as an empty one. */}
                  <TestOutputsEditor
                    test={rebuilt}
                    onTestChange={onChange}
                    diffs={runDiffs}
                    labelExtra={markFrom(marksOut)}
                  />
                </>
              )}
            </>
          )}
          {rebuilt !== undefined && onRun !== undefined && (
            <div className="broken-run">
              <RunControl
                status={runState?.status}
                results={runState?.results}
                onRun={runWithUnsetCheck}
                labelId="broken.runWorkingCopy"
              />
            </div>
          )}
        </div>
      </div>
    </section>
  );
}

export default function BrokenTestView({
  view,
  onRebuildChange,
  onRun,
  onRetarget,
  onReplace,
  onDiscard,
  runStates,
}: Props): React.JSX.Element {
  const [rebuilt, setRebuilt] = useState<TestList>(view.rebuilt);
  // A new view (retarget, undo, revert) is authoritative; it is never the
  // echo of an edit made here.
  useEffect(() => {
    setRebuilt(view.rebuilt);
  }, [view.rebuilt]);
  // A scope gone from its module, or a module gone from the project: either
  // way the tester picks where the test goes now.
  const pickable = view.notes.find(
    (n) => n.kind === 'ScopeNotFound' || n.kind === 'ModuleNotFound'
  );
  const picker =
    (pickable?.kind === 'ScopeNotFound' ||
      pickable?.kind === 'ModuleNotFound') &&
    pickable.value.candidates.length > 0 ? (
      <ScopePicker
        candidates={pickable.value.candidates}
        qualified={pickable.kind === 'ModuleNotFound'}
        onRetarget={onRetarget}
      />
    ) : undefined;
  // One divider for the whole view.
  const [split, setSplit] = useState<number>(storedSplit);
  const update = (next: TestList): void => {
    setRebuilt(next);
    onRebuildChange?.(next);
  };
  const moveSplit = (pct: number): void => {
    setSplit(pct);
    storeSplit(pct);
  };

  // Nothing to rebuild against: the notes are the message.
  const blocked = view.rebuilt.length === 0;
  const notes = view.notes.map((n, i) => (
    <li key={n.kind + String(i)}>
      <Note note={n} />
    </li>
  ));

  return (
    <div className="broken-view">
      <div className="broken-banner">
        <span className="codicon codicon-warning"></span>
        <div>
          <strong>
            <FormattedMessage
              id={blocked ? 'broken.blockedTitle' : 'broken.title'}
            />
          </strong>
          <p>
            <FormattedMessage
              id={blocked ? 'broken.blockedExplanation' : 'broken.explanation'}
            />
          </p>
          {blocked ? (
            notes.length > 0 && <ul className="broken-notes">{notes}</ul>
          ) : (
            <>
              <p className="broken-arrangement">
                <FormattedMessage
                  id="broken.arrangement"
                  values={{ file: <code>{view.working_copy}</code> }}
                />
              </p>
              {/* The two ways out, next to the sentence that promises them. */}
              <div className="broken-exit">
                <button
                  className="broken-candidate"
                  onClick={(): void => onReplace?.(rebuilt)}
                  disabled={onReplace === undefined}
                >
                  <span className="codicon codicon-check"></span>{' '}
                  <FormattedMessage id="broken.replace" />
                </button>
                <button
                  className="broken-candidate"
                  onClick={(): void => onDiscard?.()}
                  disabled={onDiscard === undefined}
                >
                  <span className="codicon codicon-discard"></span>{' '}
                  <FormattedMessage id="broken.discard" />
                </button>
              </div>
            </>
          )}
        </div>
      </div>

      {!blocked && notes.length > 0 && (
        <ul className="broken-notes">{notes}</ul>
      )}

      {view.original.map((authored, i) => (
        <TestPanes
          key={authored.testing_scope + String(i)}
          authored={authored}
          rebuilt={rebuilt[i]}
          marksIn={marksFor(view.carry_outcomes, authored.testing_scope, 'In')}
          marksOut={marksFor(
            view.carry_outcomes,
            authored.testing_scope,
            'Out'
          )}
          split={split}
          onSplit={moveSplit}
          picker={picker}
          runState={runStates?.[authored.testing_scope]}
          onRun={
            onRun === undefined
              ? undefined
              : (): void => onRun(authored.testing_scope)
          }
          onChange={(next): void =>
            update(rebuilt.map((t, j) => (j === i ? next : t)))
          }
        />
      ))}
    </div>
  );
}
