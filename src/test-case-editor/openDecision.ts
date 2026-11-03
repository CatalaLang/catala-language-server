import type {
  CannotOpenCause,
  ParseResults,
  ReadFailure,
  Recovery,
  TestList,
} from '../generated/catala_types';
import { assertUnreachable } from '../shared/util';

/**
 * What opening a test shows: the editor, the repair view, or an error page.
 *
 * `read` fails whenever anything goes wrong, not only when the scope changed
 * underneath the test, and says where (`ReadFailure`). The repair view is for
 * drift, and for a repair under way.
 *
 * On disk, a test has its original and maybe a draft, `<test>.repair`, written
 * by a save or by VS Code's backup of an unsaved repair. Only Replace and
 * Discard delete it, the tester asking. Opening the test:
 * 0. the original reads: it opens normally, offering the draft if there is one
 *    (`CatalaTestCaseDocument.workingCopySaved`); content without a test, or
 *    output we cannot parse, is an error page. When it does not read:
 * 1. the test does not parse: error page;
 * 2. an error outside the test: the module does not build, error page;
 * otherwise rebuild decides:
 * 3. it refuses the file, or says nothing: error page;
 * 4. the module does not build, or the tool failed: error page, the draft kept;
 * 5. a draft exists: repair view. Fitting its target only means it is ready;
 * 6. the scope or module is gone, or a field no longer fits: repair view;
 * 7. every field fits: error page, `read` failed for a reason outside the test.
 * Rebuild's notes give 4 and 6; at most one of their four kinds is present.
 * The open-from-banner (`openWorkingCopy`) skips all this: the tester asked.
 * One test per case in `tests/editors/open-decision.spec.ts`: change a
 * decision there.
 */
export type ReadAnswer =
  | { kind: 'read'; tests: TestList }
  // `read`'s output could not be parsed: ours.
  | { kind: 'unreadable'; message: string }
  // Without `where`, a plugin that predates it, or a crash.
  | { kind: 'failed'; where?: ReadFailure; message: string };

export type RebuildAnswer =
  | { kind: 'recovered'; view: Recovery }
  | { kind: 'refused'; message: string }
  | { kind: 'silent' };

export function decideOpen({
  read,
  rebuild,
  hasDraft,
  blank,
  ranFrom,
}: {
  read: ReadAnswer;
  // Only run when the decision needs it: it compiles the module again.
  rebuild: () => RebuildAnswer;
  hasDraft: (view: Recovery) => boolean;
  blank: boolean;
  ranFrom?: string;
}): ParseResults {
  const cannotOpen = (
    cause: CannotOpenCause['kind'],
    message: string,
    ranFrom?: string
  ): ParseResults => ({
    kind: 'CannotOpen',
    value: { cause: { kind: cause }, message, ran_from: ranFrom },
  });
  switch (read.kind) {
    case 'read':
      return !blank && read.tests.length === 0
        ? cannotOpen('EmptyTestListMismatch', '')
        : { kind: 'Results', value: read.tests };
    case 'unreadable':
      return cannotOpen('ToolProblem', read.message);
    case 'failed':
      // Where it failed decides, or else the rebuild.
      break;
    default:
      assertUnreachable(read);
  }
  switch (read.where?.kind) {
    case 'TestSyntax':
      return cannotOpen('TestFileInvalid', read.message);
    case 'OutsideTest':
      return cannotOpen('ModuleWontBuild', read.message, ranFrom);
    default:
      // In the test, or unknown: only the rebuild tells drift from an error.
      break;
  }
  const rebuilt = rebuild();
  switch (rebuilt.kind) {
    // More specific than `read`'s first error.
    case 'refused':
      return cannotOpen('TestFileInvalid', rebuilt.message);
    case 'silent':
      return cannotOpen('TestFileInvalid', read.message, ranFrom);
    case 'recovered': {
      const decision = decideOnReadFailure(
        read.message,
        rebuilt.view,
        hasDraft(rebuilt.view)
      );
      return decision.kind === 'repair'
        ? { kind: 'BrokenTest', value: rebuilt.view }
        : cannotOpen(decision.cause.kind, decision.message, ranFrom);
    }
    default:
      return assertUnreachable(rebuilt);
  }
}

export type OpenDecision =
  | { kind: 'repair' }
  | { kind: 'cannotOpen'; cause: CannotOpenCause; message: string };

export function decideOnReadFailure(
  readError: string,
  view: Recovery,
  hasDraft: boolean
): OpenDecision {
  for (const note of view.notes) {
    switch (note.kind) {
      // Renamed or deleted: the tester picks the new target.
      case 'ScopeNotFound':
      case 'ModuleNotFound':
        return { kind: 'repair' };
      // The module itself, or the folder catala runs from (test_32).
      case 'ModuleWontCompile':
        return {
          kind: 'cannotOpen',
          cause: { kind: 'ModuleWontBuild' },
          message: note.value.error,
        };
      // The module builds and has the scope: our bug.
      case 'Other':
        return {
          kind: 'cannotOpen',
          cause: { kind: 'ToolProblem' },
          message: note.value.error,
        };
      // About the .repair file, not the test.
      case 'WorkingCopyUnreadable':
      case 'WorkingCopyRecovered':
        break;
      default:
        assertUnreachable(note);
    }
  }
  if (hasDraft) return { kind: 'repair' };
  // Nothing to repair: only `read`'s message says what failed.
  return everythingFits(view)
    ? { kind: 'cannotOpen', cause: { kind: 'ToolProblem' }, message: readError }
    : { kind: 'repair' };
}

function everythingFits(view: Recovery): boolean {
  return (
    view.tests.length > 0 &&
    view.tests.every(
      (t) =>
        t.rebuilt !== undefined &&
        t.outcomes.every((o) => o.outcome.kind === 'Fits')
    )
  );
}
