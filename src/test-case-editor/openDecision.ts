import type { CannotOpenCause, Recovery } from '../generated/catala_types';
import { assertUnreachable } from '../shared/util';

/**
 * What a test shows when `read` failed: the repair view, or an error page.
 *
 * `read` fails whenever anything goes wrong, not only when the scope changed
 * underneath the test: the module may not build, catala may run from the
 * wrong folder, or the tool may be at fault. The repair view is for the first
 * case, and for a repair under way.
 *
 * On disk, a test has its original and maybe a draft, `<test>.repair`, written
 * by a save or by VS Code's backup of an unsaved repair. When the original
 * reads, it opens normally (and a draft goes to the trash, elsewhere). When it
 * does not:
 * 1. the module does not build, or the tool failed: error page, the draft kept;
 * 2. a draft exists: repair view. Fitting its target only means it is ready;
 * 3. the scope or module is gone, or a field no longer fits: repair view;
 * 4. every field fits: error page, `read` failed for a reason outside the test.
 * Rebuild's notes give 1 and 3; at most one of their four kinds is present.
 * One test per case in `tests/editors/open-decision.spec.ts`: change a
 * decision there.
 */
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
