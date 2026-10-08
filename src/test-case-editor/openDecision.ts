import type { CannotOpenCause, Recovery } from '../generated/catala_types';
import { assertUnreachable } from '../shared/util';

/**
 * What a test shows when `read` failed: the repair view, or an error page.
 *
 * `read` fails whenever anything goes wrong, not only when the scope changed
 * underneath the test: the module may not build, catala may run from the
 * wrong folder, or the tool may be at fault. The repair view is for the first
 * case only. Rebuild's notes say which case this is; at most one of the four
 * decisive notes is present, the working copy ones come on top. One test per
 * case in `tests/editors/open-decision.spec.ts`: change a decision there.
 */
export type OpenDecision =
  | { kind: 'repair' }
  | { kind: 'cannotOpen'; cause: CannotOpenCause; message: string };

export function decideOnReadFailure(
  readError: string,
  view: Recovery
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
  // Nothing to repair: `read` failed for a reason outside the test, and only
  // its message says which.
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
