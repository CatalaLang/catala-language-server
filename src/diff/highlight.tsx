import { type ReactElement } from 'react';
import type { Diff, PathSegment } from '../generated/test_case';

/**
 * Compare two paths for exact equality.
 */
export function pathEquals(a: PathSegment[], b: PathSegment[]): boolean {
  return (
    a.length === b.length &&
    a.every((seg, i) => JSON.stringify(seg) === JSON.stringify(b[i]))
  );
}

/**
 * Returns true if 'prefix' is a prefix of 'full'.
 */
export function isPathPrefix(
  prefix: PathSegment[],
  full: PathSegment[]
): boolean {
  if (prefix.length > full.length) return false;
  return prefix.every(
    (seg, i) => JSON.stringify(seg) === JSON.stringify(full[i])
  );
}

/**
 * Find a diff whose path exactly matches the given path.
 */
export function findMatchingDiff(
  diffs: Diff[],
  path: PathSegment[]
): Diff | undefined {
  return diffs.find((diff) => pathEquals(diff.path, path));
}

/**
 * Returns true if the given path is a proper prefix of any diff path.
 */
export function isParentOfAnyDiff(diffs: Diff[], path: PathSegment[]): boolean {
  return diffs.some(
    (diff) => diff.path.length > path.length && isPathPrefix(path, diff.path)
  );
}

/**
 * Hook that only highlights the presence of diffs without inlining expected/actual values.
 * Used for the right-hand "actual" pane in two-pane mode.
 */
export function createHighlightOnlyHook(diffs: Diff[]) {
  return (editor: ReactElement, path: PathSegment[]): ReactElement => {
    const matchingDiff = findMatchingDiff(diffs, path);
    const isParentOfDiff = !matchingDiff && isParentOfAnyDiff(diffs, path);

    if (matchingDiff || isParentOfDiff) {
      return <div className="diff-highlight container-diff">{editor}</div>;
    }
    return editor;
  };
}
