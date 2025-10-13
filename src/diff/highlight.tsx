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
 * Find a diff whose path exactly matches the given path.
 */
export function findMatchingDiff(
  diffs: Diff[],
  path: PathSegment[]
): Diff | undefined {
  return diffs.find((diff) => pathEquals(diff.path, path));
}
