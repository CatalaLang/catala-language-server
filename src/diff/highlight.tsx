import type { Diff, PathSegment } from '../generated/catala_types';

/**
 * Compare two paths for exact equality using structural comparison
 * and NFC-normalized string values to avoid false negatives with
 * accented identifiers from different sources.
 */
function _normalizeStr(s: string): string {
  return s.normalize('NFC');
}

function _sameSegment(a: PathSegment, b: PathSegment): boolean {
  if (a.kind !== b.kind) return false;
  switch (a.kind) {
    case 'StructField':
    case 'EnumPayload':
      return (
        _normalizeStr(a.value) ===
        _normalizeStr((b as { kind: typeof a.kind; value: string }).value)
      );
    case 'ListIndex':
    case 'TupleIndex':
      return a.value === (b as { kind: typeof a.kind; value: number }).value;
    default:
      return false;
  }
}

export function pathEquals(a: PathSegment[], b: PathSegment[]): boolean {
  if (a.length !== b.length) return false;
  for (let i = 0; i < a.length; i++) {
    if (!_sameSegment(a[i], b[i])) return false;
  }
  return true;
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
 * Returns true if 'prefix' is a prefix of 'full'.
 * Uses structural comparison with NFC-normalized strings.
 */
export function isPathPrefix(
  prefix: PathSegment[],
  full: PathSegment[]
): boolean {
  if (prefix.length > full.length) return false;
  for (let i = 0; i < prefix.length; i++) {
    if (!_sameSegment(prefix[i], full[i])) return false;
  }
  return true;
}

/**
 * Returns true if the given path is a proper prefix of any diff path.
 */
export function isParentOfAnyDiff(diffs: Diff[], path: PathSegment[]): boolean {
  return diffs.some(
    (diff) => diff.path.length > path.length && isPathPrefix(path, diff.path)
  );
}
