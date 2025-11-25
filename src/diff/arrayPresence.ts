/* Array presence diff helpers (pure).
 * - Arrays asserted as a whole.
 * - actual-only: present in actual, absent in expected.
 * - expected-only: present in expected, absent in actual.
 * - Append-only accept and delete-last are path-stable; middle ops invalidate.
 */
import type {
  Diff,
  PathSegment,
  RuntimeValue,
} from '../generated/test_case';
import { isPathPrefix } from './highlight';

export function isEmptyValue(rv: RuntimeValue): boolean {
  return rv.value.kind === 'Empty';
}

export function childIndexFromPath(
  parentPath: PathSegment[],
  path: PathSegment[]
): number | null {
  if (!isPathPrefix(parentPath, path)) return null;
  // Only consider direct children (no deeper descendants)
  if (path.length !== parentPath.length + 1) return null;
  const seg = path[parentPath.length];
  return seg?.kind === 'ListIndex' ? seg.value : null;
}

export function findChildIndexDiff(
  diffs: Diff[],
  parentPath: PathSegment[],
  index: number
): Diff | undefined {
  return diffs.find((d) => childIndexFromPath(parentPath, d.path) === index);
}

export function isActualOnly(d: Diff): boolean {
  return isEmptyValue(d.expected) && !isEmptyValue(d.actual);
}

export function isExpectedOnly(d: Diff): boolean {
  return !isEmptyValue(d.expected) && isEmptyValue(d.actual);
}

export function computeActualOnlyIndices(
  diffs: Diff[],
  parentPath: PathSegment[]
): number[] {
  const res: number[] = [];
  for (const d of diffs) {
    const idx = childIndexFromPath(parentPath, d.path);
    if (idx === null) continue;
    if (isActualOnly(d)) res.push(idx);
  }
  return Array.from(new Set(res)).sort((a, b) => a - b);
}

export function indicesToRender(
  arrayLength: number,
  actualOnlyIndices: number[]
): number[] {
  const base = Array.from({ length: arrayLength }, (_, i) => i);
  return Array.from(new Set([...base, ...actualOnlyIndices])).sort((a, b) => a - b);
}

export function canAcceptAppend(arrayLength: number, index: number): boolean {
  return index === arrayLength;
}

export function canRemoveLast(arrayLength: number, index: number): boolean {
  return index === arrayLength - 1;
}
