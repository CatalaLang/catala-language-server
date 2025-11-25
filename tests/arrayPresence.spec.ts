import { describe, it, expect } from 'vitest';
import type {
  Diff,
  PathSegment,
  RuntimeValue,
} from '../src/generated/test_case';
import {
  computeActualOnlyIndices,
  indicesToRender,
  canAcceptAppend,
  canRemoveLast,
  findChildIndexDiff,
  isActualOnly,
  isExpectedOnly,
} from '../src/diff/arrayPresence';

function rvEmpty(): RuntimeValue {
  return { value: { kind: 'Empty' }, attrs: [] };
}
function rvInt(n: number): RuntimeValue {
  return { value: { kind: 'Integer', value: n }, attrs: [] };
}
function childPath(parent: PathSegment[], idx: number): PathSegment[] {
  return [...parent, { kind: 'ListIndex', value: idx }];
}

describe('array presence helpers', () => {
  const parent: PathSegment[] = [{ kind: 'StructField', value: 'out' }];

  it('computes phantom add indices for 0 expected / 2 actual', () => {
    const diffs: Diff[] = [
      {
        path: childPath(parent, 0),
        expected: rvEmpty(),
        actual: rvInt(10),
      } as unknown as Diff,
      {
        path: childPath(parent, 1),
        expected: rvEmpty(),
        actual: rvInt(20),
      } as unknown as Diff,
    ];
    expect(computeActualOnlyIndices(diffs, parent)).toEqual([0, 1]);
  });

  it('unions base indices and phantoms', () => {
    expect(indicesToRender(0, [0, 1])).toEqual([0, 1]);
    expect(indicesToRender(1, [1])).toEqual([0, 1]);
  });

  it('append-only accept rule', () => {
    expect(canAcceptAppend(0, 0)).toBe(true);
    expect(canAcceptAppend(1, 1)).toBe(true);
    expect(canAcceptAppend(1, 0)).toBe(false);
  });

  it('delete-last only rule', () => {
    expect(canRemoveLast(1, 0)).toBe(true);
    expect(canRemoveLast(2, 1)).toBe(true);
    expect(canRemoveLast(2, 0)).toBe(false);
  });

  it('finds diff at child index', () => {
    const d: Diff = {
      path: childPath(parent, 3),
      expected: rvEmpty(),
      actual: rvInt(1),
    } as unknown as Diff;
    expect(findChildIndexDiff([d], parent, 3)).toBe(d);
    expect(findChildIndexDiff([d], parent, 2)).toBeUndefined();
  });

  it('actual-only / expected-only guards', () => {
    const add: Diff = {
      path: childPath(parent, 0),
      expected: rvEmpty(),
      actual: rvInt(1),
    } as unknown as Diff;
    const del: Diff = {
      path: childPath(parent, 0),
      expected: rvInt(1),
      actual: rvEmpty(),
    } as unknown as Diff;
    expect(isActualOnly(add)).toBe(true);
    expect(isExpectedOnly(add)).toBe(false);
    expect(isActualOnly(del)).toBe(false);
    expect(isExpectedOnly(del)).toBe(true);
  });

  it('ignores nested descendant diffs for this parent', () => {
    const nestedPath = [
      ...childPath(parent, 0),
      { kind: 'ListIndex', value: 2 } as const,
    ];
    const d: Diff = {
      path: nestedPath as unknown as PathSegment[],
      expected: rvEmpty(),
      actual: rvInt(1),
    } as unknown as Diff;
    expect(computeActualOnlyIndices([d], parent)).toEqual([]);
  });
});
