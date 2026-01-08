import { describe, it, expect } from 'vitest';
import type {
  Diff,
  PathSegment,
  RuntimeValue,
} from '../../src/generated/catala_types';
import {
  computeActualOnlyIndices,
  indicesToRender,
} from '../../src/diff/arrayPresence';

function rv(kind: string): RuntimeValue {
  return { value: { kind } as any, attrs: [] };
}

describe('Array presence logic debugging', () => {
  it('computes actual-only indices correctly', () => {
    const currentPath: PathSegment[] = [];
    const diffs: Diff[] = [
      {
        path: [{ kind: 'ListIndex', value: 0 }],
        expected: rv('Empty'),
        actual: rv('Integer'),
      },
    ];

    const actualOnlyIndices = computeActualOnlyIndices(diffs, currentPath);
    console.log('actualOnlyIndices:', actualOnlyIndices);
    expect(actualOnlyIndices).toEqual([0]);
  });

  it('computes indices to render correctly for empty array with phantom', () => {
    const arrayLength = 0;
    const phantomIndices = [0];

    const indices = indicesToRender(arrayLength, phantomIndices);
    console.log('indices to render:', indices);
    expect(indices).toEqual([0]);
  });
});
