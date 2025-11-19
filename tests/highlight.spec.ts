import { describe, it, expect } from 'vitest';
import { pathEquals, isPathPrefix } from '../src/diff/highlight';
import type { PathSegment } from '../src/generated/test_case';

function seg(kind: PathSegment['kind'], value: any): PathSegment {
  return { kind, value } as any;
}

describe('diff/highlight path comparison', () => {
  it('matches semantically equal strings with different Unicode normalization', () => {
    const nfc = 'é';
    const nfd = 'e\u0301';
    expect(nfc).not.toBe(nfd);

    const a = [seg('StructField', nfc)];
    const b = [seg('StructField', nfd)];
    expect(pathEquals(a, b)).toBe(true);
  });

  it('prefix check works structurally', () => {
    const p = [seg('StructField', 'foo')];
    const f = [seg('StructField', 'foo'), seg('StructField', 'bar')];
    expect(isPathPrefix(p, f)).toBe(true);
  });
});
