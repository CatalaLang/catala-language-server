import { describe, it, expect } from 'vitest';
import { catalaFoldingRanges } from '../src/extension/catalaFolding';

const file = [
  '# Article 1', // 0
  '',
  'Some prose.',
  '```catala', // 3
  'declaration scope S:',
  '  input x content integer',
  '  output y content integer',
  '```', // 7
  '',
  '```catala-metadata', // 9
  'declaration structure P:',
  '  data a content date',
  '```', // 12
  '```ocaml', // 13
  'let x = 1',
  '```', // 15
];

describe('catalaFoldingRanges', () => {
  it('makes each catala block a range, fences included', () => {
    expect(catalaFoldingRanges(file)).toEqual([
      { start: 3, end: 7 },
      { start: 9, end: 12 },
    ]);
  });

  it('ignores an unclosed block', () => {
    expect(catalaFoldingRanges(['```catala', 'x'])).toEqual([]);
  });
});
