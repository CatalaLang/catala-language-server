import { describe, it, expect } from 'vitest';
import {
  catalaFoldingRanges,
  codeBlockRanges,
  indentationRanges,
} from '../src/extension/catalaFolding';

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

describe('codeBlockRanges', () => {
  it('makes each catala block a range, fences included', () => {
    expect(codeBlockRanges(file)).toEqual([
      { start: 3, end: 7 },
      { start: 9, end: 12 },
    ]);
  });

  it('ignores an unclosed block', () => {
    expect(codeBlockRanges(['```catala', 'x'])).toEqual([]);
  });
});

describe('indentationRanges', () => {
  const ranges = (body: string[]) =>
    indentationRanges(['```catala', ...body, '```'], {
      start: 0,
      end: body.length + 1,
    });

  it('folds a line down to the last line indented deeper', () => {
    expect(
      ranges([
        'scope S:', // 1
        '  definition x equals', // 2
        '    1', // 3
        '  definition y equals 2', // 4
        'scope T:', // 5
      ])
    ).toEqual([
      { start: 1, end: 4 },
      { start: 2, end: 3 },
    ]);
  });

  it('does not end a range on a blank line, nor include trailing ones', () => {
    expect(ranges(['scope S:', '  a', '', '  b', '', 'scope T:'])).toEqual([
      { start: 1, end: 4 },
    ]);
  });

  it('stays inside the block', () => {
    expect(catalaFoldingRanges(file)).toEqual([
      { start: 3, end: 7 },
      { start: 4, end: 6 },
      { start: 9, end: 12 },
      { start: 10, end: 11 },
    ]);
  });
});
