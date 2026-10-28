/** Folding for literate Catala files: each ```catala… block is a range, so
 *  "fold all" hides the code and leaves the prose. Inside a block, indentation
 *  ranges stand in for the ones VS Code drops once a provider exists. */

export type FoldRange = { start: number; end: number };

const OPENING_FENCE = /^\s*```catala[\w-]*\s*$/;
const CLOSING_FENCE = /^\s*```\s*$/;

export function codeBlockRanges(lines: string[]): FoldRange[] {
  const ranges: FoldRange[] = [];
  let open: number | undefined;
  lines.forEach((line, i) => {
    if (open === undefined) {
      if (OPENING_FENCE.test(line)) open = i;
    } else if (CLOSING_FENCE.test(line)) {
      ranges.push({ start: open, end: i });
      open = undefined;
    }
  });
  return ranges;
}

// TODO: make something smarter (folds from the syntax: declarations, scopes,
// definitions) and drop this.
/** VS Code's indentation rule, between a block's fences: a line folds down to
 *  the last following line indented deeper; blank lines don't end a range. */
export function indentationRanges(
  lines: string[],
  block: FoldRange
): FoldRange[] {
  const indent = (i: number): number | undefined =>
    lines[i].trim() === '' ? undefined : lines[i].search(/\S/);
  const ranges: FoldRange[] = [];
  for (let i = block.start + 1; i < block.end; i++) {
    const own = indent(i);
    if (own === undefined) continue;
    let last = i;
    for (let j = i + 1; j < block.end; j++) {
      const other = indent(j);
      if (other === undefined) continue;
      if (other <= own) break;
      last = j;
    }
    if (last > i) ranges.push({ start: i, end: last });
  }
  return ranges;
}

export function catalaFoldingRanges(lines: string[]): FoldRange[] {
  return codeBlockRanges(lines).flatMap((block) => [
    block,
    ...indentationRanges(lines, block),
  ]);
}
