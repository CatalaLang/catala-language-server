/** Folding for literate Catala files: each ```catala… block is a range, so
 *  "fold all" hides the code and leaves the prose. */

export type FoldRange = { start: number; end: number };

const OPENING_FENCE = /^\s*```catala[\w-]*\s*$/;
const CLOSING_FENCE = /^\s*```\s*$/;

export function catalaFoldingRanges(lines: string[]): FoldRange[] {
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
