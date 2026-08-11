// For exhaustiveness checks
export function assertUnreachable(x: never): never {
  throw new Error(`Unexpected value: ${x}`);
}

/** Escapes the characters that would otherwise be regexp syntax. */
function escapeRegExp(text: string): string {
  return text.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

/** A piece of text, and whether it is one of the searched terms. */
export type TextChunk = { text: string; match: boolean };

/**
 * Cuts `text` around every occurrence of any of `terms`, case insensitively.
 * Used by the views to highlight the part of a text that made a filter match.
 * Empty terms are ignored, and no term at all yields the whole text as a single
 * unmatched chunk.
 */
export function splitOnTerms(text: string, terms: string[]): TextChunk[] {
  const searched = terms.filter((term) => term.length > 0).map(escapeRegExp);
  if (searched.length === 0) {
    return [{ text, match: false }];
  }
  // Split the string depending on the regexp, every item with odd index is a match
  // to the regexp. Even if the regexp matches the start of the string.
  return text
    .split(new RegExp(`(${searched.join('|')})`, 'gi'))
    .map((chunk, index) => ({ text: chunk, match: index % 2 === 1 }))
    .filter((chunk) => chunk.text.length > 0);
}
