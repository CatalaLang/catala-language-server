// For exhaustiveness checks
export function assertUnreachable(x: never): never {
  throw new Error(`Unexpected value: ${x}`);
}

/** A piece of text, and whether it is one of the searched terms. */
export type TextChunk = { text: string; match: boolean };
