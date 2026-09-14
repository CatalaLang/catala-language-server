import type { IntlShape } from 'react-intl';

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

export type Filter = {
  filter: string;
  option: 'include' | 'ignore' | 'exclude';
};
/**
 * Cuts `text` around every occurrence of any of `terms`, case insensitively.
 * Used by the views to highlight the part of a text that made a filter match.
 * Empty terms are ignored, and no term at all yields the whole text as a single
 * unmatched chunk.
 */
export function splitOnTerms(text: string, terms: Filter[]): TextChunk[] {
  const searched = terms
    .filter((term) => term.filter.length > 0 && term.option == 'include')
    .map((filter) => escapeRegExp(filter.filter));
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

/* Kept out of [pinStyle] so the colour can follow a per-pin boolean. */
export const pinBackground = (
  highlighted: 'include' | 'ignore' | 'exclude'
): React.CSSProperties => {
  switch (highlighted) {
    case 'include':
      return {
        backgroundColor: 'var(--vscode-notebookStatusSuccessIcon-foreground)',
      };
    case 'exclude':
      return { backgroundColor: 'var(--vscode-errorForeground)' };
    case 'ignore':
      return {
        backgroundColor: 'var(--vscode-button-background)',
        opacity: '0.5',
      };
  }
};

export const switchFilter = (filter: Filter): Filter => {
  switch (filter.option) {
    case 'include':
      return { filter: filter.filter, option: 'exclude' };
    case 'ignore':
      return { filter: filter.filter, option: 'include' };
    case 'exclude':
      return { filter: filter.filter, option: 'ignore' };
  }
};

export function titlePin(intl: IntlShape, filter: Filter): string {
  switch (filter.option) {
    case 'include':
      return intl.formatMessage(
        {
          id: 'generalTests.filterPin.inclusion',
          defaultMessage: 'Je veux que "{filter}" apparaisse',
        },
        { filter: filter.filter }
      );
    case 'exclude':
      return intl.formatMessage(
        {
          id: 'generalTests.filterPin.exclusion',
          defaultMessage: 'Je ne veux pas que "{filter}" apparaisse',
        },
        { filter: filter.filter }
      );
    case 'ignore':
      return intl.formatMessage(
        {
          id: 'generalTests.filterPin.ignore',
          defaultMessage: 'Ignorer le filtre "{filter}"',
        },
        { filter: filter.filter }
      );
  }
}
