import type { CSSProperties, ReactElement } from 'react';
import type { Filter } from '../FilterPin';
import { splitOnTerms } from './util';

export function HighlightText({
  filters,
  text,
}: {
  filters: Filter[];
  text: string;
}): ReactElement {
  return (
    <>
      {splitOnTerms(text, filters).map((chunk, index) =>
        chunk.match ? (
          <mark key={index} style={filterMatchStyle}>
            {chunk.text}
          </mark>
        ) : (
          chunk.text
        )
      )}
    </>
  );
}
const filterMatchStyle: CSSProperties = {
  backgroundColor:
    'var(--vscode-editor-findMatchHighlightBackground, rgba(234, 92, 0, 0.33))',
  color: 'inherit',
  borderRadius: 2,
};
