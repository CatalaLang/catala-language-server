import { Fragment, type ReactElement, type ReactNode } from 'react';

/**
 * Renders a snake_case identifier with a soft break opportunity after each
 * underscore, so long names wrap at segment boundaries instead of overflowing
 * or being truncated. The rendered text is the identifier verbatim.
 */
export function Identifier({ name }: { name: string }): ReactElement {
  const parts = name.split('_');
  return (
    <>
      {parts.map((part, i) => (
        <Fragment key={i}>
          {i > 0 && '_'}
          {i > 0 && <wbr />}
          {part}
        </Fragment>
      ))}
    </>
  );
}

/** Wraps plain-string labels in Identifier; leaves custom nodes untouched. */
export function wrapIdentifier(label: ReactNode): ReactNode {
  return typeof label === 'string' ? <Identifier name={label} /> : label;
}
