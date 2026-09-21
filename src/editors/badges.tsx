import type { ReactElement } from 'react';
import { useIntl } from 'react-intl';

/** The pill under a tab and in table rows: how many items, quietly; and,
 *  when some values inside are still blank, how many, in the warning
 *  colours. With [onClick] it is a button. */
export function CountBadge({
  items,
  unfilled = 0,
  onClick,
  title,
}: {
  items?: number;
  unfilled?: number;
  onClick?: () => void;
  /** Replaces the counts as the tooltip, e.g. where the pill navigates. */
  title?: string;
}): ReactElement | null {
  const intl = useIntl();
  if (items === undefined && unfilled === 0) return null;
  const counts = [
    items === undefined
      ? undefined
      : intl.formatMessage({ id: 'tab.items' }, { count: items }),
    unfilled === 0
      ? undefined
      : intl.formatMessage({ id: 'testEditor.unfilled' }, { count: unfilled }),
  ]
    .filter((t) => t !== undefined)
    .join(', ');
  const tooltip = title ?? counts;
  const className = [
    'count-badge',
    unfilled > 0 ? 'count-badge-unfilled' : '',
    onClick ? 'count-badge-clickable' : '',
    items === 0 && unfilled === 0 ? 'count-badge-zero' : '',
  ]
    .filter((c) => c !== '')
    .join(' ');
  const content = (
    <>
      {items}
      {unfilled > 0 && (
        <>
          {items !== undefined && <span className="count-badge-sep">·</span>}
          <span className="codicon codicon-circle-large-outline"></span>
          {unfilled}
        </>
      )}
    </>
  );
  return onClick ? (
    <button className={className} onClick={onClick} title={tooltip}>
      {content}
    </button>
  ) : (
    <span className={className} title={tooltip}>
      {content}
    </span>
  );
}
