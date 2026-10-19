import type { ReactElement } from 'react';
import { useIntl } from 'react-intl';

/** The pill under a tab and in table rows: how many items. With [onClick]
 *  it is a button. */
export function CountBadge({
  items,
  onClick,
  title,
}: {
  items?: number;
  onClick?: () => void;
  /** Replaces the count as the tooltip, e.g. where the pill navigates. */
  title?: string;
}): ReactElement | null {
  const intl = useIntl();
  if (items === undefined) return null;
  const tooltip =
    title ?? intl.formatMessage({ id: 'tab.items' }, { count: items });
  const className = [
    'count-badge',
    onClick ? 'count-badge-clickable' : '',
    items === 0 ? 'count-badge-zero' : '',
  ]
    .filter((c) => c !== '')
    .join(' ');
  return onClick ? (
    <button className={className} onClick={onClick} title={tooltip}>
      {items}
    </button>
  ) : (
    <span className={className} title={tooltip}>
      {items}
    </span>
  );
}
