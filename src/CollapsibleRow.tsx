import type { ReactElement, ReactNode } from 'react';

type Props = {
  label: string;
  children: ReactNode;
  className?: string;
  isCollapsed?: boolean;
  onToggleCollapse?: () => void;
};

export default function CollapsibleRow({
  label,
  children,
  className = '',
  isCollapsed,
  onToggleCollapse,
}: Props): ReactElement {
  const isCollapsible =
    isCollapsed !== undefined && onToggleCollapse !== undefined;

  return (
    <tr className={className}>
      <td>
        <div className="row-header">
          {isCollapsible && (
            <button
              className="test-editor-fold"
              onClick={onToggleCollapse}
              title={isCollapsed ? 'Unfold' : 'Fold'}
            >
              <span
                className={`codicon ${
                  isCollapsed ? 'codicon-unfold' : 'codicon-fold'
                }`}
              ></span>
            </button>
          )}
          <strong className="identifier">{label}</strong>
        </div>
      </td>
      <td>{(!isCollapsible || !isCollapsed) && children}</td>
    </tr>
  );
}
