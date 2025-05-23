import type { ReactElement, ReactNode } from 'react';

type Props = {
  label: string;
  children: ReactNode;
  isCollapsed?: boolean;
  onToggleCollapse?: () => void;
};

export default function CollapsibleRow({
  label,
  children,
  isCollapsed,
  onToggleCollapse,
}: Props): ReactElement {
  const isCollapsible =
    isCollapsed !== undefined && onToggleCollapse !== undefined;

  return (
    <tr>
      <td className="row-label">
        <div className="row-header">
          {isCollapsible && (
            <button
              className="test-editor-collapse"
              onClick={onToggleCollapse}
              title={isCollapsed ? 'Expand' : 'Collapse'}
            >
              <span
                className={`codicon ${
                  isCollapsed ? 'codicon-unfold' : 'codicon-fold'
                }`}
              ></span>
            </button>
          )}
          <strong className="identifier" title={label}>
            {label}
          </strong>
        </div>
      </td>
      <td className="row-content">
        {(!isCollapsible || !isCollapsed) && children}
      </td>
    </tr>
  );
}
