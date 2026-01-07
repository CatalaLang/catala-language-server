import { useEffect, useRef } from 'react';
import type { ReactElement, ReactNode } from 'react';
import { createPortal } from 'react-dom';

type ContextMenuProps = {
  isOpen: boolean;
  onClose: () => void;
  anchorElement: HTMLElement | null;
  children: ReactNode;
};

export function ContextMenu({
  isOpen,
  onClose,
  anchorElement,
  children,
}: ContextMenuProps): ReactElement | null {
  const menuRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!isOpen) return;

    const handleClickOutside = (event: MouseEvent): void => {
      if (
        menuRef.current &&
        !menuRef.current.contains(event.target as Node) &&
        anchorElement &&
        !anchorElement.contains(event.target as Node)
      ) {
        onClose();
      }
    };

    const handleEscape = (event: KeyboardEvent): void => {
      if (event.key === 'Escape') {
        onClose();
      }
    };

    document.addEventListener('mousedown', handleClickOutside);
    document.addEventListener('keydown', handleEscape);

    return (): void => {
      document.removeEventListener('mousedown', handleClickOutside);
      document.removeEventListener('keydown', handleEscape);
    };
  }, [isOpen, onClose, anchorElement]);

  // Calculate position
  useEffect(() => {
    if (!isOpen || !anchorElement || !menuRef.current) return;

    const rect = anchorElement.getBoundingClientRect();
    const menu = menuRef.current;
    const viewportHeight = window.innerHeight;
    const viewportWidth = window.innerWidth;

    // Reset position to measure actual menu size
    menu.style.top = '0';
    menu.style.left = '0';
    menu.style.visibility = 'hidden';
    menu.style.display = 'block';

    const menuRect = menu.getBoundingClientRect();
    const menuHeight = menuRect.height;
    const menuWidth = menuRect.width;

    // Calculate best position
    const spaceBelow = viewportHeight - rect.bottom;
    const spaceAbove = rect.top;

    let top: number;
    let left: number;

    // Vertical positioning
    if (spaceBelow >= menuHeight || spaceBelow > spaceAbove) {
      // Position below
      top = rect.bottom + 4;
    } else {
      // Position above
      top = Math.max(8, rect.top - menuHeight - 4);
    }

    // Horizontal positioning
    left = rect.left;
    // Ensure menu doesn't go off right edge
    if (left + menuWidth > viewportWidth) {
      left = Math.max(8, viewportWidth - menuWidth - 8);
    }

    menu.style.top = `${top}px`;
    menu.style.left = `${left}px`;
    menu.style.visibility = 'visible';
  }, [isOpen, anchorElement]);

  if (!isOpen) return null;

  return createPortal(
    <div ref={menuRef} className="context-menu">
      {children}
    </div>,
    document.body
  );
}
