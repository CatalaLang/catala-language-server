// Minimal context menu implementation. If we need nested menus, focus trapping,
// or more complex behavior, consider @radix-ui/react-dropdown-menu or @floating-ui/react.

import { useEffect, useRef } from 'react';
import type { ReactElement, ReactNode } from 'react';
import { createPortal } from 'react-dom';

type ContextMenuProps = {
  isOpen: boolean;
  onClose: () => void;
  anchorElement?: HTMLElement | null;
  position?: { x: number; y: number }; // Alternative to anchorElement: position at coordinates
  children: ReactNode;
};

export function ContextMenu({
  isOpen,
  onClose,
  anchorElement,
  position,
  children,
}: ContextMenuProps): ReactElement | null {
  const menuRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!isOpen) return;

    const handleClickOutside = (event: MouseEvent): void => {
      if (
        menuRef.current &&
        !menuRef.current.contains(event.target as Node) &&
        !anchorElement?.contains(event.target as Node)
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
    if (!isOpen || !menuRef.current) return;
    if (!anchorElement && !position) return;

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

    let top: number;
    let left: number;

    if (position) {
      // Position at coordinates (for right-click context menus)
      top = position.y;
      left = position.x;

      // Adjust if menu would go off bottom
      if (top + menuHeight > viewportHeight) {
        top = Math.max(8, viewportHeight - menuHeight - 8);
      }
    } else {
      // Position relative to anchor element
      const rect = anchorElement!.getBoundingClientRect();
      const spaceBelow = viewportHeight - rect.bottom;
      const spaceAbove = rect.top;

      // Vertical positioning
      if (spaceBelow >= menuHeight || spaceBelow > spaceAbove) {
        top = rect.bottom + 4;
      } else {
        top = Math.max(8, rect.top - menuHeight - 4);
      }

      left = rect.left;
    }

    // Ensure menu doesn't go off right edge
    if (left + menuWidth > viewportWidth) {
      left = Math.max(8, viewportWidth - menuWidth - 8);
    }

    menu.style.top = `${top}px`;
    menu.style.left = `${left}px`;
    menu.style.visibility = 'visible';
  }, [isOpen, anchorElement, position]);

  if (!isOpen) return null;

  return createPortal(
    <div ref={menuRef} className="context-menu">
      {children}
    </div>,
    document.body
  );
}
