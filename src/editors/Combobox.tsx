import {
  type ReactElement,
  useId,
  useState,
  useRef,
  useEffect,
  useCallback,
} from 'react';
import { createPortal } from 'react-dom';
import { FormattedMessage } from 'react-intl';

export type ComboboxOption = {
  value: string;
  label: string;
};

type ComboboxProps = {
  options: ComboboxOption[];
  value: string | null;
  placeholder?: string;
  disabled?: boolean;
  onChange(value: string | null): void;
};

export function Combobox(props: ComboboxProps): ReactElement {
  const {
    options,
    value,
    placeholder = '',
    disabled = false,
    onChange,
  } = props;

  const [isOpen, setIsOpen] = useState(false);
  const [filter, setFilter] = useState('');
  const [activeIndex, setActiveIndex] = useState(-1);
  const [listboxPos, setListboxPos] = useState<{
    top: number;
    left: number;
    width: number;
  } | null>(null);

  const inputRef = useRef<HTMLInputElement>(null);
  const listRef = useRef<HTMLUListElement>(null);
  const containerRef = useRef<HTMLDivElement>(null);

  const selectedLabel = options.find((o) => o.value === value)?.label ?? '';

  const filtered = filter
    ? options.filter((o) =>
        o.label.toLowerCase().includes(filter.toLowerCase())
      )
    : options;

  const open = useCallback(() => {
    if (disabled) return;
    setIsOpen(true);
    setFilter('');
    setActiveIndex(-1);
  }, [disabled]);

  const close = useCallback(() => {
    setIsOpen(false);
    setFilter('');
    setActiveIndex(-1);
  }, []);

  const select = useCallback(
    (optionValue: string | null) => {
      onChange(optionValue);
      close();
      // Return focus to the input after selection
      requestAnimationFrame(() => inputRef.current?.focus());
    },
    [onChange, close]
  );

  // Compute portal position when opening
  useEffect(() => {
    if (!isOpen || !containerRef.current) return;
    const rect = containerRef.current.getBoundingClientRect();
    const spaceBelow = window.innerHeight - rect.bottom;
    const maxMenuHeight = 200;
    const top =
      spaceBelow >= maxMenuHeight || spaceBelow > rect.top
        ? rect.bottom - 1
        : rect.top - maxMenuHeight + 1;
    setListboxPos({ top, left: rect.left, width: rect.width });
  }, [isOpen]);

  // Close on outside click — listRef is portaled so not inside containerRef
  useEffect(() => {
    if (!isOpen) return;
    const handler = (e: MouseEvent): void => {
      const target = e.target as Node;
      if (
        !containerRef.current?.contains(target) &&
        !listRef.current?.contains(target)
      ) {
        close();
      }
    };
    document.addEventListener('mousedown', handler);
    return (): void => document.removeEventListener('mousedown', handler);
  }, [isOpen, close]);

  // Close on scroll (any ancestor scrolling)
  useEffect(() => {
    if (!isOpen) return;
    const handler = (e: Event): void => {
      if (listRef.current?.contains(e.target as Node)) return;
      close();
    };
    window.addEventListener('scroll', handler, true);
    return (): void => window.removeEventListener('scroll', handler, true);
  }, [isOpen, close]);

  // Scroll active option into view
  useEffect((): void => {
    if (!isOpen || activeIndex < 0 || !listRef.current) return;
    const item = listRef.current.children[activeIndex] as
      | HTMLElement
      | undefined;
    item?.scrollIntoView({ block: 'nearest' });
  }, [activeIndex, isOpen]);

  const handleKeyDown = (e: React.KeyboardEvent): void => {
    if (disabled) return;

    switch (e.key) {
      case 'ArrowDown':
        e.preventDefault();
        if (!isOpen) {
          open();
        } else {
          setActiveIndex((i) => Math.min(i + 1, filtered.length - 1));
        }
        break;
      case 'ArrowUp':
        e.preventDefault();
        if (isOpen) {
          setActiveIndex((i) => Math.max(i - 1, 0));
        }
        break;
      case 'Enter':
        e.preventDefault();
        if (isOpen) {
          if (activeIndex >= 0 && activeIndex < filtered.length) {
            select(filtered[activeIndex].value);
          } else {
            // Empty input, no highlight → unset
            select(null);
          }
        } else {
          open();
        }
        break;
      case 'Tab':
        if (isOpen) {
          if (activeIndex >= 0 && activeIndex < filtered.length) {
            onChange(filtered[activeIndex].value);
          } else {
            // Empty input, no highlight → unset
            onChange(null);
          }
          close();
        }
        break;
      case 'Escape':
        if (isOpen) {
          e.preventDefault();
          e.stopPropagation();
          close();
        }
        break;
    }
  };

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>): void => {
    const v = e.target.value;
    setFilter(v);
    setActiveIndex(v ? 0 : -1);
    if (!isOpen) {
      setIsOpen(true);
    }
  };

  const uid = useId();
  const listboxId = `combobox-listbox-${uid}`;
  const activeId =
    activeIndex >= 0 ? `combobox-option-${uid}-${activeIndex}` : undefined;

  return (
    <div className="combobox" ref={containerRef}>
      <input
        ref={inputRef}
        role="combobox"
        aria-expanded={isOpen}
        aria-controls={listboxId}
        aria-activedescendant={activeId}
        aria-autocomplete="list"
        value={isOpen ? filter : selectedLabel}
        placeholder={placeholder}
        disabled={disabled}
        onChange={handleInputChange}
        onKeyDown={handleKeyDown}
        onClick={() => {
          if (!isOpen) open();
        }}
      />
      <button
        className="combobox-toggle"
        tabIndex={-1}
        aria-label="Toggle options"
        disabled={disabled}
        onClick={() => {
          if (isOpen) {
            close();
          } else {
            open();
            requestAnimationFrame(() => inputRef.current?.focus());
          }
        }}
      >
        <span className={`codicon codicon-chevron-${isOpen ? 'up' : 'down'}`} />
      </button>
      {isOpen &&
        listboxPos &&
        createPortal(
          <ul
            ref={listRef}
            id={listboxId}
            role="listbox"
            className="combobox-listbox"
            style={{
              top: listboxPos.top,
              left: listboxPos.left,
              width: listboxPos.width,
            }}
          >
            {filtered.length === 0 && (
              <li className="combobox-no-results" role="presentation">
                <FormattedMessage
                  id="combobox.noMatches"
                  defaultMessage="No matches"
                />
              </li>
            )}
            {filtered.map((option, i) => (
              <li
                key={option.value}
                id={`combobox-option-${uid}-${i}`}
                role="option"
                aria-selected={option.value === value}
                className={`combobox-option${i === activeIndex ? ' active' : ''}${option.value === value ? ' selected' : ''}`}
                onMouseDown={(e) => {
                  e.preventDefault(); // prevent blur
                  select(option.value);
                }}
                onMouseEnter={() => setActiveIndex(i)}
              >
                {option.label}
              </li>
            ))}
          </ul>,
          document.body
        )}
    </div>
  );
}
