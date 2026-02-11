import {
  type ReactElement,
  useState,
  useRef,
  useEffect,
  useCallback,
} from 'react';

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

  // Close on outside click
  useEffect(() => {
    if (!isOpen) return;
    const handler = (e: MouseEvent) => {
      if (
        containerRef.current &&
        !containerRef.current.contains(e.target as Node)
      ) {
        close();
      }
    };
    document.addEventListener('mousedown', handler);
    return () => document.removeEventListener('mousedown', handler);
  }, [isOpen, close]);

  // Scroll active option into view
  useEffect(() => {
    if (!isOpen || activeIndex < 0 || !listRef.current) return;
    const item = listRef.current.children[activeIndex] as
      | HTMLElement
      | undefined;
    item?.scrollIntoView({ block: 'nearest' });
  }, [activeIndex, isOpen]);

  const handleKeyDown = (e: React.KeyboardEvent) => {
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

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const v = e.target.value;
    setFilter(v);
    setActiveIndex(v ? 0 : -1);
    if (!isOpen) {
      setIsOpen(true);
    }
  };

  const handleInputFocus = () => {
    // Don't auto-open on focus; user clicks or presses arrow to open
  };

  const listboxId = 'combobox-listbox';
  const activeId =
    activeIndex >= 0 ? `combobox-option-${activeIndex}` : undefined;

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
        onFocus={handleInputFocus}
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
      {isOpen && (
        <ul
          ref={listRef}
          id={listboxId}
          role="listbox"
          className="combobox-listbox"
        >
          {filtered.length === 0 && (
            <li className="combobox-no-results" role="presentation">
              No matches
            </li>
          )}
          {filtered.map((option, i) => (
            <li
              key={option.value}
              id={`combobox-option-${i}`}
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
        </ul>
      )}
    </div>
  );
}
