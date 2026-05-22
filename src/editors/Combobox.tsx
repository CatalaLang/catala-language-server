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
import {
  useFloating,
  flip,
  size,
  offset,
  autoUpdate,
} from '@floating-ui/react';

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
  const listRef = useRef<HTMLUListElement | null>(null);

  const { refs, floatingStyles } = useFloating({
    placement: 'bottom-start',
    open: isOpen,
    middleware: [
      offset(2),
      flip(),
      size({
        apply({
          rects,
          availableHeight,
          elements,
        }: {
          rects: { reference: { width: number } };
          availableHeight: number;
          elements: { floating: HTMLElement };
        }) {
          elements.floating.style.width = `${rects.reference.width}px`;
          elements.floating.style.maxHeight = `${Math.min(availableHeight - 4, 200)}px`;
        },
      }),
    ],
    whileElementsMounted: autoUpdate,
  });

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
      requestAnimationFrame(() => inputRef.current?.focus());
    },
    [onChange, close]
  );

  // Close on outside click — floating listbox is portaled so not inside the reference
  useEffect(() => {
    if (!isOpen) return;
    const handler = (e: MouseEvent): void => {
      const target = e.target as Node;
      const refEl = refs.reference.current as Element | null;
      if (
        !refEl?.contains(target) &&
        !refs.floating.current?.contains(target)
      ) {
        close();
      }
    };
    document.addEventListener('mousedown', handler);
    return (): void => document.removeEventListener('mousedown', handler);
  }, [isOpen, close, refs.reference, refs.floating]);

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
    <div className="combobox" ref={refs.setReference}>
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
        createPortal(
          <ul
            ref={(el) => {
              listRef.current = el;
              refs.setFloating(el);
            }}
            id={listboxId}
            role="listbox"
            className="combobox-listbox"
            style={floatingStyles}
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
