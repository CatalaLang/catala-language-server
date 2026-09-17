import type { CSSProperties, ReactElement } from 'react';
import type { IntlShape } from 'react-intl';
import { useIntl } from 'react-intl';

export type Filter = {
  filter: string;
  option: 'include' | 'ignore' | 'exclude';
};

// To Remove: ByPass the @typescript-eslint/no-unused-vars
FilterPins;

function FilterPins({
  filters,
  setFilters,
}: {
  filters: Filter[];
  setFilters: React.Dispatch<React.SetStateAction<Filter[]>>;
}): ReactElement | null {
  const intl = useIntl();
  if (filters.length === 0) {
    return null;
  }
  // Always hand a new array to the setter: React skips the re-render when an
  // updater returns the very same reference.
  const onClickFilter = (filter: string): void => {
    setFilters((old) =>
      old.map((pin) => {
        if (filter == pin.filter) {
          return switchFilter(pin);
        } else {
          return pin;
        }
      })
    );
  };
  const removeFilter = (toRemove: string): void => {
    setFilters((old) => old.filter((current) => current.filter !== toRemove));
  };
  return (
    <div style={pinsStyle}>
      {filters.map((filter) => (
        <span
          key={filter.filter}
          onClick={(e) => {
            e.preventDefault();
            onClickFilter(filter.filter);
          }}
          style={{ ...pinStyle, ...pinBackground(filter.option) }}
          title={titlePin(intl, filter)}
        >
          <span>{filter.filter}</span>
          <span
            className="codicon codicon-close"
            title={intl.formatMessage({ id: 'filterPin.removeFilter' })}
            style={{ cursor: 'pointer' }}
            onClick={() => removeFilter(filter.filter)}
          />
        </span>
      ))}
    </div>
  );
}

function titlePin(intl: IntlShape, filter: Filter): string {
  switch (filter.option) {
    case 'include':
      return intl.formatMessage(
        {
          id: 'filterPin.inclusion',
          defaultMessage: 'Je veux que "{filter}" apparaisse',
        },
        { filter: filter.filter }
      );
    case 'exclude':
      return intl.formatMessage(
        {
          id: 'filterPin.exclusion',
          defaultMessage: 'Je ne veux pas que "{filter}" apparaisse',
        },
        { filter: filter.filter }
      );
    case 'ignore':
      return intl.formatMessage(
        {
          id: 'filterPin.ignore',
          defaultMessage: 'Ignorer le filtre "{filter}"',
        },
        { filter: filter.filter }
      );
  }
}

function pinBackground(
  highlighted: 'include' | 'ignore' | 'exclude'
): React.CSSProperties {
  switch (highlighted) {
    case 'include':
      return {
        backgroundColor: 'var(--vscode-notebookStatusSuccessIcon-foreground)',
      };
    case 'exclude':
      return { backgroundColor: 'var(--vscode-errorForeground)' };
    case 'ignore':
      return {
        backgroundColor: 'var(--vscode-button-background)',
        opacity: '0.5',
      };
  }
}

function switchFilter(filter: Filter): Filter {
  switch (filter.option) {
    case 'include':
      return { filter: filter.filter, option: 'exclude' };
    case 'ignore':
      return { filter: filter.filter, option: 'include' };
    case 'exclude':
      return { filter: filter.filter, option: 'ignore' };
  }
}

// The background is not set here: `pinBackground` gives it the colour of the
// pin's own option, and clicking cycles through them.
const pinStyle: CSSProperties = {
  display: 'inline-flex',
  alignItems: 'center',
  gap: 4,
  padding: '2px 6px',
  borderRadius: 4,
  color: 'var(--vscode-badge-foreground)',
  fontFamily: 'var(--vscode-editor-font-family, monospace)',
  cursor: 'pointer',
};

const pinsStyle: CSSProperties = {
  display: 'flex',
  flexWrap: 'wrap',
  gap: 6,
  margin: '0 0 8px 0',
};
