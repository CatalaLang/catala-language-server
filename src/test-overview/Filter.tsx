import { type ReactElement } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';

import { VscodeTextfield } from '@vscode-elements/react-elements';
import { matchFilter, scopesFromTests } from './testUtils';
import type { TestMacro } from './types';
import { Loading } from './widgets';

/**
 * Type to build the Filter component with filter on Scope, on description and
 * wether the test is a Catala Test Case editor generated test
 */
type FilterArg = {
  tests: TestMacro[] | undefined;
  filter: string;
  filterScope: string[];
  setFilterScope: React.Dispatch<React.SetStateAction<string[]>>;
  setFilter: React.Dispatch<React.SetStateAction<string>>;
  filterGui: boolean;
  setFilterGui: React.Dispatch<React.SetStateAction<boolean>>;
};

type ScopeFilterArg = {
  tests: TestMacro[] | undefined;
  filterScope: string[];
  setFilterScope: React.Dispatch<React.SetStateAction<string[]>>;
};

function ScopeFilter({
  tests,
  filterScope,
  setFilterScope,
}: ScopeFilterArg): ReactElement {
  const intl = useIntl();
  let filteredScope = scopesFromTests(tests ?? []);
  return (
    <div className="pins">
      <div
        style={{
          display: 'flex',
          flexWrap: 'wrap',
          gap: 1,
          textAlign: 'center',
        }}
      >
        {filteredScope.length == 0 ? (
          <span className="no-scope">
            <FormattedMessage
              id="generalTests.noScopeFound"
              defaultMessage="Aucun Champ d'application trouvé"
            />
          </span>
        ) : (
          Array.from(filteredScope).map((scope, index) => (
            <span
              key={index}
              title={intl.formatMessage(
                {
                  id: 'generalTests.tooltip.filterScope',
                  defaultMessage:
                    'Filtrer les tests selon le champ d\'application "{scope}"',
                },
                { scope }
              )}
              onClick={(event) => {
                event.preventDefault();
                setFilterScope((previous) => {
                  let length = previous.length;
                  let newScopes = previous.filter((value) => value != scope);
                  if (newScopes.length == length) {
                    newScopes.push(scope);
                  }
                  return newScopes;
                });
              }}
              className={`scope-title ${filterScope.includes(scope) ? 'selected-filter' : ''}`}
            >
              {scope}
            </span>
          ))
        )}
      </div>
    </div>
  );
}

export function Filter({
  tests,
  filter,
  filterScope,
  setFilterScope,
  setFilter,
  filterGui,
  setFilterGui,
}: FilterArg): ReactElement {
  const intl = useIntl();
  // Restore the default state: GUI-only checkbox checked, no scope selected,
  // empty search bar.
  const resetFilters = (): void => {
    setFilterGui(true);
    setFilterScope([]);
    setFilter('');
  };

  const filteredTests = tests?.filter((test) =>
    matchFilter(test, filter, [], filterGui)
  );

  return (
    <div className="box-filter">
      <div className="filter-title">
        <h2>
          <FormattedMessage
            id="generalTests.filters"
            defaultMessage="Filtres"
          />
        </h2>
        <span
          className="vscode-button reset-filters"
          onClick={(event) => {
            event.preventDefault();
            resetFilters();
          }}
        >
          <span className="codicon codicon-clear-all" />
          <FormattedMessage
            id="generalTests.resetFilters"
            defaultMessage="Réinitialiser les filtres"
          />
        </span>
      </div>
      {tests === undefined ? (
        <Loading size="small" />
      ) : (
        <>
          <div style={{ display: 'flex', flexDirection: 'column' }}>
            <div>
              <input
                type="checkbox"
                checked={filterGui}
                onChange={(event) => setFilterGui(event.target.checked)}
              />
              <FormattedMessage
                id="generalTests.guiOnly"
                defaultMessage="Tests GUI uniquement"
              />
            </div>
          </div>
          <ScopeFilter
            tests={filteredTests}
            filterScope={filterScope}
            setFilterScope={setFilterScope}
          />
          <div style={{ marginLeft: 'auto' }}>
            <VscodeTextfield
              className="search-bar"
              value={filter}
              placeholder={intl.formatMessage({
                id: 'generalTests.searchPlaceholder',
                defaultMessage: 'Rechercher un test…',
              })}
              onInput={(e) => {
                const value = (e.target as HTMLInputElement).value;
                setFilter(value);
              }}
            >
              <span className="codicon codicon-search" slot="content-before" />
            </VscodeTextfield>
          </div>
        </>
      )}
    </div>
  );
}
