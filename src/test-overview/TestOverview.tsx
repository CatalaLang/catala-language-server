import { useState, type ReactElement } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';

import { type WebviewApi } from 'vscode-webview';

import { AddNewTest, RunAllTests } from './buttons';
import { Filter } from './Filter';
import { matchFilter, noFilter } from './testUtils';
import { TestsGrid } from './TestsView';
import { useTests } from './useTests';
import { Loading } from './widgets';

type TestOverviewArg = {
  vscode: WebviewApi<unknown>;
};

export default function TestOverview({
  vscode,
}: TestOverviewArg): ReactElement {
  const intl = useIntl();
  const [filter, setFilter] = useState<string>('');
  const [filterScope, setFilterScope] = useState<string[]>([]);
  const [filterGui, setFilterGui] = useState<boolean>(true);
  const [grid, setGrid] = useState<boolean>(true);
  const { tests, reload, onRun, runTests, refresh } = useTests(vscode);

  const filteredTests = tests?.filter((test) =>
    matchFilter(test, filter, filterScope, filterGui)
  );

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: '10px' }}>
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          flexDirection: 'row',
        }}
      >
        <FormattedMessage
          id="generalTests.title"
          defaultMessage="Ensemble des tests"
          children={(msg) => <h1>{msg}</h1>}
        />
        <div className="tests-button">
          <AddNewTest vscode={vscode} />
          <RunAllTests
            onRun={() =>
              runTests(
                noFilter(filter, filterScope, filterGui) ? 'all' : filteredTests
              )
            }
          />{' '}
        </div>
      </div>
      <Filter
        tests={tests}
        setFilterScope={setFilterScope}
        filterScope={filterScope}
        filterGui={filterGui}
        setFilterGui={setFilterGui}
        filter={filter}
        setFilter={setFilter}
      />
      <div className="select-test-print">
        <FormattedMessage
          id="generalTests.display"
          defaultMessage="Affichage :"
          children={(msg) => <h3>{msg}</h3>}
        />
        <div
          className={`pp-button ${grid ? 'selected' : ''}`}
          title={intl.formatMessage({
            id: 'generalTests.tooltip.displayCard',
            defaultMessage: 'Afficher les tests sous forme de cartes',
          })}
          onClick={(event) => {
            event.preventDefault();
            setGrid((_) => true);
          }}
        >
          <span className="codicon codicon-layout" />
          <span>
            <FormattedMessage id="generalTests.card" defaultMessage="Carte" />
          </span>
        </div>
        <div
          className={`pp-button ${grid ? '' : 'selected'}`}
          title={intl.formatMessage({
            id: 'generalTests.tooltip.displayList',
            defaultMessage: 'Afficher les tests sous forme de liste',
          })}
          onClick={(event) => {
            event.preventDefault();
            setGrid((_) => false);
          }}
        >
          <span className="codicon codicon-list-unordered" />
          <span>
            <FormattedMessage id="generalTests.list" defaultMessage="Liste" />
          </span>
        </div>
        <div
          className="refresh-box"
          title={intl.formatMessage(
            reload
              ? {
                  id: 'generalTests.tooltip.reloading',
                  defaultMessage: 'Rechargement en cours…',
                }
              : {
                  id: 'generalTests.tooltip.reload',
                  defaultMessage: 'Recharger la liste des tests',
                }
          )}
        >
          <span
            className={`refresh codicon ${reload ? 'codicon-loading codicon-modifier-spin' : 'codicon-refresh'}`}
            onClick={(event) => {
              event.preventDefault();
              refresh();
            }}
          />
        </div>
      </div>
      {filteredTests === undefined ? (
        <Loading size="medium" />
      ) : (
        <TestsGrid
          vscode={vscode}
          filtered={filteredTests}
          grid={grid}
          filterScope={filterScope}
          onRun={onRun}
        />
      )}
    </div>
  );
}
