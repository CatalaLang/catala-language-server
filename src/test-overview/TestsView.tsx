import { type ReactElement } from 'react';
import { FormattedMessage } from 'react-intl';

import { type WebviewApi } from 'vscode-webview';

import { AddNewTest } from './buttons';
import { CardGrid } from './CardView';
import { TestList } from './ListView';
import type { TestMacro } from './types';

type TestGridArg = {
  vscode: WebviewApi<unknown>;
  filtered: TestMacro[];
  grid: boolean;
  filterScope: string[];
  onRun: (id: number) => void;
};

export function TestsGrid({
  vscode,
  filtered,
  grid,
  filterScope,
  onRun,
}: TestGridArg): ReactElement {
  if (filtered == undefined || filtered.length == 0) {
    return (
      <div className="no-tests">
        <span className="info-text">
          <FormattedMessage
            id="generalTests.noTestsFound"
            defaultMessage="Aucun test trouvé"
          />
        </span>
        <AddNewTest vscode={vscode} />
        <span className="help-text">
          <FormattedMessage
            id="generalTests.maybeClerkStart"
            defaultMessage="Si vous avez des tests dans votre projet mais que ce message persiste cela peut être dû à une installation incomplète de clerk, lancer la commande clerk start à la racine de votre projet pourrait résoudre ce problème"
          />
        </span>
      </div>
    );
  }

  return grid ? (
    <CardGrid
      filteredScope={filterScope}
      vscode={vscode}
      tests={filtered}
      onRun={onRun}
    />
  ) : (
    <TestList
      filteredScope={filterScope}
      vscode={vscode}
      tests={filtered}
      onRun={onRun}
    />
  );
}
