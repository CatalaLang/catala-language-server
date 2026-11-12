import { Fragment, type ReactElement } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';

import { OpenGUI, OpenTextEditor, RunIcon } from './buttons';
import { isGui, testDescription, testingScope, testTitle } from './testUtils';
import type { CardGridArg, TestItemArg, TestMacro } from './types';
import { SeparationLine, TestDate, TestStateIcon } from './widgets';

/**
 * This is the component used in the Grid representation of the list of tests.
 * It has some logic on wether the test is a Catala Testcase test or not but it's
 * mostly items with css to render them poperly
 *
 */
function TestItem({ vscode, test, onRun }: TestItemArg): ReactElement {
  const intl = useIntl();
  return (
    <div className="test-item">
      <div className="test-item-header">
        <b
          className="test-title"
          title={intl.formatMessage({
            id: 'generalTests.header.title',
            defaultMessage: 'Titre',
          })}
        >
          {testTitle(test)}
        </b>
        <span
          className="test-number"
          title={intl.formatMessage({
            id: 'generalTests.header.id',
            defaultMessage: 'Numéro du test',
          })}
        >
          <FormattedMessage
            id="generalTests.testNumber"
            defaultMessage="Test #{num}"
            values={{ num: test.index + 1 }}
          />
        </span>
      </div>
      <span
        className="test-descr"
        title={intl.formatMessage({
          id: 'generalTests.header.description',
          defaultMessage: 'Description',
        })}
      >
        {testDescription(test)}
      </span>
      <SeparationLine />
      <div className="footer">
        <TestStateIcon loading={test.loading} success={test.success} />
        <span
          title={intl.formatMessage({
            id: 'generalTests.header.lastTestDate',
            defaultMessage: 'Date du dernier test',
          })}
        >
          {test.date !== undefined ? (
            <FormattedMessage
              id="generalTests.testedOn"
              defaultMessage="Testé le {date}"
              values={{ date: <TestDate date={test.date} /> }}
            />
          ) : (
            <FormattedMessage
              id="generalTests.neverRun"
              defaultMessage="Jamais lancé"
            />
          )}
        </span>
        {isGui(test) ? (
          <OpenGUI vscode={vscode} filename={test.filename} />
        ) : (
          <OpenTextEditor vscode={vscode} filename={test.filename} />
        )}
        <RunIcon className="run-icon" onRun={() => onRun(test.index)} />
      </div>
    </div>
  );
}

export function CardGrid({
  vscode,
  tests,
  filteredScope,
  onRun,
}: CardGridArg): ReactElement {
  let gridTests = new Map<string, TestMacro[]>();
  if (filteredScope.length != 0) {
    for (let index = 0; index < tests.length; index++) {
      const elt = tests[index];
      let scopeFiltered = testingScope(elt);
      let scopeTested = gridTests.get(scopeFiltered) ?? [];
      scopeTested.push(elt);
      gridTests.set(scopeFiltered, scopeTested);
    }
    return (
      <div className="scope-grid">
        {Array.from(gridTests.entries()).map(([scope, tests]) => (
          <Fragment key={scope}>
            <div>
              <h2 style={{ overflowWrap: 'anywhere' }}>{scope}</h2>
              <h3>
                <FormattedMessage
                  id="generalTests.associatedTests"
                  defaultMessage="{count} tests associés"
                  values={{ count: tests.length }}
                />
              </h3>
            </div>
            <div className="card-grid scoped">
              {tests.map((elt) => (
                <div key={elt.index} className="card-cell">
                  <TestItem vscode={vscode} test={elt} onRun={onRun} />
                </div>
              ))}
            </div>
          </Fragment>
        ))}
      </div>
    );
  } else {
    return (
      <div className="card-grid">
        {tests.map((elt) => (
          <div key={elt.index} className="card-cell">
            <TestItem vscode={vscode} test={elt} onRun={onRun} />
          </div>
        ))}
      </div>
    );
  }
}
