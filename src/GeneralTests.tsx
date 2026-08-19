import {
  createContext,
  Fragment,
  useContext,
  useEffect,
  useLayoutEffect,
  useRef,
  useState,
  type ReactElement,
} from 'react';
import { FormattedMessage, useIntl } from 'react-intl';

import { type WebviewApi } from 'vscode-webview';

import type { TestDebugger } from './generated/catala_types';
import { readDownMessage, writeUpMessage } from './generated/catala_types';
import { Box, Checkbox, FormControlLabel, Grid } from '@mui/material';
import { VscodeTextfield } from '@vscode-elements/react-elements';
import { assertUnreachable, splitOnTerms } from './shared/util';
import { setVsCodeApi } from './shared/webviewApi';

type FilteredTests = {
  test: TestMacro;
  index: number;
}[];

type TestGridArg = {
  vscode: WebviewApi<unknown>;
  filtered: FilteredTests;
  grid: boolean;
  filterScope: string[];
  orderFailure: boolean;
  onRun: (id: number) => void;
};

type GeneralTestsArg = {
  vscode: WebviewApi<unknown>;
};

type TestState =
  | { state: 'Success' }
  | { state: 'Loading' }
  | { state: 'Failed' }
  | { state: 'Unknown' };

type TestMacro = TestDebugger & TestState;

type TestItemArg = {
  vscode: WebviewApi<unknown>;
  test: TestMacro;
  num: number;
  onRun: (id: number) => void;
};

type Filters = [string, boolean][];
/**
 * Type to build the Filter component with filter on Scope, on description and
 * wether the test is a Catala Test Case editor generated test
 */
type FilterArg = {
  tests: TestMacro[] | undefined;
  filters: Filters;
  filterScope: string[];
  setFilterScope: React.Dispatch<React.SetStateAction<string[]>>;
  addFilter: (filter: string, include: boolean) => void;
  removeFilter: (filter: string, include: boolean) => void;
  removeAllFilter: () => void;
  // What is currently typed: it filters live, and can be pinned on top.
  searchBar: string;
  setSearchBar: React.Dispatch<React.SetStateAction<string>>;
  filterGui: boolean;
  setFilterGui: React.Dispatch<React.SetStateAction<boolean>>;
  orderFailure: boolean;
  setOrder: React.Dispatch<React.SetStateAction<boolean>>;
};

type ScopeFilterArg = {
  tests: FilteredTests | undefined;
  filterScope: string[];
  setFilterScope: React.Dispatch<React.SetStateAction<string[]>>;
};

/**
 * Component to run a bunch of tests, the run function is given in argument
 * so that we can choose the function to give
 */
function RunAllTests({
  onRun,
}: {
  className?: string | undefined;
  onRun: () => void;
}): ReactElement {
  return (
    <span
      onClick={(event) => {
        event.preventDefault();
        onRun();
      }}
      className="vscode-button"
    >
      <FormattedMessage
        id="generalTests.runAllTests"
        defaultMessage="Lancer les tests"
      />
    </span>
  );
}

/**
 * Component that open a new window to create a catala file and generates a test
 * in it.
 */
function AddNewTest({ vscode }: { vscode: WebviewApi<unknown> }): ReactElement {
  return (
    <span
      className="vscode-button"
      onClick={(event) => {
        event.preventDefault();
        vscode.postMessage(
          writeUpMessage({
            kind: 'OpenTestScopePicker',
          })
        );
      }}
    >
      <FormattedMessage
        id="generalTests.addTest"
        defaultMessage="Ajouter un test"
      />
    </span>
  );
}

function SeparationLine(): ReactElement {
  return <div className="separation-line" />;
}

/**
 * Return the correct symbol depending on a state, usually the
 * given state comes from an object TestMacro.
 * The state is only conveyed by the shape and the color of the icon, so it is
 * also spelled out in a tooltip.
 */
function TestStateIcon({ success }: { success: TestState }): ReactElement {
  const intl = useIntl();
  switch (success.state) {
    case 'Success':
      return (
        <span
          title={intl.formatMessage({
            id: 'generalTests.tooltip.stateSuccess',
            defaultMessage: 'Test réussi',
          })}
          className="codicon codicon-check-all check-icon"
          style={{ color: 'darkgreen', fontSize: '1.5em' }}
        />
      );
    case 'Failed':
      return (
        <span
          title={intl.formatMessage({
            id: 'generalTests.tooltip.stateFailed',
            defaultMessage: 'Test échoué',
          })}
          className="codicon codicon-error wrong-icon"
          style={{ color: 'darkred', fontSize: '1.5em' }}
        />
      );
    case 'Loading':
      return (
        <span
          title={intl.formatMessage({
            id: 'generalTests.tooltip.stateLoading',
            defaultMessage: 'Test en cours…',
          })}
          className="codicon codicon-loading codicon-modifier-spin"
          style={{ fontSize: '1.5em' }}
        />
      );
    case 'Unknown':
      return (
        <span
          title={intl.formatMessage({
            id: 'generalTests.tooltip.stateUnknown',
            defaultMessage: 'Test jamais lancé',
          })}
          className="codicon codicon-question"
        />
      );
    default:
      return assertUnreachable(success);
  }
}

/**
 * The component RunIcon is a little icon Run to run a test on a Catala file
 * A className can be given in parameter so that we can reuse the same component
 * in different case.
 */
function RunIcon({
  className,
  onRun,
}: {
  className?: string | undefined;
  onRun: () => void;
}): ReactElement {
  const intl = useIntl();
  return (
    <span
      title={intl.formatMessage({
        id: 'generalTests.tooltip.runTest',
        defaultMessage: 'Lancer le test',
      })}
      onClick={(event) => {
        event.preventDefault();
        onRun();
      }}
      className={`codicon codicon-debug-start ${className}`}
    />
  );
}

/**
 * Component that represents a button to open the Catala Test Case editor
 * on a Catala file with a test.
 */
function OpenGUI({
  vscode,
  filename,
}: {
  vscode: WebviewApi<unknown>;
  filename: string;
}): ReactElement {
  const intl = useIntl();
  return (
    <span
      title={intl.formatMessage({
        id: 'generalTests.tooltip.openGui',
        defaultMessage: "Ouvrir l'éditeur Catala",
      })}
      onClick={(event) => {
        event.preventDefault();
        vscode.postMessage(
          writeUpMessage({ kind: 'OpenInTestEditor', value: filename })
        );
      }}
      className="codicon codicon-eye open-gui"
    />
  );
}

/**
 * Component that represents a button to open a Catala file with a test
 * This button is here for Catala files with test but are not generated with
 * the Catala test case editor
 */
function OpenTextEditor({
  vscode,
  filename,
}: {
  vscode: WebviewApi<unknown>;
  filename: string;
}): ReactElement {
  const intl = useIntl();
  return (
    <span
      title={intl.formatMessage({
        id: 'generalTests.tooltip.openTextEditor',
        defaultMessage: "Ouvrir l'éditeur de texte",
      })}
      onClick={(event) => {
        event.preventDefault();
        vscode.postMessage(
          writeUpMessage({
            kind: 'OpenInTextEditor',
            value: { value: filename },
          })
        );
      }}
      className="codicon codicon-go-to-file open-text"
    />
  );
}

/**
 * This is the component used in the Grid representation of the list of tests.
 * It has some logic on wether the test is a Catala Testcase test or not but it's
 * mostly items with css to render them poperly
 *
 */
function TestItem({ vscode, test, num, onRun }: TestItemArg): ReactElement {
  const intl = useIntl();
  return (
    <Box className="test-item">
      <div className="test-item-header">
        <b
          className="test-title"
          title={intl.formatMessage({
            id: 'generalTests.header.title',
            defaultMessage: 'Titre',
          })}
        >
          <HighlightedText text={testTitle(test)} />
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
            values={{ num: <HighlightedText text={(num + 1).toString()} /> }}
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
        <HighlightedText text={testDescription(test)} />
      </span>
      <SeparationLine />
      <div className="footer">
        <TestStateIcon success={test} />
        <span
          title={intl.formatMessage({
            id: 'generalTests.header.lastTestDate',
            defaultMessage: 'Date du dernier test',
          })}
        >
          <FormattedMessage
            id="generalTests.testedOn"
            defaultMessage="Testé le {date}"
            values={{
              date: test.date ?? '??/??/????',
            }}
          />
        </span>
        {isGui(test) ? (
          <OpenGUI vscode={vscode} filename={test.filename} />
        ) : (
          <OpenTextEditor vscode={vscode} filename={test.filename} />
        )}
        <RunIcon className="run-icon" onRun={() => onRun(num)} />
      </div>
    </Box>
  );
}

/**
 * This function tells if the rendered span overflows or not,
 * it's useful to know if we want to display an icon to expand or not the view
 * @param event represent the current span
 * @returns if the span overflow
 */
function isOverflowActive(event: HTMLSpanElement): boolean {
  return (
    event.offsetHeight < event.scrollHeight ||
    event.offsetWidth < event.scrollWidth
  );
}

/**
 * The component to represent a Line in the table representation of the tests,
 * it has some logic due to the description that can overflow
 */
function TestLine({
  vscode,
  test,
  num,
  onRun,
}: TestItemArg & { expected: string[] }): ReactElement {
  // This textRef is used on the description span, it will be set when
  // the span is rendered so that we can know if the span overflows or not
  const textRef = useRef<HTMLSpanElement>(null);
  const [overflowActive, setOverflowActive] = useState(false);
  // Tells if the description span is expanded or not if it is change some class
  // and change the icon next to the text
  let [expanded, setExpanded] = useState<boolean>(false);

  const description = testDescription(test);

  // A new description invalidates the previous expansion state
  useEffect(() => setExpanded(false), [description]);

  // Set the overflow active if the currently rendered span overflows.
  // This has to be re-measured every time the text changes (hence the
  // `description` dependency: the ref object itself never changes identity, so
  // depending on it would only ever run this once) and every time the column is
  // resized (hence the ResizeObserver).
  // While expanded the clamping class is removed, so a measure would always
  // report "no overflow": skip it and keep the last known value.
  useLayoutEffect(() => {
    const span = textRef.current;
    if (span == null || expanded) {
      return;
    }

    const measure = (): void => setOverflowActive(isOverflowActive(span));
    measure();

    const observer = new ResizeObserver(measure);
    observer.observe(span);
    return (): void => observer.disconnect();
  }, [description, expanded]);

  return (
    <tr>
      <th>
        <a
          href=""
          title={test.filename}
          onClick={(event) => {
            event.preventDefault();
            vscode.postMessage(
              writeUpMessage(
                isGui(test)
                  ? { kind: 'OpenInTestEditor', value: test.filename }
                  : {
                      kind: 'OpenInTextEditor',
                      value: { value: test.filename },
                    }
              )
            );
          }}
        >
          <HighlightedText text={(num + 1).toString()} />
        </a>
      </th>
      <td>
        <HighlightedText text={testTitle(test)} />
      </td>
      <td>
        <HighlightedText text={testingScope(test)} />
      </td>
      <td
        className={overflowActive ? `descr-column` : ''}
        onClick={(event) => {
          if (overflowActive) {
            let selection = window.getSelection()?.toString();
            if (selection == undefined || selection.length == 0) {
              event.preventDefault();
              setExpanded((oldExpanded) => !oldExpanded);
            }
          }
        }}
      >
        <span
          ref={textRef}
          className={`test-descr ${expanded ? 'text' : 'test-descr-hidden'}`}
        >
          <HighlightedText text={description} />
        </span>
        {overflowActive && (
          <span
            className={`codicon codicon-fold-${expanded ? 'up' : 'down'}`}
          />
        )}
      </td>
      <td>{test.date ?? '??/??/????'}</td>
      <td>
        <TestStateIcon success={test} />
      </td>
      <td>
        <RunIcon className="run-icon" onRun={() => onRun(num)} />
      </td>
      <td>
        {isGui(test) ? (
          <OpenGUI vscode={vscode} filename={test.filename} />
        ) : (
          <OpenTextEditor vscode={vscode} filename={test.filename} />
        )}
      </td>
    </tr>
  );
}

function HeaderLine({
  expected,
  gui,
}: {
  expected?: string[] | undefined;
  gui: boolean;
}): ReactElement {
  return (
    <thead>
      <tr>
        <th>
          <FormattedMessage
            id="generalTests.header.id"
            defaultMessage="Numéro du test"
          />
        </th>
        <td>
          <FormattedMessage
            id="generalTests.header.title"
            defaultMessage="Titre"
          />
        </td>
        <td>
          <FormattedMessage
            id="generalTests.header.scope"
            defaultMessage="Champ d'application"
          />
        </td>
        <td>
          <FormattedMessage
            id="generalTests.header.description"
            defaultMessage="Description"
          />
        </td>
        {expected?.map((value) => <td>{value}</td>)}
        <td>
          <FormattedMessage
            id="generalTests.header.lastTestDate"
            defaultMessage="Date du dernier test"
          />
        </td>
        <td>
          <FormattedMessage
            id="generalTests.header.testResult"
            defaultMessage="Résultat du test"
          />
        </td>
        <td>
          <RunIcon onRun={() => {}} />
        </td>
        <td>
          <FormattedMessage
            id={gui ? 'generalTests.header.gui' : 'generalTests.header.editor'}
          />
        </td>
      </tr>
    </thead>
  );
}

// The `gui` flag is now carried by the `test` union's constructor: a GUI
// entry wraps a full `Test`, a plain entry wraps a `TestSum`. These helpers
// read the fields that both variants share, plus the run metadata that lives
// on the `TestDebugger` wrapper.
function isGui(test: TestDebugger): boolean {
  return test.test.kind === 'GUI';
}

function testTitle(test: TestDebugger): string {
  return test.test.kind === 'GUI'
    ? (test.test.value.title ?? '')
    : test.test.value.scope;
}

function testDescription(test: TestDebugger): string {
  return test.test.kind == 'GUI'
    ? (test.test.value.description ?? '')
    : `Test of ${test.test.value.scope} in ${test.filename}`;
}

function testingScope(test: TestDebugger): string {
  return test.test.kind == 'GUI'
    ? test.test.value.scope_tested
    : test.test.value.scope;
}

function testMacro(test: TestDebugger): TestMacro {
  if (test.success == undefined) {
    return {
      ...test,
      state: 'Unknown',
    };
  } else {
    return {
      ...test,
      state: test.success ? 'Success' : 'Failed',
    };
  }
}

/**
 * Function to verify if a test passes all the filters
 * @param test checks the filters on this test
 * @param index the index of the test so that the user
 * can also search on the index
 * @param filterBar the content of the search bar
 * @param filterScope the different Scopes that are filtered
 * @param filterGui tells if we want tests that are generated from the
 * testcase editor
 * @returns true if the tests matches conditions depending on the filters
 */
function matchFilter(
  test: TestDebugger,
  index: number,
  filterBar: Filters,
  filterScope: string[],
  filterGui: boolean
): boolean {
  let searchBarFilter = true;
  for (let [filterRaw, include] of filterBar) {
    let filter = filterRaw.toLowerCase();
    let includesFilter =
      testTitle(test).toLowerCase().includes(filter) ||
      testDescription(test).toLowerCase().includes(filter) ||
      testingScope(test).toLowerCase().includes(filter) ||
      (index + 1).toString().includes(filter);
    let currentFilter = include ? includesFilter : !includesFilter;
    searchBarFilter = searchBarFilter && currentFilter;
  }
  let scopeFilter =
    filterScope.length == 0
      ? true
      : filterScope.some((value) => testingScope(test) == value);
  let guiFilter = filterGui ? isGui(test) : true;
  return searchBarFilter && scopeFilter && guiFilter;
}

/**
 * The filters currently applied to the list, shared with the whole tree so that
 * any displayed text can highlight what the user searched for, without having
 * to thread the filters through every intermediate component.
 */
const FiltersContext = createContext<Filters>([]);

/**
 * Renders `text`, wrapping every substring matched by a filter in a `<mark>` so
 * that the user sees what made the test show up. Matching is case insensitive,
 * like in `matchFilter`.
 * Only inclusion filters are considered: a test that survived an exclusion
 * filter cannot contain the excluded term in the first place.
 */
function HighlightedText({ text }: { text: string }): ReactElement {
  const filters = useContext(FiltersContext);
  const terms = filters.filter(([, include]) => include).map(([term]) => term);
  return (
    <>
      {splitOnTerms(text, terms).map((chunk, index) =>
        chunk.match ? (
          <mark key={index} className="filter-match">
            {chunk.text}
          </mark>
        ) : (
          chunk.text
        )
      )}
    </>
  );
}

type OriginalTest = { index: number; test: TestMacro };

type CardGridArg = {
  vscode: WebviewApi<unknown>;
  filteredScope: string[];
  tests: OriginalTest[];
  onRun: (id: number) => void;
};

function CardGrid({
  vscode,
  tests,
  filteredScope,
  onRun,
}: CardGridArg): ReactElement {
  let gridTests = new Map<string, OriginalTest[]>();
  if (filteredScope.length != 0) {
    for (let index = 0; index < tests.length; index++) {
      const elt = tests[index];
      let scopeFiltered = testingScope(elt.test);
      let scopeTested = gridTests.get(scopeFiltered) ?? [];
      scopeTested.push(elt);
      gridTests.set(scopeFiltered, scopeTested);
    }
    return (
      <Grid container spacing={4}>
        {Array.from(gridTests.entries()).map(([scope, tests]) => (
          <>
            <Grid size={3}>
              <h2 style={{ overflowWrap: 'anywhere' }}>{scope}</h2>
              <h3>
                <FormattedMessage
                  id="generalTests.associatedTests"
                  defaultMessage="{count} tests associés"
                  values={{ count: tests.length }}
                />
              </h3>
            </Grid>
            <Grid container size={9} spacing={2} columns={3}>
              {tests.map((elt, index) => (
                <Grid key={index} size={1}>
                  <div style={{ fontSize: '8px', height: '100%' }}>
                    <TestItem
                      vscode={vscode}
                      test={elt.test}
                      num={elt.index}
                      onRun={onRun}
                    />
                  </div>
                </Grid>
              ))}
            </Grid>
          </>
        ))}
      </Grid>
    );
  } else {
    return (
      <Grid container spacing={4} columns={{ xs: 1, sm: 3, md: 4 }}>
        {tests.map((elt, index) => (
          <Grid key={index} size={1}>
            <div style={{ fontSize: '8px', height: '100%' }}>
              <TestItem
                vscode={vscode}
                test={elt.test}
                num={elt.index}
                onRun={onRun}
              />
            </div>
          </Grid>
        ))}
      </Grid>
    );
  }
}

type SortingTest = { index: number; success: boolean | undefined };

function sortTests(test_a: SortingTest, test_b: SortingTest): number {
  let scale = (success: boolean | undefined): number => {
    if (success == undefined) {
      return 1;
    } else if (success) {
      return 0;
    } else {
      return 2;
    }
  };
  let a_scale = scale(test_a.success);
  let b_scale = scale(test_b.success);
  let failures = b_scale - a_scale;
  return failures != 0 ? failures : test_a.index - test_b.index;
}

function TestList({
  vscode,
  onRun,
  tests,
  orderFailure,
  filteredScope,
}: CardGridArg & { orderFailure: boolean }): ReactElement {
  let map = new Map<string, [OriginalTest[], OriginalTest[], OriginalTest[]]>();
  let not_gui: OriginalTest[] = [];
  for (let index = 0; index < tests.length; index++) {
    const element = tests[index];
    if (element.test.test.kind == 'GUI') {
      let scope = element.test.test.value.scope_tested;
      let [fList, uList, sList] = map.get(scope) ?? [[], [], []];
      if (orderFailure && element.test.success == undefined) {
        uList.push(element);
      } else if (orderFailure && element.test.success) {
        sList.push(element);
      } else {
        fList.push(element);
      }
      map.set(scope, [fList, uList, sList]);
    } else {
      not_gui.push(element);
    }
  }
  if (orderFailure) {
    not_gui = not_gui.sort((test_a, test_b) => {
      let a = { index: test_a.index, success: test_a.test.success };
      let b = { index: test_b.index, success: test_b.test.success };
      return sortTests(a, b);
    });
  }

  let allTests: [string, OriginalTest[]][] = [];
  if (filteredScope.length == 0) {
    let list = [...map.values()].flat().flat();
    if (orderFailure) {
      list = list.sort((test_a, test_b) => {
        let a = { index: test_a.index, success: test_a.test.success };
        let b = { index: test_b.index, success: test_b.test.success };
        return sortTests(a, b);
      });
    }
    allTests = [['Tests', list]];
  } else {
    let list = [...map.entries()].map((value) => {
      let res: [string, OriginalTest[]] = [value[0], value[1].flat().flat()];
      return res;
    });
    allTests = list;
  }
  return (
    <>
      {allTests.map(([testedScope, tests]) => {
        return (
          <Fragment key={testedScope}>
            <h1>{testedScope}</h1>
            <table className="test-list">
              <HeaderLine gui={true} />
              <tbody>
                {tests.map(({ test, index }) => (
                  <TestLine
                    key={index}
                    vscode={vscode}
                    test={test}
                    num={index}
                    onRun={onRun}
                    expected={[]}
                  />
                ))}
              </tbody>
            </table>
          </Fragment>
        );
      })}
      {not_gui.length > 0 ? (
        <>
          <h1>Autres Tests</h1>
          <table className="test-list">
            <HeaderLine gui={false} />
            <tbody>
              {not_gui.map(({ test, index }) => {
                return (
                  <TestLine
                    key={index}
                    vscode={vscode}
                    test={test}
                    num={index}
                    onRun={onRun}
                    expected={[]}
                  />
                );
              })}
            </tbody>
          </table>
        </>
      ) : null}
    </>
  );
}

function TestsGrid({
  vscode,
  filtered,
  grid,
  filterScope,
  onRun,
  orderFailure,
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
      orderFailure={orderFailure}
      filteredScope={filterScope}
      vscode={vscode}
      tests={filtered}
      onRun={onRun}
    />
  );
}

function scopesFromTests(tests: FilteredTests): string[] {
  let allScopes = tests?.map((test) => testingScope(test.test)).sort();
  let scopes = [];
  let prev = '';
  for (let index = 0; index < allScopes!.length; index++) {
    const element = allScopes![index];
    if (prev != element) {
      scopes.push(element);
      prev = element;
    }
  }
  return scopes;
}

function ScopeFilter({
  tests,
  filterScope,
  setFilterScope,
}: ScopeFilterArg): ReactElement {
  const intl = useIntl();
  let filteredScope = scopesFromTests(tests ?? []);
  return (
    <div className="pins">
      <Box
        sx={{ display: 'flex', flexWrap: 'wrap', gap: 1, textAlign: 'center' }}
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
      </Box>
    </div>
  );
}

type FilterPinArg = {
  removeFilter: (filter: string, include: boolean) => void;
  filter: string;
  include: boolean;
};

function FilterPin({
  removeFilter,
  filter,
  include,
}: FilterPinArg): ReactElement {
  const intl = useIntl();
  // Both branches keep their descriptor inline so that the message ids stay
  // statically extractable
  const tooltip = include
    ? intl.formatMessage(
        {
          id: 'generalTests.filterPin.inclusion',
          defaultMessage: 'Je veux que "{filter}" apparaisse dans le test',
        },
        { filter }
      )
    : intl.formatMessage(
        {
          id: 'generalTests.filterPin.exclusion',
          defaultMessage:
            'Je ne veux pas que "{filter}" apparaisse dans le test',
        },
        { filter }
      );
  return (
    <div
      style={{ display: 'flex', alignItems: 'center' }}
      title={tooltip}
      className={`filter-pin ${include ? 'inclusion-filter' : 'exclusion-filter'}`}
    >
      <span>{filter}</span>
      <span
        style={{ cursor: 'pointer' }}
        className="codicon codicon-close"
        onClick={(event) => {
          event.preventDefault();
          removeFilter(filter, include);
        }}
      />
    </div>
  );
}

function FilterPins({
  removeFilter,
  filters,
}: {
  removeFilter: (filter: string, include: boolean) => void;
  filters: Filters;
}): ReactElement | null {
  return filters.length == 0 ? null : (
    <div className="pins" style={{ width: '30em' }}>
      <Box
        sx={{ display: 'flex', flexWrap: 'wrap', gap: 1, textAlign: 'center' }}
      >
        {Array.from(filters).map(([filter, include], index) => (
          <FilterPin
            key={index}
            removeFilter={removeFilter}
            filter={filter}
            include={include}
          />
        ))}
      </Box>
    </div>
  );
}

function Filter({
  tests,
  filters,
  filterScope,
  setFilterScope,
  addFilter,
  removeFilter,
  removeAllFilter,
  filterGui,
  setFilterGui,
  orderFailure,
  setOrder,
  searchBar,
  setSearchBar,
}: FilterArg): ReactElement {
  const intl = useIntl();
  // Restore the default state: GUI-only checkbox checked, no scope selected,
  // empty search bar.
  const resetFilters = (): void => {
    setFilterGui(true);
    setFilterScope([]);
    removeAllFilter();
    setSearchBar('');
  };

  let valueSearch = searchBar.trim();

  const filteredTests = tests
    ?.map((test, index) => ({ test, index }))
    .filter(({ test, index }) =>
      matchFilter(test, index, filters, [], filterGui)
    );

  // Pin what is typed, then clear the field so several filters can be chained.
  const pinSearch = (include: boolean): void => {
    if (valueSearch == '') return;
    addFilter(valueSearch, include);
    setSearchBar('');
  };
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
            <FormControlLabel
              control={
                <Checkbox
                  checked={filterGui}
                  onChange={(event) => setFilterGui(event.target.checked)}
                  sx={{
                    color: 'gray',
                    '&.Mui-checked': { color: 'lightgray' },
                  }}
                />
              }
              label={
                <FormattedMessage
                  id="generalTests.guiOnly"
                  defaultMessage="Tests GUI uniquement"
                />
              }
              sx={{ '.MuiFormControlLabel-label': { color: 'gray' } }}
            />
            <FormControlLabel
              control={
                <Checkbox
                  checked={orderFailure}
                  onChange={(event) => setOrder(event.target.checked)}
                  sx={{
                    color: 'gray',
                    '&.Mui-checked': { color: 'lightgray' },
                  }}
                />
              }
              label={
                <FormattedMessage
                  id="generalTests.highlightFails"
                  defaultMessage="Mettre en avant les tests échoués"
                />
              }
              sx={{ '.MuiFormControlLabel-label': { color: 'gray' } }}
            />
          </div>
          <ScopeFilter
            tests={filteredTests}
            filterScope={filterScope}
            setFilterScope={setFilterScope}
          />
          <div style={{ marginLeft: 'auto' }}>
            <FilterPins removeFilter={removeFilter} filters={filters} />
            <VscodeTextfield
              className="search-bar"
              value={searchBar}
              placeholder={intl.formatMessage({
                id: 'generalTests.searchPlaceholder',
                defaultMessage: 'Rechercher un test…',
              })}
              onInput={(e) => {
                const value = (e.target as HTMLInputElement).value;
                setSearchBar(value);
              }}
              onKeyDown={(e) => {
                if (e.key === 'Enter') {
                  e.preventDefault();
                  pinSearch(true);
                }
              }}
            >
              <span className="codicon codicon-search" slot="content-before" />
              <span
                title={intl.formatMessage(
                  {
                    id: 'generalTests.filterBar.inclusion',
                    defaultMessage: 'Inclure {filter} dans la recherche',
                  },
                  { filter: valueSearch == '' ? '' : `"${valueSearch}"` }
                )}
                style={{ cursor: 'pointer' }}
                className="codicon codicon-save"
                slot="content-after"
                // Keep the focus in the field so that the user can chain
                // several filters without clicking back into it.
                onMouseDown={(event) => event.preventDefault()}
                onClick={(event) => {
                  event.preventDefault();
                  pinSearch(true);
                }}
              />
              <span
                title={intl.formatMessage(
                  {
                    id: 'generalTests.filterBar.exclusion',
                    defaultMessage: 'Exclure {filter} de la recherche',
                  },
                  { filter: valueSearch == '' ? '' : `"${valueSearch}"` }
                )}
                style={{ cursor: 'pointer' }}
                className="codicon codicon-circle-slash"
                slot="content-after"
                onMouseDown={(event) => event.preventDefault()}
                onClick={(event) => {
                  event.preventDefault();
                  pinSearch(false);
                }}
              />
            </VscodeTextfield>
          </div>
        </>
      )}
    </div>
  );
}

function Loading({
  size,
}: {
  size: 'small' | 'medium' | 'large' | undefined;
}): ReactElement {
  const fontSize = size === 'small' ? '2em' : size === 'large' ? '6em' : '4em';
  return (
    <div
      style={{
        display: 'flex',
        margin: 'auto',
      }}
    >
      <span
        className="codicon codicon-loading codicon-modifier-spin"
        style={{
          fontSize,
          alignItems: 'center',
          justifyContent: 'center',
        }}
      />
    </div>
  );
}

function noFilter(
  filter: Filters,
  filterScope: string[],
  filterGui: boolean
): boolean {
  return filter.length == 0 && filterScope.length == 0 && filterGui == false;
}

// Total duration of the `highlight` blink animation (`blink 1s ... 4` = 1s ×
// 4 iterations), plus a small margin so the timer settles just after the
// animation has visually finished.
// const HIGHLIGHT_MS = 4100;

export default function GeneralTests({
  vscode,
}: GeneralTestsArg): ReactElement {
  const intl = useIntl();
  const [filter, setFilter] = useState<Filters>([]);
  const [filterScope, setFilterScope] = useState<string[]>([]);
  const [filterGui, setFilterGui] = useState<boolean>(true);
  const [orderFailure, setOrderFails] = useState<boolean>(true);
  const [grid, setGrid] = useState<boolean>(true);
  const [tests, setTests] = useState<TestMacro[] | undefined>(undefined);
  const [reload, setReload] = useState<boolean>(false);
  const [searchBar, setSearchBar] = useState<string>('');

  // These three helpers must always hand a *new* array to setFilter: React
  // bails out of the re-render when the updater returns the very same reference,
  // so mutating the current state in place would leave the view one step behind.
  const addFilter = (filter: string, include: boolean): void => {
    setFilter((oldFilter) =>
      oldFilter.some(([term, inc]) => term == filter && inc == include)
        ? oldFilter
        : [...oldFilter, [filter, include]]
    );
  };

  const removeAllFilter = (): void => {
    setFilter([]);
  };

  const removeFilter = (filter: string, include: boolean): void => {
    setFilter((oldFilter) =>
      oldFilter.filter(([current, currentInclude]) => {
        return current != filter || include != currentInclude;
      })
    );
  };

  useEffect(() => {
    setVsCodeApi(vscode);
  }, [vscode]);

  useEffect(() => {
    const handleMessage = (event: MessageEvent): void => {
      const message = readDownMessage(event.data);
      switch (message.kind) {
        case 'AllTests': {
          setReload(false);
          let tests = message.value;
          let tsTests: TestMacro[] = [];
          for (let index = 0; index < tests.length; index++) {
            const test = tests[index];
            tsTests.push(testMacro(test));
          }
          setTests(tsTests);
          break;
        }
        case 'TestRunResults': {
          break;
        }
        case 'Update': {
          /* TODO */
          break;
        }
        case 'TestScopeResult': {
          let [result, run, id] = message.value;
          setTests((oldTests) =>
            oldTests?.map((test, index) => {
              if (index != id) {
                return test;
              }
              let updatedTest: TestDebugger = {
                filename: test.filename,
                test: result,
                success: run.success,
                date: run.date,
              };
              return testMacro(updatedTest);
            })
          );
          // if (!run.success) {
          //   scheduleSettle(id);
          // }
          break;
        }
        default:
          break;
      }
    };

    window.addEventListener('message', handleMessage);

    // Cleanup function to remove event listener
    return (): void => {
      window.removeEventListener('message', handleMessage);
    };
  }, []);

  const onRun = (id: number): void => {
    if (!tests) {
      return;
    }
    setTests((oldTests) =>
      oldTests?.map((test, index) =>
        index === id ? { ...test, state: 'Loading' } : test
      )
    );
    vscode.postMessage(
      writeUpMessage({ kind: 'SpecificTestRequest', value: [id] })
    );
  };

  const filteredTests = tests
    ?.map((test, index) => ({ test, index }))
    .filter(({ test, index }) =>
      matchFilter(test, index, filter, filterScope, filterGui)
    );

  const sortedTests = orderFailure
    ? filteredTests?.sort((test_a, test_b) => {
        let a = { index: test_a.index, success: test_a.test.success };
        let b = { index: test_b.index, success: test_b.test.success };
        return sortTests(a, b);
      })
    : filteredTests;

  return (
    <FiltersContext.Provider value={filter}>
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
              onRun={() => {
                if (tests) {
                  if (noFilter(filter, filterScope, filterGui)) {
                    setTests((oldTests) =>
                      oldTests?.map((test) => {
                        return { ...test, state: 'Loading' };
                      })
                    );
                    vscode.postMessage(
                      writeUpMessage({ kind: 'SpecificTestRequest', value: [] })
                    );
                  } else if (sortedTests) {
                    let indexes = sortedTests.map(({ index }) => index);
                    setTests((oldTests) => {
                      return oldTests?.map((test, index) => {
                        let loading = indexes.includes(index);
                        return loading ? { ...test, state: 'Loading' } : test;
                      });
                    });
                    vscode.postMessage(
                      writeUpMessage({
                        kind: 'SpecificTestRequest',
                        value: indexes,
                      })
                    );
                  }
                }
              }}
            />{' '}
          </div>
        </div>
        <Filter
          tests={tests}
          addFilter={addFilter}
          removeFilter={removeFilter}
          removeAllFilter={removeAllFilter}
          setFilterScope={setFilterScope}
          filterScope={filterScope}
          filterGui={filterGui}
          setFilterGui={setFilterGui}
          orderFailure={orderFailure}
          setOrder={setOrderFails}
          filters={filter}
          searchBar={searchBar}
          setSearchBar={setSearchBar}
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
                setReload(true);
                vscode.postMessage(writeUpMessage({ kind: 'Reload' }));
              }}
            />
          </div>
        </div>
        {sortedTests === undefined ? (
          <Loading size="medium" />
        ) : (
          <TestsGrid
            vscode={vscode}
            filtered={sortedTests}
            grid={grid}
            filterScope={filterScope}
            orderFailure={orderFailure}
            onRun={onRun}
          />
        )}
      </div>
    </FiltersContext.Provider>
  );
}
