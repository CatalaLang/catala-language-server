import { useEffect, useRef, useState, type ReactElement } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';

import { type WebviewApi } from 'vscode-webview';

import type { TestDebugger } from './generated/catala_types';
import { readDownMessage, writeUpMessage } from './generated/catala_types';
import { Box, Checkbox, FormControlLabel, Grid } from '@mui/material';
import { VscodeTextfield } from '@vscode-elements/react-elements';
import { assertUnreachable } from './shared/util';
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
  onRun: (id: number) => void;
};

type GeneralTestsArg = {
  vscode: WebviewApi<unknown>;
};

type TestState =
  | { state: 'Success' }
  | { state: 'Loading' }
  | { state: 'Failed' }
  | { state: 'JustFailed' }
  | { state: 'Unknown' };

type TestMacro = TestDebugger & TestState;

type TestItemArg = {
  vscode: WebviewApi<unknown>;
  test: TestMacro;
  num: number;
  onRun: (id: number) => void;
};

/**
 * Type to build the Filter component with filter on Scope, on description and
 * wether the test is a Catala Test Case editor generated test
 */
type FilterArg = {
  tests: FilteredTests | undefined;
  filter: string;
  filterScope: string[];
  setFilterScope: React.Dispatch<React.SetStateAction<string[]>>;
  setFilter: React.Dispatch<React.SetStateAction<string>>;
  filterGui: boolean;
  setFilterGui: React.Dispatch<React.SetStateAction<boolean>>;
};

type ScopeFilterArg = {
  tests: FilteredTests;
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
 * given state comes from an object TestMacro
 */
function testState(success: TestState): ReactElement {
  switch (success.state) {
    case 'Success':
      return (
        <span
          className="codicon codicon-check-all check-icon"
          style={{ color: 'darkgreen', fontSize: '1.5em' }}
        />
      );
    case 'Failed':
      return (
        <span
          className="codicon codicon-error wrong-icon"
          style={{ color: 'darkred', fontSize: '1.5em' }}
        />
      );
    case 'JustFailed':
      return (
        <span
          className="codicon codicon-error wrong-icon"
          style={{ color: 'darkred', fontSize: '1.5em' }}
        />
      );
    case 'Loading':
      return (
        <span
          className="codicon codicon-loading codicon-modifier-spin"
          style={{ fontSize: '1.5em' }}
        />
      );
    case 'Unknown':
      return <span className="codicon codicon-question" />;
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
  return (
    <span
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
  success,
}: {
  vscode: WebviewApi<unknown>;
  filename: string;
  success: TestState;
}): ReactElement {
  let fail = success.state == 'JustFailed';
  let [first, setFirst] = useState<boolean>(true);
  return (
    <span
      onAnimationEnd={(event) => {
        event.preventDefault();
        setFirst(false);
      }}
      onClick={(event) => {
        event.preventDefault();
        vscode.postMessage(
          writeUpMessage({ kind: 'OpenInTestEditor', value: filename })
        );
      }}
      className={`codicon codicon-eye open-gui ${fail && first ? 'highlight ' : ''}`}
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
  return (
    <span
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
  return (
    <Box
      className={`test-item${test.state == 'JustFailed' ? ' justFailed' : ''}`}
    >
      <div className="test-item-header">
        <b className="test-title">{testTitle(test)}</b>
        <span className="test-number">
          <FormattedMessage
            id="generalTests.testNumber"
            defaultMessage="Test #{num}"
            values={{ num: num + 1 }}
          />
        </span>
      </div>
      <span className="test-descr">{testDescription(test)}</span>
      <SeparationLine />
      <div className="footer">
        {testState(test)}
        <span>
          <FormattedMessage
            id="generalTests.testedOn"
            defaultMessage="Testé le {date}"
            values={{
              date: test.date ?? '??/??/????',
            }}
          />
        </span>
        {isGui(test) ? (
          <OpenGUI vscode={vscode} filename={test.filename} success={test} />
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

  // set the overflow active is the current rendered span overflows
  useEffect(() => {
    if (textRef.current != null && isOverflowActive(textRef.current!)) {
      setOverflowActive(true);
      return;
    }

    setOverflowActive(false);
  }, [isOverflowActive]);

  return (
    <tr className={test.state == 'JustFailed' ? 'justFailed' : ''}>
      <th>
        <a
          href=""
          onClick={(event) => {
            event.preventDefault();
            vscode.postMessage(
              writeUpMessage({ kind: 'OpenInTestEditor', value: test.filename })
            );
          }}
        >
          {num + 1}
        </a>
      </th>
      <td>{testingScope(test)}</td>
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
          {testDescription(test)}
        </span>
        {overflowActive && (
          <span
            className={`codicon codicon-fold-${expanded ? 'up' : 'down'}`}
          />
        )}
      </td>
      <td>{test.date ?? '??/??/????'}</td>
      <td>{testState(test)}</td>
      <td>
        <span
          className="codicon codicon-debug-start run-icon"
          onClick={(event) => {
            event.preventDefault();
            onRun(num);
          }}
        />
      </td>
      <td>
        {isGui(test) ? (
          <OpenGUI vscode={vscode} filename={test.filename} success={test} />
        ) : (
          <OpenTextEditor vscode={vscode} filename={test.filename} />
        )}
      </td>
    </tr>
  );
}

function HeaderLine({ expected }: { expected: string[] }): ReactElement {
  return (
    <thead>
      <tr>
        <th>
          <FormattedMessage id="generalTests.header.id" defaultMessage="Id" />
        </th>
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
        {expected.map((value) => (
          <td>{value}</td>
        ))}
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
          <FormattedMessage id="generalTests.header.gui" defaultMessage="GUI" />
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

function testMacro(test: TestDebugger, previousSuccess: boolean): TestMacro {
  return {
    ...test,
    state: test.success ? 'Success' : previousSuccess ? 'JustFailed' : 'Failed',
  };
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
  filterBar: string,
  filterScope: string[],
  filterGui: boolean
): boolean {
  let filter = filterBar.toLowerCase();
  let searchBarFilter =
    testTitle(test).toLowerCase().includes(filter) ||
    testDescription(test).toLowerCase().includes(filter) ||
    testingScope(test).toLowerCase().includes(filter) ||
    (index + 1).toString().includes(filter);
  let scopeFilter =
    filterScope.length == 0
      ? true
      : filterScope.some((value) => testingScope(test) == value);
  let guiFilter = filterGui ? isGui(test) : true;
  return searchBarFilter && scopeFilter && guiFilter;
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
              <h2 style={{ overflowX: 'auto' }}>{scope}</h2>
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

function TestList({ vscode, onRun, tests }: CardGridArg): ReactElement {
  let map = new Map<string, [Set<string>, OriginalTest[]]>();
  let not_gui: OriginalTest[] = [];
  for (let index = 0; index < tests.length; index++) {
    const element = tests[index];
    if (element.test.test.kind == 'GUI') {
      let scope = element.test.test.value.scope_tested;
      let [expected, scopeList] = map.get(scope) ?? [new Set<string>(), []];
      scopeList.push(element);
      map.set(scope, [expected, scopeList]);
    } else {
      not_gui.push(element);
    }
  }
  return (
    <>
      {[...map.entries()].map(([testedScope, [allExpected, tests]]) => {
        let expected = [...allExpected.keys()];
        return (
          <>
            <h1>{testedScope}</h1>
            <table className="test-list">
              <HeaderLine expected={expected} />
              <tbody>
                {tests.map(({ test, index }) => (
                  <TestLine
                    vscode={vscode}
                    test={test}
                    num={index}
                    onRun={onRun}
                    expected={expected}
                  />
                ))}
              </tbody>
            </table>
          </>
        );
      })}
      {not_gui.length > 0 ? (
        <>
          <h1>Autres Tests</h1>
          <table className="test-list">
            <HeaderLine expected={[]} />
            <tbody>
              {not_gui.map(({ test, index }) => {
                return (
                  <TestLine
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
}: TestGridArg): ReactElement {
  if (filtered == undefined || filtered.length == 0) {
    return (
      <div className="no-tests">
        <span>
          <FormattedMessage
            id="generalTests.noTestsFound"
            defaultMessage="Aucun test trouvé"
          />
        </span>{' '}
        <AddNewTest vscode={vscode} />
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

function scopesFromTests(tests: FilteredTests): string[] {
  let allScopes = tests?.map((test, _) => testingScope(test.test)).sort();
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
  let filteredScope = scopesFromTests(tests ?? []);
  return (
    <div className="scope-filter">
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

function Filter({
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
          <FormControlLabel
            control={
              <Checkbox
                checked={filterGui}
                onChange={(event) => setFilterGui(event.target.checked)}
                sx={{ color: 'gray', '&.Mui-checked': { color: 'lightgray' } }}
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
          <ScopeFilter
            tests={tests}
            filterScope={filterScope}
            setFilterScope={setFilterScope}
          />
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
        position: 'absolute',
        inset: 0,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
      }}
    >
      <span
        className="codicon codicon-loading codicon-modifier-spin"
        style={{ fontSize }}
      />
    </div>
  );
}

function noFilter(
  filter: string,
  filterScope: string[],
  filterGui: boolean
): boolean {
  return filter == '' && filterScope.length == 0 && filterGui == false;
}

// Total duration of the `highlight` blink animation (`blink 1s ... 4` = 1s ×
// 4 iterations), plus a small margin so the timer settles just after the
// animation has visually finished.
// const HIGHLIGHT_MS = 4100;

export default function GeneralTests({
  vscode,
}: GeneralTestsArg): ReactElement {
  const [filter, setFilter] = useState<string>('');
  const [filterScope, setFilterScope] = useState<string[]>([]);
  const [filterGui, setFilterGui] = useState<boolean>(true);
  const [grid, setGrid] = useState<boolean>(true);
  const [tests, setTests] = useState<TestMacro[] | undefined>(undefined);
  const [reload, setReload] = useState<boolean>(false);

  useEffect(() => {
    setVsCodeApi(vscode);
  }, [vscode]);

  // const settleHighlight = (id: number): void => {
  //   setTests((oldTests) =>
  //     oldTests?.map((test, index) =>
  //       index === id && test.state === 'JustFailed'
  //         ? { ...test, state: 'Failed' }
  //         : test
  //     )
  //   );
  // };

  // const scheduleSettle = (id: number): void => {
  //   setTimeout(() => settleHighlight(id), HIGHLIGHT_MS);
  // };

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
            tsTests.push(testMacro(test, false));
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
              let previousSuccess = test.success == undefined || test.success!;
              return testMacro(updatedTest, previousSuccess);
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
    console.log(`Run test ${id} Loading`);
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

  const filteredTest = tests
    ?.map((test, index) => ({ test, index }))
    .filter(({ test, index }) =>
      matchFilter(test, index, filter, filterScope, filterGui)
    );

  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: '10px' }}>
      <div
        style={{ display: 'flex', alignItems: 'center', flexDirection: 'row' }}
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
                } else if (filteredTest) {
                  let indexes = filteredTest.map(({ index }) => index);
                  setTests((oldTests) => {
                    if (oldTests) {
                      for (const index of indexes) {
                        oldTests[index].state = 'Loading';
                      }
                    }
                    return oldTests;
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
        tests={filteredTest}
        filter={filter}
        setFilter={setFilter}
        setFilterScope={setFilterScope}
        filterScope={filterScope}
        filterGui={filterGui}
        setFilterGui={setFilterGui}
      />
      <div className="select-test-print">
        <FormattedMessage
          id="generalTests.display"
          defaultMessage="Affichage :"
          children={(msg) => <h3>{msg}</h3>}
        />
        <div
          className={`pp-button ${grid ? 'selected' : ''}`}
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
        <div className="refresh-box">
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
      {filteredTest === undefined ? (
        <Loading size="medium" />
      ) : (
        <TestsGrid
          vscode={vscode}
          filtered={filteredTest}
          grid={grid}
          filterScope={filterScope}
          onRun={onRun}
        />
      )}
    </div>
  );
}
