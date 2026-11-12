import {
  Fragment,
  useEffect,
  useLayoutEffect,
  useRef,
  useState,
  type ReactElement,
} from 'react';
import { FormattedMessage } from 'react-intl';

import { type WebviewApi } from 'vscode-webview';

import { writeTestsUpMessage } from '../generated/catala_types';
import { OpenGUI, OpenTextEditor, RunIcon } from './buttons';
import {
  hasFailure,
  isGui,
  testDescription,
  testingScope,
  testTitle,
} from './testUtils';
import type { CardGridArg, TestItemArg, TestMacro } from './types';
import { TestDate, TestStateIcon } from './widgets';

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

function TestPath({
  vscode,
  test,
}: {
  vscode: WebviewApi<unknown>;
  test: TestMacro;
}): ReactElement {
  const displayed = test.relative_filename ?? test.filename;
  // `+ 1` keeps the separator on the directory side, and yields 0 (hence an
  // empty directory) when the path is a bare file name. Both separators are
  // looked for, since the fallback path is whatever the host reported.
  const cut =
    Math.max(displayed.lastIndexOf('/'), displayed.lastIndexOf('\\')) + 1;
  const directory = displayed.slice(0, cut);
  const name = displayed.slice(cut);
  return (
    <a
      href=""
      title={test.filename}
      onClick={(event) => {
        event.preventDefault();
        vscode.postMessage(
          writeTestsUpMessage(
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
      {directory == '' ? null : (
        <span className="test-path-directory">{directory}</span>
      )}
      {name}
    </a>
  );
}

/**
 * The component to represent a Line in the table representation of the tests,
 * it has some logic due to the description that can overflow
 */
function TestLine({ vscode, test, onRun }: TestItemArg): ReactElement {
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
      <th className="path-column">
        <TestPath vscode={vscode} test={test} />
      </th>
      <td>{testTitle(test)}</td>
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
          {description}
        </span>
        {overflowActive && (
          <span
            className={`codicon codicon-fold-${expanded ? 'up' : 'down'}`}
          />
        )}
      </td>
      <td>
        {test.date !== undefined ? (
          <TestDate date={test.date} />
        ) : (
          <FormattedMessage
            id="generalTests.neverRun"
            defaultMessage="Jamais lancé"
          />
        )}
      </td>
      <td>
        <TestStateIcon loading={test.loading} success={test.success} />
      </td>
      <td>
        <RunIcon className="run-icon" onRun={() => onRun(test.index)} />
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

function HeaderLine({ gui }: { gui: boolean }): ReactElement {
  return (
    <thead>
      <tr>
        <th className="path-column">
          <FormattedMessage
            id="generalTests.header.file"
            defaultMessage="Fichier"
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

export function TestList({
  vscode,
  onRun,
  tests,
  filteredScope,
}: CardGridArg): ReactElement {
  let map = new Map<string, TestMacro[]>();
  let not_gui: TestMacro[] = [];
  let gui: TestMacro[] = [];
  for (let index = 0; index < tests.length; index++) {
    const element = tests[index];
    if (element.test.kind == 'GUI') {
      gui.push(element);
      let scope = element.test.value.scope_tested;
      let scopeTests = map.get(scope) ?? [];
      scopeTests.push(element);
      map.set(scope, scopeTests);
    } else {
      not_gui.push(element);
    }
  }

  let allTests: [string, TestMacro[]][] = [];
  if (filteredScope.length == 0) {
    // Everything ends up in a single table, so grouping by scope on the way
    // there would only shuffle it: `tests` arrives with the failures already
    // hoisted to the front, and that is the order to keep.
    allTests = [['Tests', gui]];
  } else {
    // One table per scope. Failures already lead inside each group — the
    // grouping above preserves the order they came in — but the groups
    // themselves have to be reordered, otherwise a failing scope can sit below
    // several passing ones. The sort is stable, so scopes that are alike keep
    // their relative order.
    allTests = [...map.entries()].sort(
      ([, a], [, b]) => Number(hasFailure(b)) - Number(hasFailure(a))
    );
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
                {tests.map((test) => (
                  <TestLine
                    key={test.index}
                    vscode={vscode}
                    test={test}
                    onRun={onRun}
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
              {not_gui.map((test) => {
                return (
                  <TestLine
                    key={test.index}
                    vscode={vscode}
                    test={test}
                    onRun={onRun}
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
