import { useEffect, useState, type ReactElement } from 'react';
import {
  readTestList,
  writeTestList,
  type Test,
  type TestList,
} from './generated/test_case';
import TestEditor from './TestEditor';
import type { DownMessage } from './messages';
import type { ParseResults } from './testCaseEditor';
import { assertUnreachable } from './util';
import type { WebviewApi } from 'vscode-webview';

// Note:
//
// Test files are (for now) represented as a list of tests (instance
// of `TestList`) -- it's enough for now but will probably require
// a richer representation later to include markdown prose
// between tests?
//

type UIState =
  | { state: 'initializing' }
  | { state: 'error'; message: string }
  | { state: 'success'; tests: TestList };
type Props = { contents: UIState; vscode: WebviewApi<unknown> };

/** Editor for a collection of tests in a single file */
export default function TestFileEditor({
  contents,
  vscode,
}: Props): ReactElement {
  const [state, setState] = useState(contents);

  function onTestChange(newValue: Test): void {
    if (state.state === 'success') {
      const idx = state.tests.findIndex(
        (tst) => tst.testing_scope === newValue.testing_scope
      );
      console.log(`test changed at index ${idx}`);
      const newTestState = [...state.tests];
      newTestState[idx] = newValue; //we can do away with this when array.with() becomes widely available
      console.log('old test state');
      console.log(state.tests);
      console.log('new test state');
      console.log(newTestState);

      vscode.postMessage({
        kind: 'edit',
        tests: JSON.stringify(writeTestList(newTestState)),
      });
    }
  }

  function onTestDelete(testScope: string): void {
    if (state.state === 'success') {
      const newTestState = state.tests.filter(
        (test) => test.testing_scope !== testScope
      );
      console.log('Deleting test:', testScope);
      console.log('New test state:', newTestState);

      vscode.postMessage({
        kind: 'edit',
        tests: JSON.stringify(writeTestList(newTestState)),
      });
    }
  }

  useEffect(() => {
    const handleMessage = (event: MessageEvent): void => {
      //XXX we need something better! type coercion because cross-window message exchange is untyped
      // (or is it? )
      const message = event.data as DownMessage;
      switch (message.kind) {
        case 'update':
          setState(parseResultsToUiState(message.parseResults));
          break;
      }
    };

    window.addEventListener('message', handleMessage);

    // Cleanup function to remove event listener
    return (): void => {
      window.removeEventListener('message', handleMessage);
    };
  }, []);

  switch (state.state) {
    case 'error': {
      return (
        <div>
          <b>Error: </b>
          {state.message}
        </div>
      );
    }
    case 'initializing':
      return <b>Initializing...</b>;
    case 'success': {
      return (
        <>
          {state.tests.map((test) => (
            <TestEditor
              test={test}
              key={test.testing_scope}
              onTestChange={onTestChange}
              onTestDelete={onTestDelete}
            />
          ))}
        </>
      );
    }
    default:
      assertUnreachable(state);
  }
}

function parseResultsToUiState(tests: ParseResults): UIState {
  if ('error' in tests) {
    return { state: 'error', message: tests.error };
  } else if ('results' in tests) {
    return { state: 'success', tests: readTestList(tests.results) };
  } else {
    assertUnreachable(tests);
  }
}
