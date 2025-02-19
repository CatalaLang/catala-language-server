import {
  useEffect,
  useState,
  type ReactElement,
  useCallback,
  useRef,
} from 'react';
import {
  type ParseResults,
  type Test,
  type TestList,
  type TestRunResults,
  readDownMessage,
  writeUpMessage,
} from './generated/test_case';
import TestEditor from './TestEditor';
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

type TestRunState = {
  [scope: string]: {
    status: 'running' | 'success' | 'error';
    results?: TestRunResults;
  };
};

type ModalState = {
  isOpen: boolean;
  step: 'selectFile' | 'selectScope';
  scopeUnderTest?: string;
  filename?: string;
  availableScopes?: string[];
};

type Props = { contents: UIState; vscode: WebviewApi<unknown> };

/** Editor for a collection of tests in a single file */
export default function TestFileEditor({
  contents,
  vscode,
}: Props): ReactElement {
  const [state, setState] = useState(contents);
  const [testRunState, setTestRunState] = useState<TestRunState>({});
  const [modalState, setModalState] = useState<ModalState>({
    isOpen: false,
    scopeUnderTest: '',
    filename: '',
    step: 'selectFile',
  });
  const modalContentRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (modalState.isOpen && modalContentRef.current) {
      modalContentRef.current.focus();
    }
  }, [modalState.isOpen]);

  const renderModal = (): ReactElement | null => {
    if (!modalState.isOpen) return null;

    const handleKeyDown = (e: React.KeyboardEvent): void => {
      if (e.key === 'Escape') {
        handleModalClose();
      }
    };

    return (
      <div className="modal">
        <div
          className="modal-content"
          ref={modalContentRef}
          tabIndex={-1}
          onKeyDown={handleKeyDown}
        >
          <h2>Add New Test</h2>

          {modalState.step === 'selectFile' && (
            <>
              <p>Select a Catala file to test:</p>
              <button className="file-select-button" onClick={handleFileSelect}>
                <span className="codicon codicon-file"></span>
                Select File...
              </button>
            </>
          )}

          {modalState.step === 'selectScope' && modalState.filename && (
            <>
              <p>Select a scope from {modalState.filename}:</p>
              <select
                value={modalState.scopeUnderTest || ''}
                onChange={(e) =>
                  setModalState((prev) => ({
                    ...prev,
                    scopeUnderTest: e.target.value,
                  }))
                }
              >
                <option value="">Select a scope</option>
                {modalState.availableScopes?.map((scope) => (
                  <option key={scope} value={scope}>
                    {scope}
                  </option>
                ))}
              </select>
            </>
          )}

          <div className="modal-buttons">
            {modalState.step === 'selectScope' && (
              <button
                onClick={handleModalSubmit}
                disabled={!modalState.scopeUnderTest}
              >
                Create Test
              </button>
            )}
            <button onClick={handleModalClose}>Cancel</button>
          </div>
        </div>
      </div>
    );
  };

  const onTestChange = useCallback(
    (newValue: Test): void => {
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

        vscode.postMessage(
          writeUpMessage({
            kind: 'Edit',
            value: newTestState,
          })
        );
      }
    },
    [state, vscode]
  );

  const onTestDelete = useCallback(
    (testScope: string): void => {
      if (state.state === 'success') {
        const newTestState = state.tests.filter(
          (test) => test.testing_scope !== testScope
        );
        console.log('Deleting test:', testScope);
        console.log('New test state:', newTestState);

        vscode.postMessage(
          writeUpMessage({
            kind: 'Edit',
            value: newTestState,
          })
        );
      }
    },
    [state, vscode]
  );

  const onTestRun = useCallback(
    (testScope: string): void => {
      setTestRunState((prev) => ({
        ...prev,
        [testScope]: { status: 'running' },
      }));
      vscode.postMessage(
        writeUpMessage({
          kind: 'TestRunRequest',
          value: {
            scope: testScope,
          },
        })
      );
    },
    [vscode]
  );

  const onAddNewTest = useCallback((): void => {
    setModalState({
      isOpen: true,
      step: 'selectFile',
      scopeUnderTest: undefined,
      filename: undefined,
      availableScopes: undefined,
    });
  }, []);

  const handleFileSelect = useCallback(() => {
    vscode.postMessage(
      writeUpMessage({
        kind: 'SelectFileForNewTest',
      })
    );
  }, [vscode]);

  const handleModalSubmit = useCallback((): void => {
    if (modalState.scopeUnderTest && modalState.filename) {
      vscode.postMessage(
        writeUpMessage({
          kind: 'TestGenerateRequest',
          value: {
            scopeUnderTest: modalState.scopeUnderTest,
            filename: modalState.filename,
          },
        })
      );
      setModalState((prev) => ({
        ...prev,
        isOpen: false,
      }));
    }
  }, [modalState, vscode]);

  const handleModalClose = useCallback((): void => {
    setModalState((prev) => ({
      ...prev,
      isOpen: false,
    }));
  }, []);

  useEffect(() => {
    const handleMessage = (event: MessageEvent): void => {
      const msg = readDownMessage(event.data);
      switch (msg.kind) {
        case 'Update':
          setState(parseResultsToUiState(msg.value));
          break;
        case 'TestRunResults': {
          const results = msg.value;
          setTestRunState((prev) => {
            const updatedScope = Object.keys(prev).find(
              (scope) => prev[scope].status === 'running'
            );
            if (updatedScope) {
              return {
                ...prev,
                [updatedScope]: {
                  status: results.kind === 'Ok' ? 'success' : 'error',
                  results,
                },
              };
            }
            return prev;
          });
          break;
        }
        case 'FileSelectedForNewTest':
          setModalState((prev) => ({
            ...prev,
            step: 'selectScope',
            filename: msg.value.filename,
            availableScopes: msg.value.available_scopes,
          }));
          break;
        default:
          assertUnreachable(msg);
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
        <div role="alert" className="test-editor-error">
          <h2>Oops! This should not have happened...</h2>
          <pre className="test-editor-error-message">{state.message}</pre>
          <button
            className="test-editor-open-text"
            onClick={() =>
              vscode.postMessage(writeUpMessage({ kind: 'OpenInTextEditor' }))
            }
          >
            <span className="codicon codicon-edit"></span>
            Open in Text Editor
          </button>
        </div>
      );
    }
    case 'initializing':
      return <strong>Initializing...</strong>;
    case 'success': {
      if (state.tests.length === 0) {
        return (
          <>
            <div className="test-editor-empty">
              <p className="test-editor-empty-message">
                No test cases found. Would you like to create your first test?
              </p>
              <button className="test-editor-add-button" onClick={onAddNewTest}>
                <span className="codicon codicon-add"></span>
                Add new test
              </button>
            </div>
            {renderModal()}
          </>
        );
      }
      return (
        <div className="test-editor-container">
          <div className="test-editor-add-button-container">
            <button className="test-editor-add-button" onClick={onAddNewTest}>
              <span className="codicon codicon-add"></span>
              Add new test
            </button>
          </div>
          {state.tests.map((test) => (
            <TestEditor
              test={test}
              key={test.testing_scope}
              onTestChange={onTestChange}
              onTestDelete={onTestDelete}
              onTestRun={onTestRun}
              runState={testRunState[test.testing_scope]}
            />
          ))}
          {renderModal()}
        </div>
      );
    }
    default:
      assertUnreachable(state);
  }
}

function parseResultsToUiState(tests: ParseResults): UIState {
  switch (tests.kind) {
    case 'Error':
      return { state: 'error', message: tests.value };
    case 'Results':
      return { state: 'success', tests: tests.value };
    default:
      assertUnreachable(tests);
  }
}
