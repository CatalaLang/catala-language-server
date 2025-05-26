import {
  useEffect,
  useState,
  type ReactElement,
  useCallback,
  useRef,
} from 'react';
import { FormattedMessage } from 'react-intl';
import type { ScopeDefList } from './generated/test_case';
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
import type { CancelSourceUpdateCallback } from './contexts/CancelSourceUpdateContext';
import { CancelSourceUpdateProvider } from './contexts/CancelSourceUpdateContext';

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
  | { state: 'emptyTestListMismatch' }
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
  availableScopes?: ScopeDefList;
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
  const [isUIDisabled, setIsUIDisabled] = useState(false);
  const activeElementRef = useRef<Element | null>(null);

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
              <p>
                <FormattedMessage id="testFile.selectFile" />
              </p>
              <button className="file-select-button" onClick={handleFileSelect}>
                <span className="codicon codicon-file"></span>
                Select File...
              </button>
            </>
          )}

          {modalState.step === 'selectScope' && modalState.filename && (
            <>
              <p>
                <FormattedMessage
                  id="testFile.selectScope"
                  values={{ filename: modalState.filename }}
                />
              </p>
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
                  <option key={scope.name} value={scope.name}>
                    {scope.name}
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
                <FormattedMessage id="testFile.createTest" />
              </button>
            )}
            <button onClick={handleModalClose}>
              <FormattedMessage id="testFile.cancel" />
            </button>
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
        const newTestState = [...state.tests];
        newTestState[idx] = newValue; //we can do away with this when array.with() becomes widely available

        // Optimistically update the state
        setState({ state: 'success', tests: newTestState });

        // Send the update to the backend
        vscode.postMessage(
          writeUpMessage({
            kind: 'GuiEdit',
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

        // Optimistically update the state
        setState({ state: 'success', tests: newTestState });

        // Send the deletion to the backend
        vscode.postMessage(
          writeUpMessage({
            kind: 'GuiEdit',
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
            scope_under_test: modalState.scopeUnderTest,
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

  const cancelSourceUpdate: CancelSourceUpdateCallback = () => {
    vscode.postMessage(writeUpMessage({ kind: 'CancelSourceUpdate' }));
  };

  useEffect(() => {
    const handleMessage = (event: MessageEvent): void => {
      const message = readDownMessage(event.data);
      switch (message.kind) {
        case 'Update':
          setState(parseResultsToUiState(message.value));
          setIsUIDisabled(false);
          // Restaurer le focus après la mise à jour
          setTimeout(() => {
            if (
              activeElementRef.current &&
              activeElementRef.current instanceof HTMLElement
            ) {
              activeElementRef.current.focus();
            }
          }, 0);
          break;
        case 'TestRunResults': {
          const results = message.value;
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
            filename: message.value.filename,
            availableScopes: message.value.available_scopes,
          }));
          break;
        case 'DisableUI':
          // Sauvegarder l'élément actif avant de désactiver l'interface
          activeElementRef.current = document.activeElement;
          setIsUIDisabled(true);
          break;
        default:
          assertUnreachable(message);
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
      return <ParsingErrorWarning message={state.message} vscode={vscode} />;
    }
    case 'emptyTestListMismatch': {
      return <EmptyTestListMismatchWarning vscode={vscode} />;
    }
    case 'initializing':
      return (
        <strong>
          <FormattedMessage id="app.initializing" />
        </strong>
      );
    case 'success': {
      if (state.tests.length === 0) {
        return (
          <>
            <div className="test-editor-empty">
              <p className="test-editor-empty-message">
                <FormattedMessage id="testFile.noTests" />
              </p>
              <button className="test-editor-add-button" onClick={onAddNewTest}>
                <span className="codicon codicon-add"></span>
                <FormattedMessage id="testFile.addTest" />
              </button>
            </div>
            {renderModal()}
          </>
        );
      }
      return (
        <CancelSourceUpdateProvider onCancelSourceUpdate={cancelSourceUpdate}>
          <div
            className="test-editor-container"
            {...(isUIDisabled ? { inert: '' } : {})}
          >
            <div className="test-editor-top-bar">
              <button className="test-editor-add-button" onClick={onAddNewTest}>
                <span className="codicon codicon-add"></span>
                <FormattedMessage id="testFile.addNewTest" />
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
        </CancelSourceUpdateProvider>
      );
    }
    default:
      assertUnreachable(state);
  }
}

function ParsingErrorWarning({
  message,
  vscode,
}: {
  message: string;
  vscode: WebviewApi<unknown>;
}): ReactElement {
  return (
    <div role="alert" className="test-editor-error">
      <h2>
        <FormattedMessage id="testFile.errorTitle" />
      </h2>
      <pre className="test-editor-error-message">{message}</pre>
      <button
        className="test-editor-open-text"
        onClick={() =>
          vscode.postMessage(writeUpMessage({ kind: 'OpenInTextEditor' }))
        }
      >
        <span className="codicon codicon-edit"></span>
        <FormattedMessage id="testFile.openTextEditor" />
      </button>
    </div>
  );
}

function EmptyTestListMismatchWarning({
  vscode,
}: {
  vscode: WebviewApi<unknown>;
}): ReactElement {
  return (
    <div className="test-editor-warning">
      <h2>
        <FormattedMessage id="testFile.formatIssueTitle" />
      </h2>
      <p>
        <FormattedMessage id="testFile.formatIssueText" />
      </p>
      <p>
        <FormattedMessage id="testFile.formatIssueFix" />
      </p>
      <div className="test-editor-warning-actions">
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
    </div>
  );
}

function parseResultsToUiState(tests: ParseResults): UIState {
  switch (tests.kind) {
    case 'ParseError':
      return { state: 'error', message: tests.value };
    case 'EmptyTestListMismatch':
      return { state: 'emptyTestListMismatch' };
    case 'Results':
      return { state: 'success', tests: tests.value };
    default:
      assertUnreachable(tests);
  }
}
