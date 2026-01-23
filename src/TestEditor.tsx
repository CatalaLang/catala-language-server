import type { ChangeEvent } from 'react';
import { type ReactElement, useEffect, useRef } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import {
  type Test,
  type TestInputs,
  type TestRunResults,
  type PathSegment,
  type RuntimeValue,
} from './generated/catala_types';
import TestInputsEditor from './TestInputsEditor';
import TestOutputsEditor from './TestOutputsEditor';
import { type TestRunStatus } from './TestFileEditor';
import { confirm } from './messaging/confirm';

type Props = {
  test: Test;
  onTestChange(newValue: Test, mayBeBatched: boolean): void;
  onTestDelete(testScope: string): void;
  onTestRun(testScope: string): void;
  onTestOutputsReset(testScope: string): void;
  runState?: {
    status: TestRunStatus;
    results?: TestRunResults;
    stale?: boolean;
  };
  onDiffResolved(scope: string, path: PathSegment[]): void;
  onInvalidateDiffs(scope: string, pathPrefix: PathSegment[]): void;
};

// Editor for a single test case (child of TestFileEditor)
export default function TestEditor(props: Props): ReactElement {
  const intl = useIntl();

  function onTestInputsChange(newValue: TestInputs): void {
    props.onTestChange(
      {
        ...props.test,
        test_inputs: newValue,
      },
      false
    );
  }

  function onDescriptionChange(event: ChangeEvent<HTMLTextAreaElement>): void {
    props.onTestChange(
      {
        ...props.test,
        description: event.target.value,
      },
      true
    );
  }

  function onTitleChange(newTitle: string): void {
    props.onTestChange(
      {
        ...props.test,
        title: newTitle,
      },
      true
    );
  }

  const expectedSectionRef = useRef<HTMLDivElement>(null);
  // Scope for searching the first '.invalid-badge' or '.unset-badge' before running; used to scroll into view
  const unsetElementRef = useRef<HTMLDivElement>(null);
  const expectedAnchorId = `expected-${encodeURIComponent(props.test.testing_scope)}`;

  useEffect(() => {
    const runState = props.runState;
    const shouldFocus =
      !!runState &&
      runState.results?.kind === 'Ok' &&
      runState.results.value.assert_failures;

    if (shouldFocus) {
      setTimeout(() => {
        expectedSectionRef.current?.focus();
        expectedSectionRef.current?.scrollIntoView({
          behavior: 'smooth',
          block: 'start',
        });
      }, 0);
    }
  }, [props.runState]);

  function containsUnsetInRuntime(rv: RuntimeValue): boolean {
    switch (rv.value.kind) {
      case 'Unset':
        return true;
      case 'Array':
        return rv.value.value.some(containsUnsetInRuntime);
      case 'Struct': {
        const map = rv.value.value[1];
        return Array.from(map.values()).some(containsUnsetInRuntime);
      }
      case 'Enum': {
        const payload = rv.value.value[1][1];
        return payload?.value ? containsUnsetInRuntime(payload.value) : false;
      }
      default:
        return false;
    }
  }

  function hasUnsetInTest(test: Test): boolean {
    const inputsHas = Array.from(test.test_inputs.values()).some(
      (io) => io.value && containsUnsetInRuntime(io.value.value)
    );
    const outputsHas = Array.from(test.test_outputs.values()).some(
      (io) => io.value && containsUnsetInRuntime(io.value.value)
    );
    return inputsHas || outputsHas;
  }

  const scrollToFirstUnset = (): void => {
    setTimeout(() => {
      const container = unsetElementRef.current ?? document;
      const el = container.querySelector(
        '.invalid-badge, .unset-badge'
      ) as HTMLElement | null;
      if (el) {
        el.scrollIntoView({ behavior: 'smooth', block: 'center' });
        (el as HTMLElement)?.focus?.();
      }
    }, 0);
  };

  const runWithUnsetCheck = async (): Promise<void> => {
    if (hasUnsetInTest(props.test)) {
      scrollToFirstUnset();
      const confirmed = await confirm('RunTestWithUnsetValues');
      if (!confirmed) return;
    }
    props.onTestRun(props.test.testing_scope);
  };

  const resetWithUnsetCheck = async (): Promise<void> => {
    if (hasUnsetInTest(props.test)) {
      scrollToFirstUnset();
      const confirmed = await confirm('RunTestWithUnsetValues');
      if (!confirmed) return;
    }
    props.onTestOutputsReset(props.test.testing_scope);
  };

  return (
    <div className="test-editor" ref={unsetElementRef}>
      <div className="test-editor-breadcrumb body-b3">
        {props.test.testing_scope} ➛ {String(props.test.tested_scope.name)}
      </div>
      <div className="test-title-wrapper">
        <h2
          className="test-title-editable heading-h2"
          contentEditable
          suppressContentEditableWarning
          role="textbox"
          aria-label={intl.formatMessage({
            id: 'testEditor.title',
            defaultMessage: 'Title',
          })}
          onBlur={(e) => onTitleChange(e.currentTarget.textContent || '')}
          onKeyDown={(e) => {
            if (e.key === 'Enter' || e.key === 'Escape') {
              e.preventDefault();
              (e.target as HTMLElement).blur();

              if (e.key === 'Escape') {
                // Move focus to next focusable element
                const focusableElements = Array.from(
                  document.querySelectorAll(
                    'button, [href], input, select, textarea, [tabindex]:not([tabindex="-1"])'
                  )
                );
                const currentIndex = focusableElements.indexOf(
                  e.target as HTMLElement
                );
                const nextElement = focusableElements[
                  currentIndex + 1
                ] as HTMLElement;
                nextElement?.focus();
              }
            }
          }}
          onPaste={(e) => {
            e.preventDefault();
            const text = e.clipboardData.getData('text/plain');
            document.execCommand('insertText', false, text);
          }}
          dangerouslySetInnerHTML={{ __html: props.test.title }}
        />
        <span
          className="codicon codicon-edit test-title-edit-icon"
          onClick={(e) => {
            const h2 = e.currentTarget.previousElementSibling as HTMLElement;
            h2?.focus();
          }}
          aria-hidden="true"
        ></span>
      </div>
      <div className="test-editor-content">
        <div className="test-section">
          <h2 className="test-section-title heading-h2">
            <FormattedMessage
              id="testEditor.description"
              defaultMessage="Description"
            />
          </h2>
          <div className="test-description-editor">
            <textarea
              value={props.test.description}
              onChange={onDescriptionChange}
              onBlur={onDescriptionChange}
              placeholder={intl.formatMessage({
                id: 'testEditor.descriptionPlaceholder',
              })}
              rows={10}
              className="test-description-textarea"
            />
          </div>
        </div>
        <div className="test-section">
          <h2 className="test-section-title heading-h2">
            <FormattedMessage id="testEditor.inputs" />
          </h2>
          <TestInputsEditor
            test_inputs={props.test.test_inputs}
            onTestInputsChange={onTestInputsChange}
          />
        </div>
        <div
          className="test-section"
          id={expectedAnchorId}
          ref={expectedSectionRef}
          tabIndex={-1}
        >
          <h2 className="test-section-title heading-h2">
            <FormattedMessage id="testEditor.expectedValues" />
          </h2>
          <div className="test-result-header">
            <div className="test-result-action-bar">
              <button
                className="reset-expected-values button-action-dvp body-b3"
                title={intl.formatMessage({ id: 'testEditor.resetExpected' })}
                onClick={resetWithUnsetCheck}
              >
                <span className="codicon codicon-refresh"></span>{' '}
                <FormattedMessage id="testEditor.resetExpectedButton" />
              </button>
              <button
                className={`button-action-dvp ${props.runState?.status ?? ''}`}
                title={intl.formatMessage({ id: 'testEditor.runTest' })}
                onClick={runWithUnsetCheck}
                disabled={props.runState?.status === 'running'}
              >
                <span
                  className={`codicon ${props.runState?.status === 'running' ? 'codicon-loading codicon-modifier-spin' : 'codicon-play'}`}
                ></span>{' '}
                Lancer le test
              </button>
            </div>
            <div className="test-result">
              {props.runState?.status === 'success' &&
                props.runState?.results?.kind === 'Ok' &&
                !props.runState.results.value.assert_failures && (
                  <p className="test-run-result test-run-success body-1">
                    <span className="codicon codicon-check-all"></span>
                    <FormattedMessage
                      id="testEditor.passed"
                      defaultMessage="Passed"
                    />
                  </p>
                )}
              {(props.runState?.status === 'error' ||
                (props.runState?.results?.kind === 'Ok' &&
                  props.runState.results.value.assert_failures)) && (
                <div className="test-result-information">
                  <p className="test-run-result test-run-error body-1">
                    <span className="codicon codicon-warning"></span>
                    <FormattedMessage
                      id="testEditor.failed"
                      defaultMessage="Failed"
                    />
                  </p>
                </div>
              )}
            </div>
            {props.runState?.stale && (
              <div className="test-result-information">
                <p className="body-3">
                  <span className="codicon codicon-history"></span>{' '}
                  <FormattedMessage
                    id="testEditor.diffsStale"
                    defaultMessage="Diffs are out of date. Re-run to refresh."
                  />
                </p>
              </div>
            )}
          </div>

          <TestOutputsEditor
            test={props.test}
            onTestChange={(test) => {
              props.onTestChange(test, false);
            }}
            diffs={
              props.runState?.results?.kind === 'Ok'
                ? props.runState.results.value.diffs
                : []
            }
            onDiffResolved={(path: PathSegment[]) =>
              props.onDiffResolved(props.test.testing_scope, path)
            }
            onInvalidateDiffs={(pathPrefix: PathSegment[]) =>
              props.onInvalidateDiffs(props.test.testing_scope, pathPrefix)
            }
          />
        </div>
      </div>
    </div>
  );
}
