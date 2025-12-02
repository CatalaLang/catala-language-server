import type { ChangeEvent } from 'react';
import { type ReactElement, useEffect, useRef, useState } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import {
  type Test,
  type TestInputs,
  type TestRunResults,
  type PathSegment,
} from './generated/test_case';
import TestInputsEditor from './TestInputsEditor';
import TestOutputsEditor from './TestOutputsEditor';
import { type TestRunStatus } from './TestFileEditor';

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

  function onTitleChange(event: ChangeEvent<HTMLInputElement>): void {
    props.onTestChange(
      {
        ...props.test,
        title: event.target.value,
      },
      true
    );
  }

  const [isCollapsed, setIsCollapsed] = useState(false);
  const expectedSectionRef = useRef<HTMLDivElement>(null);
  const expectedAnchorId = `expected-${encodeURIComponent(props.test.testing_scope)}`;

  useEffect(() => {
    const runState = props.runState;
    const shouldFocus =
      !!runState &&
      runState.results?.kind === 'Ok' &&
      runState.results.value.assert_failures;

    if (shouldFocus) {
      if (isCollapsed) setIsCollapsed(false);
      setTimeout(() => {
        expectedSectionRef.current?.focus();
        expectedSectionRef.current?.scrollIntoView({
          behavior: 'smooth',
          block: 'start',
        });
      }, 0);
    }
  }, [props.runState, isCollapsed]);

  return (
    <div className="test-editor">
      <div className="test-editor-breadcrumb body-b3">
        {props.test.testing_scope} ➛ {String(props.test.tested_scope.name)}
      </div>
      <h2 className="test-section-title heading-h2">
        <FormattedMessage id="testEditor.title" defaultMessage="Title" />
      </h2>
      <input
        type="text"
        className="test-title-input heading-h2"
        value={props.test.title}
        onChange={onTitleChange}
      />
      <div
        className="test-editor-content"
        style={{ display: isCollapsed ? 'none' : 'block' }}
      >
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
            <h3 className="heading-h3">Résultats obtenus</h3>
            <p className="body-3">Aucun test réalisé / Date du dernier test</p>
            <div className="test-result-action-bar">
              <button
                className="reset-expected-values button-action-dvp body-b3"
                title={intl.formatMessage({ id: 'testEditor.resetExpected' })}
                onClick={() => {
                  props.onTestOutputsReset(props.test.testing_scope);
                }}
              >
                <span className="codicon codicon-refresh"></span> Remplacer avec
                les valeurs attendues
              </button>
              <button
                className={`button-action-dvp ${props.runState?.status ?? ''}`}
                title={intl.formatMessage({ id: 'testEditor.run' })}
                onClick={() => props.onTestRun(props.test.testing_scope)}
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
                  <p className="body-3">
                    Informations complémentaires sur les erreurs rencontrées
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
