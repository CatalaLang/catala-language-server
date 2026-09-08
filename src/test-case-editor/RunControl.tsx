import React from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import type { TestRunStatus } from './TestFileEditor';
import type { TestRunResults } from '../generated/catala_types';

/** The run button and its result, shared by the test editor and the recovery view. */

export type RunControlProps = {
  status?: TestRunStatus;
  results?: TestRunResults;
  onRun: () => void;
  /** Names what is being run, when that is not simply "the test". */
  labelId?: string;
};

export default function RunControl({
  status,
  results,
  onRun,
  labelId = 'testEditor.runTest',
}: RunControlProps): React.JSX.Element {
  const intl = useIntl();
  const label = intl.formatMessage({ id: labelId });
  const failed =
    status === 'error' ||
    (results?.kind === 'Ok' && results.value.assert_failures);
  const passed =
    status === 'success' &&
    results?.kind === 'Ok' &&
    !results.value.assert_failures;

  return (
    <div className="run-control">
      <button
        className={`button-action-dvp body-b3 ${status ?? ''}`}
        title={label}
        onClick={onRun}
        disabled={status === 'running'}
      >
        <span
          className={`codicon ${status === 'running' ? 'codicon-loading codicon-modifier-spin' : 'codicon-play'}`}
        ></span>{' '}
        {label}
      </button>
      {passed && (
        <p className="test-run-result test-run-success body-1">
          <span className="codicon codicon-check-all"></span>
          <FormattedMessage id="testEditor.passed" defaultMessage="Passed" />
        </p>
      )}
      {failed && (
        <p className="test-run-result test-run-error body-1">
          <span className="codicon codicon-warning"></span>
          <FormattedMessage id="testEditor.failed" defaultMessage="Failed" />
        </p>
      )}
    </div>
  );
}
