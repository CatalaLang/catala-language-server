import { type ReactElement } from 'react';
import { useIntl } from 'react-intl';

import { assertUnreachable } from '../shared/util';
import { testMacro } from './testUtils';

export function SeparationLine(): ReactElement {
  return <div className="separation-line" />;
}

/**
 * The date of the last run of a test, in the conventions of the language of
 * VS Code: day, then time to the second.
 */
export function TestDate({ date }: { date: string }): ReactElement {
  const intl = useIntl();
  return (
    <>
      {intl.formatDate(date, {
        year: 'numeric',
        month: '2-digit',
        day: '2-digit',
        hour: '2-digit',
        minute: '2-digit',
        second: '2-digit',
      })}
    </>
  );
}

/**
 * Return the correct symbol depending on a state, usually the
 * given state comes from an object TestMacro.
 * The state is only conveyed by the shape and the color of the icon, so it is
 * also spelled out in a tooltip.
 */
export function TestStateIcon({
  loading,
  success,
}: {
  loading?: boolean;
  success?: boolean;
}): ReactElement {
  const intl = useIntl();
  const state = loading === true ? 'Loading' : testMacro(success);
  switch (state) {
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
      return assertUnreachable(state);
  }
}

export function Loading({
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
