import { type ReactElement } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';

import { type WebviewApi } from 'vscode-webview';

import { writeTestsUpMessage } from '../generated/catala_types';

/**
 * Component to run a bunch of tests, the run function is given in argument
 * so that we can choose the function to give
 */
export function RunAllTests({
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
export function AddNewTest({
  vscode,
}: {
  vscode: WebviewApi<unknown>;
}): ReactElement {
  return (
    <span
      className="vscode-button"
      onClick={(event) => {
        event.preventDefault();
        vscode.postMessage(
          writeTestsUpMessage({
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

/**
 * The component RunIcon is a little icon Run to run a test on a Catala file
 * A className can be given in parameter so that we can reuse the same component
 * in different case.
 */
export function RunIcon({
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
export function OpenGUI({
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
          writeTestsUpMessage({ kind: 'OpenInTestEditor', value: filename })
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
export function OpenTextEditor({
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
          writeTestsUpMessage({
            kind: 'OpenInTextEditor',
            value: { value: filename },
          })
        );
      }}
      className="codicon codicon-go-to-file open-text"
    />
  );
}
