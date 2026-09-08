import { type ReactElement } from 'react';
import { IntlProvider } from 'react-intl';
import TestFileEditor from './test-case-editor/TestFileEditor';
import ScopeInputEditor from './scope-editor/ScopeInputEditor';

import { type WebviewApi } from 'vscode-webview';

import frMessages from './locales/fr.json';
import enMessages from './locales/en.json';
import plMessages from './locales/pl.json';

type Messages = Record<string, string>;

/* Undo belongs to the document, not to whichever <input> has focus: the
   browser's native input undo swallows the first Ctrl+Z and VS Code only
   sees the second. preventDefault stops the native undo; the key event is
   still forwarded to the workbench, which runs the custom editor's undo. */
window.addEventListener(
  'keydown',
  (e: KeyboardEvent): void => {
    if (
      (e.ctrlKey || e.metaKey) &&
      !e.altKey &&
      (e.key === 'z' || e.key === 'Z' || e.key === 'y' || e.key === 'Y')
    ) {
      e.preventDefault();
    }
  },
  true
);

const allMessages: Record<string, Messages> = {
  fr: frMessages,
  en: enMessages,
  pl: plMessages,
};

type Props = {
  language: string;
  vscode: WebviewApi<unknown>;
  scopename?: string;
};

export default function App({ language, vscode }: Props): ReactElement {
  const messages = allMessages[language] || enMessages;

  return (
    <IntlProvider locale={language} messages={messages} defaultLocale="en">
      <TestFileEditor contents={{ state: 'initializing' }} vscode={vscode} />
    </IntlProvider>
  );
}

export function InputApp({ language, vscode, scopename }: Props): ReactElement {
  const messages = allMessages[language] || enMessages;

  return (
    <IntlProvider locale={language} messages={messages} defaultLocale="en">
      <ScopeInputEditor
        contents={{ state: 'initializing' }}
        vscode={vscode}
        scopename={scopename ?? ''}
      />
    </IntlProvider>
  );
}
