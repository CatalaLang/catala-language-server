import { type ReactElement } from 'react';
import { IntlProvider } from 'react-intl';
import TestFileEditor from './TestFileEditor';
import ScopeInputEditor from './scope_input_editor/ScopeInputEditor';

import { type WebviewApi } from 'vscode-webview';

import frMessages from './locales/fr.json';
import enMessages from './locales/en.json';

type Messages = Record<string, string>;

const allMessages: Record<string, Messages> = {
  fr: frMessages,
  en: enMessages,
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
