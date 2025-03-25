import { type ReactElement } from 'react';
import { IntlProvider } from 'react-intl';
import TestFileEditor from './TestFileEditor';

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
};

export default function App({ language, vscode }: Props): ReactElement {
  const messages = allMessages[language] || enMessages;

  return (
    <IntlProvider locale={language} messages={messages} defaultLocale="en">
      <TestFileEditor contents={{ state: 'initializing' }} vscode={vscode} />
    </IntlProvider>
  );
}
