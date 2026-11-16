import {
  type ReactElement,
  type ReactNode,
  createContext,
  useEffect,
  useState,
} from 'react';
import { IntlProvider } from 'react-intl';
import {
  type WebviewSettings,
  readDownMessage,
} from '../generated/catala_types';
import { allMessages } from './settings';

const SettingsContext = createContext<WebviewSettings>({ locale: 'en' });

export function CatalaSettingsProvider({
  language,
  children,
}: {
  language: string;
  children: ReactNode;
}): ReactElement {
  const [settings, setSettings] = useState<WebviewSettings>({
    locale: language,
  });
  useEffect(() => {
    const handleMessage = (event: MessageEvent): void => {
      const message = readDownMessage(event.data);
      if (message.kind === 'Settings') {
        setSettings(message.value);
      }
    };
    window.addEventListener('message', handleMessage);
    return (): void => window.removeEventListener('message', handleMessage);
  }, []);
  const resolved = settings.locale in allMessages ? settings.locale : 'en';
  return (
    <SettingsContext.Provider value={settings}>
      <IntlProvider
        locale={resolved}
        messages={allMessages[resolved]}
        defaultLocale="en"
      >
        {children}
      </IntlProvider>
    </SettingsContext.Provider>
  );
}
