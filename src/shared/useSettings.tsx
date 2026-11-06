import {
  type ReactElement,
  type ReactNode,
  createContext,
  useEffect,
  useState,
} from 'react';
import { IntlProvider } from 'react-intl';
import type { CatalaSettings } from './settings';
import { defaultSettings, isSettingsMessage } from './settings';
import frMessages from '../locales/fr.json';
import enMessages from '../locales/en.json';
import plMessages from '../locales/pl.json';

const allMessages: Record<string, Record<string, string>> = {
  fr: frMessages,
  en: enMessages,
  pl: plMessages,
};

const SettingsContext = createContext<CatalaSettings>(defaultSettings);

export function CatalaSettingsProvider({
  language,
  children,
}: {
  language: string;
  children: ReactNode;
}): ReactElement {
  const [settings, setSettings] = useState<CatalaSettings>(defaultSettings);
  const [locale, setLocale] = useState(language);
  useEffect(() => {
    const handleMessage = (event: MessageEvent): void => {
      if (isSettingsMessage(event.data)) {
        setSettings(event.data.value);
        setLocale(event.data.locale);
      }
    };
    window.addEventListener('message', handleMessage);
    return (): void => window.removeEventListener('message', handleMessage);
  }, []);
  const resolved = locale in allMessages ? locale : 'en';
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
