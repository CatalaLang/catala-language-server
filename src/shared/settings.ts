import enMessages from '../locales/en.json';
import frMessages from '../locales/fr.json';
import plMessages from '../locales/pl.json';

export const allMessages: Record<string, Record<string, string>> = {
  en: enMessages,
  fr: frMessages,
  pl: plMessages,
};

export type LanguageSetting = 'file' | 'vscode';

const LANGUAGE_SETTINGS: readonly LanguageSetting[] = ['file', 'vscode'];

export type CatalaSettings = {
  language: LanguageSetting;
};

const defaultSettings: CatalaSettings = {
  language: 'file',
};

function primarySubtag(locale: string): string {
  return locale.split(/[-_]/)[0].toLowerCase();
}

export function fileLanguage(file: string | undefined): string | undefined {
  const match = file?.match(/\.catala_([a-z]+)(?:\.md)?$/i);
  const language = match?.[1].toLowerCase();
  return language !== undefined && language in allMessages
    ? language
    : undefined;
}

export function resolveLanguage(
  settings: CatalaSettings,
  file: string | undefined,
  locale: string
): string {
  switch (settings.language) {
    case 'file':
      return fileLanguage(file) ?? primarySubtag(locale);
    case 'vscode':
      return primarySubtag(locale);
  }
}

export function readSettings(stored: unknown): CatalaSettings {
  if (stored === null || typeof stored !== 'object') {
    return defaultSettings;
  }
  const language = (stored as { language?: unknown }).language;
  return {
    language: LANGUAGE_SETTINGS.includes(language as LanguageSetting)
      ? (language as LanguageSetting)
      : defaultSettings.language,
  };
}
