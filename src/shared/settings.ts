export type LanguageSetting = 'default' | 'en' | 'fr' | 'pl' | 'file';

const LANGUAGE_SETTINGS: readonly LanguageSetting[] = [
  'default',
  'en',
  'fr',
  'pl',
  'file',
];

export type CatalaSettings = {
  language: LanguageSetting;
};

export const defaultSettings: CatalaSettings = {
  language: 'default',
};

export type SettingsMessage = {
  kind: 'catalaSettings';
  value: CatalaSettings;
  locale: string;
};

export function isSettingsMessage(data: unknown): data is SettingsMessage {
  return (
    data !== null &&
    typeof data === 'object' &&
    (data as { kind?: unknown }).kind === 'catalaSettings'
  );
}

const SUPPORTED_LANGUAGES = ['en', 'fr', 'pl'];

function primarySubtag(locale: string): string {
  return locale.split(/[-_]/)[0].toLowerCase();
}

export function fileLanguage(file: string | undefined): string | undefined {
  const match = file?.match(/\.catala_([a-z]+)(?:\.md)?$/i);
  const language = match?.[1].toLowerCase();
  return language !== undefined && SUPPORTED_LANGUAGES.includes(language)
    ? language
    : undefined;
}

export function resolveLanguage(
  settings: CatalaSettings,
  file: string | undefined,
  locale: string
): string {
  switch (settings.language) {
    case 'en':
    case 'fr':
    case 'pl':
      return settings.language;
    case 'file':
      return fileLanguage(file) ?? primarySubtag(locale);
    case 'default':
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
