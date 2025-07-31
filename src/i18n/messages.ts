// Messages for VS Code extension (not React UI)
// This file contains localization strings only for the "shell" part of the extension
// (VS Code dialogs, notifications, etc.) that cannot use React-intl.
// The main UI localization happens in the src/locales/*.json files which are used by React-intl.
export const messages = {
  en: {
    resetOutputsConfirmation:
      'Replace expected outputs with test run results. Are you sure?',
    resetButton: 'Replace',
  },
  fr: {
    resetOutputsConfirmation:
      "Remplacer les sorties attendues par le résultat de l'exécution du test ?",
    resetButton: 'Remplacer',
  },
};

// Helper function to get localized messages based on language
export function getLocalizedMessages(language: string): Record<string, string> {
  // Default to English if language is not supported
  const lang = language.startsWith('fr') ? 'fr' : 'en';
  return messages[lang];
}
