// Messages for VS Code extension (not React UI)
// This file contains localization strings only for the "shell" part of the extension
// (VS Code dialogs, notifications, etc.) that cannot use React-intl.
// The main UI localization happens in the src/locales/*.json files which are used by React-intl.
export const messages = {
  en: {
    resetOutputsConfirmation:
      'Replace expected outputs with test run results. Are you sure?',
    resetButton: 'Replace',
    unsavedChangesWarning:
      'The file has unsaved changes. Save before running test?',
    saveAndRun: 'Save and Run',
    runWithoutSaving: 'Run Without Saving',
    runAgainstSavedContent:
      'Test will run against the saved file content, not your current changes.',
    continue: 'Continue',
    deleteArrayElementConfirmation: 'Delete this element?',
    deleteAssertionConfirmation: 'Delete this assertion?',
    deleteButton: 'Delete',
    true: 'true',
    false: 'false',
  },
  fr: {
    resetOutputsConfirmation:
      "Remplacer les sorties attendues par le résultat de l'exécution du test ?",
    resetButton: 'Remplacer',
    unsavedChangesWarning:
      "Le fichier contient des modifications non enregistrées. Enregistrer avant d'exécuter le test ?",
    saveAndRun: 'Enregistrer et exécuter',
    runWithoutSaving: 'Exécuter sans enregistrer',
    runAgainstSavedContent:
      "Le test s'exécutera sur la version enregistrée du fichier, pas sur vos modifications actuelles.",
    continue: 'Continuer',
    deleteArrayElementConfirmation: 'Supprimer cet élément ?',
    deleteAssertionConfirmation: 'Supprimer cette assertion ?',
    deleteButton: 'Supprimer',
    true: 'vrai',
    false: 'faux',
  },
};

// Helper function to get localized messages based on language
export function getLocalizedMessages(language: string): Record<string, string> {
  // Default to English if language is not supported
  const lang = language.startsWith('fr') ? 'fr' : 'en';
  return messages[lang];
}
