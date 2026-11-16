import { describe, it, expect } from 'vitest';
import {
  fileLanguage,
  readSettings,
  resolveLanguage,
} from '../src/shared/settings';
import type { LanguageSetting } from '../src/shared/settings';

const at = (language: LanguageSetting, file?: string, locale = 'de') =>
  resolveLanguage({ language }, file, locale);

describe('resolveLanguage', () => {
  it('without a setting, takes the language of the file', () => {
    expect(resolveLanguage(readSettings({}), 'x.catala_fr', 'en')).toBe('fr');
    expect(resolveLanguage(readSettings(undefined), 'x.catala_en', 'fr')).toBe(
      'en'
    );
  });

  it('vscode follows the locale', () => {
    expect(at('vscode', 'x.catala_fr', 'en')).toBe('en');
    expect(at('vscode', 'x.catala_en', 'fr-FR')).toBe('fr');
  });

  it('reads unknown values as the default', () => {
    expect(readSettings({ language: 'fr' })).toEqual({ language: 'file' });
  });

  it('file takes the language of the file', () => {
    expect(at('file', '/a/b/x.catala_fr')).toBe('fr');
    expect(at('file', '/a/b/x.catala_en')).toBe('en');
    expect(at('file', '/a/b/x.catala_fr.md')).toBe('fr');
    expect(at('file', '/a/b/x.catala_en.md')).toBe('en');
  });

  it('file falls back to the locale when the file says nothing usable', () => {
    expect(at('file', '/a/b/x.catala_de', 'en')).toBe('en');
    expect(at('file', '/a/b/notes.md', 'fr')).toBe('fr');
    expect(at('file', undefined, 'fr')).toBe('fr');
  });

  it('reads the file language on its own', () => {
    expect(fileLanguage('x.catala_EN')).toBe('en');
    expect(fileLanguage('x.catala_pl')).toBe('pl');
    expect(fileLanguage('x.catala_de')).toBeUndefined();
    expect(fileLanguage(undefined)).toBeUndefined();
    // The suffix has to end the name, not merely appear in it.
    expect(fileLanguage('x.catala_fr.backup')).toBeUndefined();
  });
});
