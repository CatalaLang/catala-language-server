import { describe, it, expect } from 'vitest';
import { splitOnTerms } from '../src/shared/util';

/** Renders the chunks as a single string, matches shown between brackets. */
function render(text: string, terms: string[]): string {
  return splitOnTerms(text, terms)
    .map((chunk) => (chunk.match ? `[${chunk.text}]` : chunk.text))
    .join('');
}

describe('splitOnTerms', () => {
  it('leaves the text alone when there is nothing to search', () => {
    expect(splitOnTerms('calcul', [])).toEqual([
      { text: 'calcul', match: false },
    ]);
    expect(splitOnTerms('calcul', [''])).toEqual([
      { text: 'calcul', match: false },
    ]);
  });

  it('marks the matched substring', () => {
    expect(render('Test of calcul_impot', ['calcul'])).toBe(
      'Test of [calcul]_impot'
    );
  });

  it('matches regardless of case', () => {
    expect(render('Impot sur le revenu', ['IMPOT'])).toBe(
      '[Impot] sur le revenu'
    );
  });

  it('marks every occurrence', () => {
    expect(render('impot, impot, impot', ['impot'])).toBe(
      '[impot], [impot], [impot]'
    );
  });

  it('handles a match at either end without emitting empty chunks', () => {
    expect(render('calcul du reste', ['calcul'])).toBe('[calcul] du reste');
    expect(render('reste du calcul', ['calcul'])).toBe('reste du [calcul]');
    expect(render('calcul', ['calcul'])).toBe('[calcul]');
    expect(splitOnTerms('calcul', ['calcul'])).toEqual([
      { text: 'calcul', match: true },
    ]);
  });

  it('handles adjacent matches', () => {
    expect(render('abab', ['ab'])).toBe('[ab][ab]');
  });

  it('takes several terms into account', () => {
    expect(render('abc def ghi', ['abc', 'ghi'])).toBe('[abc] def [ghi]');
  });

  it('treats the terms as plain text, not as regexps', () => {
    // Would match "1x2" too if the dot kept its regexp meaning
    expect(render('version 1.2 et 1x2', ['1.2'])).toBe('version [1.2] et 1x2');
    expect(render('f(x) et fx', ['f(x)'])).toBe('[f(x)] et fx');
    expect(render('a+b', ['a+b'])).toBe('[a+b]');
    // An unescaped backslash would make the regexp invalid and throw
    expect(render('a\\b', ['\\'])).toBe('a[\\]b');
  });

  it('reports no match when the term is absent', () => {
    expect(splitOnTerms('rien a voir', ['zzz'])).toEqual([
      { text: 'rien a voir', match: false },
    ]);
  });
});
