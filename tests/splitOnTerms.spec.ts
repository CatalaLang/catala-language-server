import { describe, it, expect } from 'vitest';
import type { Filter } from '../src/shared/util';
import { splitOnTerms } from '../src/shared/util';

/** The terms as the views hand them over: a pin, with what to do with it. */
function include(...terms: string[]): Filter[] {
  return terms.map((filter) => ({ filter, option: 'include' }));
}

/** Renders the chunks as a single string, matches shown between brackets. */
function render(text: string, terms: Filter[]): string {
  return splitOnTerms(text, terms)
    .map((chunk) => (chunk.match ? `[${chunk.text}]` : chunk.text))
    .join('');
}

describe('splitOnTerms', () => {
  it('leaves the text alone when there is nothing to search', () => {
    expect(splitOnTerms('calcul', [])).toEqual([
      { text: 'calcul', match: false },
    ]);
    expect(splitOnTerms('calcul', include(''))).toEqual([
      { text: 'calcul', match: false },
    ]);
  });

  it('marks the matched substring', () => {
    expect(render('Test of calcul_impot', include('calcul'))).toBe(
      'Test of [calcul]_impot'
    );
  });

  it('matches regardless of case', () => {
    expect(render('Impot sur le revenu', include('IMPOT'))).toBe(
      '[Impot] sur le revenu'
    );
  });

  it('marks every occurrence', () => {
    expect(render('impot, impot, impot', include('impot'))).toBe(
      '[impot], [impot], [impot]'
    );
  });

  it('handles a match at either end without emitting empty chunks', () => {
    expect(render('calcul du reste', include('calcul'))).toBe(
      '[calcul] du reste'
    );
    expect(render('reste du calcul', include('calcul'))).toBe(
      'reste du [calcul]'
    );
    expect(render('calcul', include('calcul'))).toBe('[calcul]');
    expect(splitOnTerms('calcul', include('calcul'))).toEqual([
      { text: 'calcul', match: true },
    ]);
  });

  it('handles adjacent matches', () => {
    expect(render('abab', include('ab'))).toBe('[ab][ab]');
  });

  it('takes several terms into account', () => {
    expect(render('abc def ghi', include('abc', 'ghi'))).toBe(
      '[abc] def [ghi]'
    );
  });

  it('treats the terms as plain text, not as regexps', () => {
    // Would match "1x2" too if the dot kept its regexp meaning
    expect(render('version 1.2 et 1x2', include('1.2'))).toBe(
      'version [1.2] et 1x2'
    );
    expect(render('f(x) et fx', include('f(x)'))).toBe('[f(x)] et fx');
    expect(render('a+b', include('a+b'))).toBe('[a+b]');
    // An unescaped backslash would make the regexp invalid and throw
    expect(render('a\\b', include('\\'))).toBe('a[\\]b');
  });

  it('reports no match when the term is absent', () => {
    expect(splitOnTerms('rien a voir', include('zzz'))).toEqual([
      { text: 'rien a voir', match: false },
    ]);
  });

  // Only an inclusion says what the user is looking for; the other two say
  // what to leave out of the tree, and highlighting them would be misleading.
  it('highlights inclusions only', () => {
    const pin = (option: Filter['option']): Filter[] => [
      { filter: 'impot', option },
    ];
    expect(render('calcul impot', pin('include'))).toBe('calcul [impot]');
    expect(render('calcul impot', pin('exclude'))).toBe('calcul impot');
    expect(render('calcul impot', pin('ignore'))).toBe('calcul impot');
  });

  it('ignores the non-inclusions when picking the terms', () => {
    const filters: Filter[] = [
      { filter: 'abc', option: 'include' },
      { filter: 'def', option: 'exclude' },
    ];
    expect(render('abc def', filters)).toBe('[abc] def');
  });
});
