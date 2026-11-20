import { describe, expect, it } from 'vitest';
import {
  matchesFilenamePattern,
  mostCommonLanguage,
} from '../../src/shared/testFileName';

describe('matchesFilenamePattern', () => {
  const pattern = '*test*.catala_*';

  it('accepts what the test editor opens', () => {
    expect(matchesFilenamePattern('test.catala_fr', pattern)).toBe(true);
    expect(matchesFilenamePattern('my_TEST_2.catala_en', pattern)).toBe(true);
    expect(matchesFilenamePattern('tests.catala_pl', pattern)).toBe(true);
  });

  it('refuses what it would not open', () => {
    expect(matchesFilenamePattern('benefit.catala_fr', pattern)).toBe(false);
    expect(matchesFilenamePattern('test.txt', pattern)).toBe(false);
    expect(matchesFilenamePattern('test.catala', pattern)).toBe(false);
  });

  it('takes the dots of the pattern literally', () => {
    expect(matchesFilenamePattern('testXcatala_fr', pattern)).toBe(false);
  });
});

describe('mostCommonLanguage', () => {
  it('picks the language of most files', () => {
    expect(
      mostCommonLanguage([
        'a.catala_fr',
        'b.catala_fr.md',
        'c.catala_en',
        'notes.md',
      ])
    ).toBe('fr');
  });

  it('is undefined without Catala files', () => {
    expect(mostCommonLanguage(['README.md'])).toBeUndefined();
  });
});
