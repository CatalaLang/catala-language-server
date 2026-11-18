/** The pure part of the test overview panel: filters, scopes, states. */
import { describe, it, expect } from 'vitest';
import type { TestDebugger } from '../../src/generated/catala_types';
import {
  hasFailure,
  matchFilter,
  noFilter,
  scopesFromTests,
  testDescription,
  testFileName,
  testingScope,
  testMacro,
  testTitle,
} from '../../src/test-overview/testUtils';
import { guiTest, plainTest } from './fixtures';

const allowance: TestDebugger = guiTest({
  file: '/work/tests/test_allowance.catala_en',
  scope: 'TestAllowance',
  tested: 'Allowance',
  title: 'Two children',
  description: 'A household with two children',
});
const tax: TestDebugger = plainTest({
  file: 'C:\\work\\tests\\test_tax.catala_en',
  scope: 'TestTax',
});

describe('test fields', () => {
  it('a GUI test shows its title and description, and the scope it tests', () => {
    expect(testTitle(allowance)).toBe('Two children');
    expect(testDescription(allowance)).toBe('A household with two children');
    expect(testingScope(allowance)).toBe('Allowance');
  });

  it('a plain test has no title nor description: its scope stands in', () => {
    expect(testTitle(tax)).toBe('TestTax');
    expect(testDescription(tax)).toContain('TestTax');
    expect(testingScope(tax)).toBe('TestTax');
  });

  it('the file name drops the directories, whatever the separator', () => {
    expect(testFileName(allowance)).toBe('test_allowance.catala_en');
    expect(testFileName(tax)).toBe('test_tax.catala_en');
  });
});

describe('matchFilter', () => {
  it('the search box looks into the title, the description and the scope', () => {
    expect(matchFilter(allowance, 'CHILDREN', [], false)).toBe(true);
    expect(matchFilter(allowance, 'household', [], false)).toBe(true);
    expect(matchFilter(allowance, 'allowance', [], false)).toBe(true);
    expect(matchFilter(allowance, 'pension', [], false)).toBe(false);
  });

  it('a number is looked for in the text, not in the position in the list', () => {
    expect(matchFilter(allowance, '1', [], false)).toBe(false);
  });

  it('selected scopes keep only their tests', () => {
    expect(matchFilter(allowance, '', ['Allowance'], false)).toBe(true);
    expect(matchFilter(tax, '', ['Allowance'], false)).toBe(false);
    expect(matchFilter(tax, '', ['Allowance', 'TestTax'], false)).toBe(true);
  });

  it('"GUI tests only" hides the plain tests', () => {
    expect(matchFilter(allowance, '', [], true)).toBe(true);
    expect(matchFilter(tax, '', [], true)).toBe(false);
  });

  it('every filter must match', () => {
    expect(matchFilter(allowance, 'children', ['TestTax'], false)).toBe(false);
  });
});

describe('noFilter', () => {
  it('holds only when nothing narrows the list', () => {
    expect(noFilter('  ', [], false)).toBe(true);
    expect(noFilter('a', [], false)).toBe(false);
    expect(noFilter('', ['Allowance'], false)).toBe(false);
    expect(noFilter('', [], true)).toBe(false);
  });
});

describe('scopesFromTests', () => {
  it('lists each tested scope once, sorted', () => {
    const other = guiTest({
      file: '/work/tests/test_allowance_2.catala_en',
      scope: 'TestAllowance2',
      tested: 'Allowance',
    });
    expect(scopesFromTests([tax, allowance, other])).toEqual([
      'Allowance',
      'TestTax',
    ]);
  });
});

describe('test state', () => {
  it('a test never run is unknown, not failed', () => {
    expect(testMacro(undefined)).toBe('Unknown');
    expect(testMacro(true)).toBe('Success');
    expect(testMacro(false)).toBe('Failed');
  });

  it('hasFailure counts only the tests that ran and failed', () => {
    expect(hasFailure([allowance, tax])).toBe(false);
    expect(
      hasFailure([
        allowance,
        {
          ...tax,
          last_run: { success: true, date: '2026-11-03T14:32:00.000Z' },
        },
      ])
    ).toBe(false);
    expect(
      hasFailure([
        allowance,
        {
          ...tax,
          last_run: { success: false, date: '2026-11-03T14:32:00.000Z' },
        },
      ])
    ).toBe(true);
  });
});
