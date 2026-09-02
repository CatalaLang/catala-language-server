/** The right pane's starting state: the webview posts only subsequent edits. */
import { describe, it, expect } from 'vitest';
import type {
  Recovery,
  ParseResults,
  Test,
} from '../../src/generated/catala_types';
import { rebuiltFrom } from '../../src/test-case-editor/testCaseUtils';

function test(scope: string): Test {
  return {
    testing_scope: scope,
    tested_scope: { module_name: 'B', name: 'C', inputs: [], outputs: [] },
    test_inputs: new Map(),
    test_outputs: new Map(),
    description: '',
    title: '',
  };
}

function view(rebuilt: Test[]): Recovery {
  return {
    original: [test('C_one')],
    rebuilt: rebuilt,
    notes: [],
    working_copy: 'x.catala_en.updated',
    carry_outcomes: [],
  };
}

describe('rebuiltFrom', () => {
  it('starts a broken test on the rebuild the recovery pass produced', () => {
    const results: ParseResults = {
      kind: 'BrokenTest',
      value: view([test('C_one')]),
    };
    expect(rebuiltFrom(results)?.length).toBe(1);
  });

  it('has nothing to offer when the rebuild is empty', () => {
    const results: ParseResults = { kind: 'BrokenTest', value: view([]) };
    expect(rebuiltFrom(results)).toEqual([]);
  });

  it('has nothing to offer for a test that is not broken', () => {
    const results: ParseResults = { kind: 'Results', value: [test('C_one')] };
    expect(rebuiltFrom(results)).toBeUndefined();
  });
});
