import { describe, it, expect } from 'vitest';
import type { Typ } from '../../src/generated/catala_types';
import { replaceLosses } from '../../src/test-case-editor/testCaseUtils';
import { intVal, ioOf, recoveryOf, rv, testOf } from './test-helpers';

const int: Typ = { kind: 'TInt' };
const none = { dropped_assertions: [], unset_fields: [], dropped_tests: [] };

describe('replaceLosses', () => {
  it('names an assertion the rebuild no longer has', () => {
    const authored = testOf(
      'T',
      {},
      { total: ioOf(int, { value: intVal(3) }) }
    );
    const rebuilt = testOf('T', {}, { total: ioOf(int) });
    expect(replaceLosses(recoveryOf([authored, rebuilt]), [rebuilt])).toEqual({
      ...none,
      dropped_assertions: ['total'],
    });
  });

  it('does not list an output the scope no longer has', () => {
    const authored = testOf('T', {}, { gone: ioOf(int, { value: intVal(3) }) });
    const rebuilt = testOf('T', {}, {});
    expect(replaceLosses(recoveryOf([authored, rebuilt]), [rebuilt])).toEqual(
      none
    );
  });

  it('names a field still blank, and nothing when all is filled', () => {
    const authored = testOf('T', { a: ioOf(int, { value: intVal(1) }) }, {});
    const blank = testOf(
      'T',
      { a: ioOf(int, { value: rv({ kind: 'Unset' }) }) },
      {}
    );
    expect(replaceLosses(recoveryOf([authored, blank]), [blank])).toEqual({
      ...none,
      unset_fields: ['a'],
    });
    const filled = testOf('T', { a: ioOf(int, { value: intVal(2) }) }, {});
    expect(replaceLosses(recoveryOf([authored, filled]), [filled])).toEqual(
      none
    );
  });

  it('names a test that has no rebuild at all', () => {
    const a = testOf('A', {}, {});
    const b = testOf('B', {}, { total: ioOf(int, { value: intVal(1) }) });
    expect(replaceLosses(recoveryOf([a, a], [b, undefined]), [a])).toEqual({
      ...none,
      dropped_tests: ['B'],
    });
  });

  it('qualifies names by scope when the file holds several tests', () => {
    const a = testOf('A', {}, { total: ioOf(int, { value: intVal(1) }) });
    const a2 = testOf('A', {}, { total: ioOf(int) });
    const b = testOf('B', {}, {});
    expect(
      replaceLosses(recoveryOf([a, a2], [b, b]), [a2, b]).dropped_assertions
    ).toEqual(['A.total']);
  });
});
