import { type RuntimeValueRaw, type Diff } from './generated/test_case';

// For exhaustiveness checks
export function assertUnreachable(x: never): never {
  throw new Error(`Unexpected value: ${x}`);
}

/**
 * Tells whether the diff is elemental, i.e.
 * not a whole structure, array or tuple
 * @param diff
 */
export function isElemental(diff: Diff): boolean {
  if (diff.expected.value.kind !== diff.actual.value.kind) {
    throw new Error('Type mismatch between expected and actual values');
  }

  return _isElemental(diff.expected.value) && _isElemental(diff.actual.value);
}

function _isElemental(value: RuntimeValueRaw): boolean {
  if (!['Enum', 'Struct', 'Array', 'Tuple'].includes(value.kind)) {
    return true;
  } else if (value.kind === 'Enum') {
    const underlying = value.value[1][1];
    if (underlying === null) {
      return true;
    }
    return _isElemental(underlying.value.value);
  }

  return false;
}
