import type { RuntimeValue, RuntimeValueRaw } from '../generated/test_case';

/**
 * Returns true for primitive-like runtime values (used by diff display logic)
 */
export function isAtomicRuntime(rv: RuntimeValue): boolean {
  switch (rv.value.kind) {
    case 'Bool':
    case 'Money':
    case 'Integer':
    case 'Decimal':
    case 'Date':
    case 'Duration':
    case 'Empty':
      return true;
    default:
      return false;
  }
}

/**
 * Returns true if a RuntimeValueRaw is atomic (i.e., not a container).
 * Enums are atomic if their payload is atomic (or absent).
 */
export function isAtomicRaw(value: RuntimeValueRaw): boolean {
  if (!['Enum', 'Struct', 'Array', 'Tuple'].includes(value.kind)) {
    return true;
  }
  if (value.kind === 'Enum') {
    const payload = value.value[1][1];
    if (payload === null) {
      return true;
    }
    return isAtomicRaw(payload.value.value);
  }
  return false;
}
