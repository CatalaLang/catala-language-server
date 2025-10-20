import type { RuntimeValue } from '../generated/test_case';

// Returns true for primitive-like runtime values (used by diff display logic)
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
