import { type RuntimeValueRaw, type Diff } from './generated/test_case';

// For exhaustiveness checks
export function assertUnreachable(x: never): never {
  throw new Error(`Unexpected value: ${x}`);
}

/**
 * Tells whether the diff is elemental (inline-displayable).
 * Rules:
 * - Empty vs atomic -> elemental
 * - Same kind and atomic on both sides -> elemental
 * - Otherwise -> non-elemental (complex)
 */
export function isElemental(diff: Diff): boolean {
  const e = diff.expected.value;
  const a = diff.actual.value;

  // If one side is Empty and the other is atomic, treat as elemental
  if (e.kind === 'Empty' && a.kind !== 'Empty') {
    return isAtomicRaw(a);
  }
  if (a.kind === 'Empty' && e.kind !== 'Empty') {
    return isAtomicRaw(e);
  }

  // Different kinds (not an Empty vs atomic case) -> complex
  if (e.kind !== a.kind) {
    return false;
  }

  // Same kind: elemental only if atomic
  return isAtomicRaw(e) && isAtomicRaw(a);
}

function isAtomicRaw(value: RuntimeValueRaw): boolean {
  // Atomic kinds are everything except containers; Enums are atomic only if no payload
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
