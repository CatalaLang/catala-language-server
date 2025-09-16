import type {
  Test,
  TestList,
  TestOutputs,
  TestIo,
  RuntimeValue,
  Diff,
  RuntimeValueRaw,
} from './generated/test_case';

export function renameIfNeeded(currentTests: TestList, newTest: Test): Test {
  const testNames = new Set(currentTests.map((test) => test.testing_scope));

  newTest.testing_scope = rename(testNames, newTest.testing_scope);
  return newTest;
}

function rename(testNames: Set<string>, newTestName: string): string {
  if (!testNames.has(newTestName)) {
    return newTestName;
  }

  const regex = /^(.+)_(\d+)$/;
  const match = newTestName.match(regex);

  if (match) {
    const baseName = match[1];
    let counter = parseInt(match[2], 10);

    while (testNames.has(`${baseName}_${counter}`)) {
      counter++;
    }

    return `${baseName}_${counter}`;
  } else {
    let counter = 1;
    while (testNames.has(`${newTestName}_${counter}`)) {
      counter++;
    }

    return `${newTestName}_${counter}`;
  }
}

export function omitPositionInfo(testOutputs: TestOutputs): TestOutputs {
  const result: TestOutputs = new Map();

  for (const [key, testIo] of testOutputs.entries()) {
    const newTestIo: TestIo = { ...testIo };

    if (newTestIo.value) {
      newTestIo.value = {
        ...newTestIo.value,
        pos: undefined, // Remove position information
      };
    }

    result.set(key, newTestIo);
  }

  return result;
}

/**
 * WARNING: This function needs significant improvement!
 * It only handles basic atomic values and doesn't properly format complex types.
 *
 * Renders a runtime value as a string for display purposes.
 */
export function renderAtomicValue(value: RuntimeValue): string {
  const raw = value.value;
  switch (raw.kind) {
    case 'Bool':
      return raw.value ? 'true' : 'false';
    case 'Integer':
      return raw.value.toString();
    case 'Decimal':
      return raw.value.toString();
    case 'Money':
      return (raw.value / 100).toFixed(2);
    case 'Date': {
      const date = raw.value;
      return `${date.year}-${String(date.month).padStart(2, '0')}-${String(date.day).padStart(2, '0')}`;
    }
    case 'Duration': {
      const d = raw.value;
      return `${d.years}y ${d.months}m ${d.days}d`;
    }
    // Complex types just get a placeholder
    case 'Enum':
      return `${raw.value[1][0]}`;
    case 'Struct':
      return 'Struct value';
    case 'Array':
      return `Array(${raw.value.length})`;
    default:
      return 'Unknown value';
  }
} /**
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
export function isAtomicRaw(value: RuntimeValueRaw): boolean {
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
