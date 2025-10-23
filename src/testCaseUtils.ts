import type {
  Test,
  TestList,
  TestOutputs,
  TestIo,
  RuntimeValue,
} from './generated/test_case';
import { isAtomicRaw } from './diff/diff';

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
 * Renders a runtime value as a string for display purposes.
 *
 * Note: this function only handles basic atomic values and doesn't
 * properly format complex types.
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
    // Enums get their label and value if the underlying value type
    // is atomic, otherwise just their label
    case 'Enum':
      if (
        raw.value[1][1] == undefined ||
        !isAtomicRaw(raw.value[1][1].value.value)
      ) {
        return `${raw.value[1][0]}`;
      } else {
        return `${raw.value[1][0]} ➡ ${renderAtomicValue(raw.value[1][1].value)}  `;
      }
    // Complex types just get a placeholder or name
    case 'Struct':
      return raw.value[0].struct_name;
    case 'Array':
      return `Array(${raw.value.length})`;
    default:
      return 'Unknown value';
  }
}
