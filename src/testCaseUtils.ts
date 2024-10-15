import type {
  Test,
  TestList,
  TestOutputs,
  TestIo,
  RuntimeValue,
  StructDeclaration,
  EnumDeclaration,
  Option,
} from './generated/test_case';
import { assertUnreachable } from './util';

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
 * Looks at a list of expected results (assertions) and a superset
 * list of actual results, and returns the actual results for which
 * an expectation is defined. Position information is omitted from
 * the diff (revisit later?)
 * @param expected
 * @param actual
 * @returns the actual results for which an expectation is defined
 */
export function select(
  expected: TestOutputs,
  actual: TestOutputs
): { expected: TestOutputs; actual: TestOutputs } {
  expected = omitPositionInfo(expected);
  actual = omitPositionInfo(actual);

  const selectedActual: TestOutputs = new Map();

  for (const [key, expectedValue] of expected.entries()) {
    if (!actual.has(key)) {
      throw new Error(
        `Expected output '${key}' is not present in actual results`
      );
    }

    const actualValue = actual.get(key)!;
    selectedActual.set(key, selectTestIo(expectedValue, actualValue));
  }

  return { expected, actual: selectedActual };
}

function selectTestIo(expected: TestIo, actual: TestIo): TestIo {
  if (!expected.value) {
    return { typ: expected.typ };
  }

  if (!actual.value) {
    throw new Error(`Expected value is defined, but actual value is not`);
  }

  return {
    typ: expected.typ,
    value: {
      value: selectRuntimeValue(expected.value.value, actual.value.value),
      pos: actual.value.pos,
    },
  };
}

function selectRuntimeValue(
  expected: RuntimeValue,
  actual: RuntimeValue
): RuntimeValue {
  if (expected.kind !== actual.kind) {
    throw new Error(
      `Mismatch in type: expected ${expected.kind}, got ${actual.kind}`
    );
  }

  switch (expected.kind) {
    case 'Bool':
    case 'Money':
    case 'Integer':
    case 'Decimal':
    case 'Date':
    case 'Duration':
      return actual;
    case 'Enum': {
      const actualValue = actual.value as [
        EnumDeclaration,
        [string, Option<RuntimeValue>],
      ]; //XXX type coercion
      if (expected.value[1][0] !== actualValue[1][0]) {
        throw new Error(
          `Mismatch in enum constructor: expected ${expected.value[1][0]}, got ${actualValue[1][0]}`
        );
      }
      if (expected.value[1][1] && actualValue[1][1]) {
        return {
          kind: 'Enum',
          value: [
            actualValue[0],
            [
              actualValue[1][0],
              {
                value: selectRuntimeValue(
                  expected.value[1][1].value,
                  actualValue[1][1].value
                ),
              },
            ],
          ],
        };
      }
      return actual;
    }
    case 'Struct': {
      const actualValue = actual.value as [
        StructDeclaration,
        Map<string, RuntimeValue>,
      ]; //XXX type coercion
      return {
        kind: 'Struct',
        value: [
          actualValue[0],
          selectStruct(expected.value[0], expected.value[1], actualValue[1]),
        ],
      };
    }
    case 'Array':
      return {
        kind: 'Array',
        value: actual.value as RuntimeValue[],
      };
    default:
      assertUnreachable(expected);
  }
}

function selectStruct(
  structDecl: StructDeclaration,
  expected: Map<string, RuntimeValue>,
  actual: Map<string, RuntimeValue>
): Map<string, RuntimeValue> {
  const selectedStruct = new Map<string, RuntimeValue>();

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  for (const [fieldName, _fieldType] of structDecl.fields.entries()) {
    const expectedField = expected.get(fieldName);
    const actualField = actual.get(fieldName);

    if (expectedField) {
      if (!actualField) {
        throw new Error(
          `Expected field '${fieldName}' is not present in actual results`
        );
      }
      selectedStruct.set(
        fieldName,
        selectRuntimeValue(expectedField, actualField)
      );
    }
  }

  return selectedStruct;
}
