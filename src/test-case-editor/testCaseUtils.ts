import type {
  EnumDeclaration,
  RecoveredTest,
  Recovery,
  ReplaceLosses,
  ParseResults,
  Test,
  TestList,
  RuntimeValue,
} from '../generated/catala_types';
import { isAtomicRaw } from '../diff/diff';
import { countUnsetIn } from '../editors/unsetValidation';

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

/**
 * Renders a runtime value as a string for display purposes.
 *
 * Note: this function only handles basic atomic values and doesn't
 * properly format complex types.
 */
export function renderAtomicValue(
  value: RuntimeValue,
  formatBool: (b: boolean) => string = (b) => (b ? 'true' : 'false'),
  formatCtor: (decl: EnumDeclaration, name: string) => string = (_, name) =>
    name
): string {
  const raw = value.value;
  switch (raw.kind) {
    case 'Bool':
      return formatBool(raw.value);
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
        return formatCtor(raw.value[0], raw.value[1][0]);
      } else {
        return `${formatCtor(raw.value[0], raw.value[1][0])} ➡ ${renderAtomicValue(raw.value[1][1].value, formatBool, formatCtor)}  `;
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

/** What "Replace the original" would lose, by field name (scope-qualified
 *  when the file holds several tests). An output the live scope no longer has
 *  is not listed: the authored pane already says so, and nothing can hold it. */
export function replaceLosses(
  view: Recovery,
  rebuilt: TestList
): ReplaceLosses {
  const qualify = (scope: string, name: string): string =>
    view.tests.length > 1 ? `${scope}.${name}` : name;
  const droppedAssertions: string[] = [];
  const unsetFields: string[] = [];
  const droppedTests: string[] = [];
  for (const { authored } of view.tests) {
    const scope = authored.testing_scope;
    const live = rebuilt.find((t) => t.testing_scope === scope);
    if (live === undefined) {
      droppedTests.push(scope);
      continue;
    }
    for (const [name, io] of authored.test_outputs) {
      const out = live.test_outputs.get(name);
      if (
        io.value !== undefined &&
        out !== undefined &&
        out.value === undefined
      )
        droppedAssertions.push(qualify(scope, name));
    }
    for (const [name, io] of [...live.test_inputs, ...live.test_outputs]) {
      if (io.value !== undefined && countUnsetIn(io.value.value, io.typ) > 0)
        unsetFields.push(qualify(scope, name));
    }
  }
  return {
    dropped_assertions: droppedAssertions,
    unset_fields: unsetFields,
    dropped_tests: droppedTests,
  };
}

/**
 * The rebuild a recovery produced, if any: the right pane's state at open. The
 * webview only posts subsequent edits.
 */
export function rebuiltOf(tests: RecoveredTest[]): TestList {
  return tests.flatMap((t) => (t.rebuilt === undefined ? [] : [t.rebuilt]));
}

export function rebuiltFrom(results: ParseResults): TestList | undefined {
  return results.kind === 'BrokenTest'
    ? rebuiltOf(results.value.tests)
    : undefined;
}
