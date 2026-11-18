/** Tests as the extension host lists them to the test overview panel. */
import type { TestDebugger } from '../../src/generated/catala_types';

// The id the host gives a test: `TestId` in testAndCoverage.ts.
const testId = (file: string, scope: string): string => `${file}:${scope}`;

/** A test written with the Catala test case editor. */
export function guiTest({
  file,
  scope,
  tested,
  title,
  description,
  success,
  date,
}: {
  file: string;
  scope: string;
  tested: string;
  title?: string;
  description?: string;
  success?: boolean;
  date?: string;
}): TestDebugger {
  return {
    test_id: testId(file, scope),
    filename: file,
    test: {
      kind: 'GUI',
      value: { scope_tested: tested, scope, title, description },
    },
    success,
    date,
  };
}

/** A test written by hand: a `#[test]` scope. */
export function plainTest({
  file,
  scope,
  success,
  date,
}: {
  file: string;
  scope: string;
  success?: boolean;
  date?: string;
}): TestDebugger {
  return {
    test_id: testId(file, scope),
    filename: file,
    test: { kind: 'Test', value: { scope } },
    success,
    date,
  };
}
