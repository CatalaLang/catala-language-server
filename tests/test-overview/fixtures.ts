/** Tests as the extension host lists them to the test overview panel. */
import type { ListedTest } from '../../src/generated/catala_types';

// The id the host gives a test: `TestId` in testAndCoverage.ts.
const testId = (file: string, scope: string): string => `${file}:${scope}`;

/** A test written with the Catala test case editor. */
export function guiTest({
  file,
  scope,
  tested,
  title,
  description,
}: {
  file: string;
  scope: string;
  tested: string;
  title?: string;
  description?: string;
}): ListedTest {
  return {
    test_id: testId(file, scope),
    filename: file,
    test: {
      kind: 'GUI',
      value: { scope_tested: tested, scope, title, description },
    },
  };
}

/** A test written by hand: a `#[test]` scope. */
export function plainTest({
  file,
  scope,
}: {
  file: string;
  scope: string;
}): ListedTest {
  return {
    test_id: testId(file, scope),
    filename: file,
    test: { kind: 'Test', value: { scope } },
  };
}
