import type { TestDebugger } from '../generated/catala_types';
import type { TestMacro, TestState } from './types';

// The `gui` flag is now carried by the `test` union's constructor: a GUI
// entry wraps a full `Test`, a plain entry wraps a `TestSum`. These helpers
// read the fields that both variants share, plus the run metadata that lives
// on the `TestDebugger` wrapper.
export function isGui(test: TestDebugger): boolean {
  return test.test.kind === 'GUI';
}

export function testTitle(test: TestDebugger): string {
  return test.test.kind === 'GUI'
    ? (test.test.value.title ?? '')
    : test.test.value.scope;
}

export function testDescription(test: TestDebugger): string {
  return test.test.kind == 'GUI'
    ? (test.test.value.description ?? '')
    : `Test of ${test.test.value.scope} in ${test.filename}`;
}

export function testingScope(test: TestDebugger): string {
  return test.test.kind == 'GUI'
    ? test.test.value.scope_tested
    : test.test.value.scope;
}

export function testMacro(success?: boolean): TestState {
  return success == true
    ? 'Success'
    : success == undefined
      ? 'Unknown'
      : 'Failed';
}

/**
 * Function to verify if a test passes all the filters
 * @param test checks the filters on this test
 * @param index the index of the test so that the user
 * can also search on the index
 * @param filterBar the content of the search bar
 * @param filterScope the different Scopes that are filtered
 * @param filterGui tells if we want tests that are generated from the
 * testcase editor
 * @returns true if the tests matches conditions depending on the filters
 */
export function matchFilter(
  test: TestDebugger,
  filterRaw: string,
  filterScope: string[],
  filterGui: boolean
): boolean {
  let filter = filterRaw.toLowerCase();
  let searchBarFilter =
    testTitle(test).toLowerCase().includes(filter) ||
    testDescription(test).toLowerCase().includes(filter) ||
    testingScope(test).toLowerCase().includes(filter) ||
    (test.index + 1).toString().includes(filter);
  let scopeFilter =
    filterScope.length == 0
      ? true
      : filterScope.some((value) => testingScope(test) == value);
  let guiFilter = filterGui ? isGui(test) : true;
  return searchBarFilter && scopeFilter && guiFilter;
}

export function noFilter(
  filter: string,
  filterScope: string[],
  filterGui: boolean
): boolean {
  return filter.trim() == '' && filterScope.length == 0 && filterGui == false;
}

export function hasFailure(tests: TestMacro[]): boolean {
  return tests.some((test) => test.success !== undefined && !test.success);
}

export function scopesFromTests(tests: TestMacro[]): string[] {
  let allScopes = tests?.map((test) => testingScope(test)).sort();
  let scopes = [];
  let prev = '';
  for (let index = 0; index < allScopes!.length; index++) {
    const element = allScopes![index];
    if (prev != element) {
      scopes.push(element);
      prev = element;
    }
  }
  return scopes;
}
