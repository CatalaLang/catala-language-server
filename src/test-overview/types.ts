import { type WebviewApi } from 'vscode-webview';

import type { ListedTest } from '../generated/catala_types';

export type TestState = 'Success' | 'Loading' | 'Failed' | 'Unknown';

// [loading]: a run of the test was asked for and its result has not come back
// yet. Kept here rather than in the components that show the test, which do
// not see the result come back.
export type OverviewTest = ListedTest & { loading?: boolean };

export type TestItemArg = {
  vscode: WebviewApi<unknown>;
  test: OverviewTest;
  onRun: (testId: string) => void;
};

export type CardGridArg = {
  vscode: WebviewApi<unknown>;
  filteredScope: string[];
  tests: OverviewTest[];
  onRun: (testId: string) => void;
};
