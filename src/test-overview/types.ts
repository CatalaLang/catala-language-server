import { type WebviewApi } from 'vscode-webview';

import type { TestDebugger } from '../generated/catala_types';

export type TestState = 'Success' | 'Loading' | 'Failed' | 'Unknown';

// [loading]: a run of the test was asked for and its result has not come back
// yet. Kept here rather than in the components that show the test, which do
// not see the result come back.
export type TestMacro = TestDebugger & { loading?: boolean };

export type TestItemArg = {
  vscode: WebviewApi<unknown>;
  test: TestMacro;
  onRun: (id: number) => void;
};

export type CardGridArg = {
  vscode: WebviewApi<unknown>;
  filteredScope: string[];
  tests: TestMacro[];
  onRun: (id: number) => void;
};
