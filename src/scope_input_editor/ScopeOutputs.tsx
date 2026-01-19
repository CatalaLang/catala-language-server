import { type ReactElement } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import { getDefaultValue } from '../editors/ValueEditors';
import ValueEditor from '../editors/ValueEditors';
import type { Test, TestIo, Diff, PathSegment, TestRunOutput, TestRunResults } from '../generated/catala_types';
import { TestRunState } from './ScopeInputEditor';

/* An editor for test outputs. Outputs are named and typed, and
   are *always* optional -- each defined output corresponds to
   an assertion, and each test contains zero or more assertions. 

   Two data sources are used for producing this component:
    - `tested_scope.outputs` is the set of all *possible* assertions
    (names and types, no value)
    - `test_outputs` (which may be better named `test_assertions`?) is the set of
    checked assertions; the names in `test_outputs` is/should be a subset of `tested_scope.outputs`
    - `test_outputs` is a named set of TestIo ; Values are defined if an assertion is present,
    and undefined otherwise.

   This component aggregates all outputs (assertions)
   for a single test and offers an interface for CRUD operations
   on assertions.
*/

type Props = {
  test_run_output: TestRunState;
};

export default function ScopeOutputs({
  test_run_output
}: Props): ReactElement {
  const intl = useIntl();
  switch (test_run_output.status) {
    case 'success':
      const outputs = test_run_output.results.test_outputs;
      let items =
        Array.from(outputs, ([outputName, v]) => {
          return (
            <>
              <span>{outputName}</span>
              <ValueEditor
                testIO={v}
                onValueChange={() => { }}
                currentPath={[]}
                diffs={[]}
                editable={false}
              />
            </>
          );
        });
      return <>{items}</>;
    case 'error':
      return <div>Error on test run: <pre>{test_run_output.message}</pre></div>
    default:
      return <div>No results to display</div>
  }
}
