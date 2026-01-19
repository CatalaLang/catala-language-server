import { type ReactElement } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import { getDefaultValue } from '../editors/ValueEditors';
import ValueEditor from '../editors/ValueEditors';
import type { Test, TestIo, Diff, PathSegment, TestRunOutput, TestRunResults } from '../generated/catala_types';

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
  test_run_output: TestRunResults;
};

export default function ScopeOutputsEditor({
  test_run_output
}: Props): ReactElement {
  const intl = useIntl();
  console.log("NEW OUTPUT")
  if (test_run_output && test_run_output.kind == "Ok") {      console.log(test_run_output)

    let items =
      Array.from(test_run_output.value.test_outputs, ([outputName, v]) => {
        const outputData = test_run_output.value.test_outputs.get(outputName);
        return (
          <>
            <span>{outputName}</span>
            <ValueEditor
              testIO={v}
              onValueChange={() => { }}
              currentPath={[]}
              diffs={[]}
            />
          </>
        );

      });
    return (
      <div className="test-outputs-editor">
        <div className="test-outputs data-card">
          {items}
        </div>
      </div>
    );
  } else {
    if (!test_run_output) { console.log("CACA") } else {
      console.log(test_run_output)
    }

    return <div>CACA</div>
  }
}
