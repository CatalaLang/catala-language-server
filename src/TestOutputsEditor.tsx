import type { ReactElement } from 'react';

/* An editor for test outputs. Outputs are named and typed, and
 are *always* optional -- each defined output corresponds to
 an assertion, and each test contains zero or more assertions.

 Two data sources are used for producing this component:
   - `tested_scope.outputs` is the set of all *possible* assertions
   (names and types, no value)
   - `test_outputs` (which may be better named `test_assertions`?) is the set of
   checked assertions; the names in `test_outputs` is/should be a subset of `tested_scope.outputs`
   - `test_outputs` is a named set of TestIo ; As far as I can tell, values should be mandatory
     (although for records we should be able to test partial values, and for tuples and arrays, 
      I do not know what the 'right' answer is).

 This component aggregates all outputs (assertions)
 for a single test and offers an interface for CRUD operations
 on assertions.

 We will also need a 'reset assertions to current output value'
 which will run the test and fill assertions values with the scope output.

 */
export default function TestOutputsEditor(): ReactElement {
  return (
    <div>
      <strong>Outputs/Assertions</strong>
    </div>
  );
}
