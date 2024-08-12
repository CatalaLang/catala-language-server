import type { ReactElement } from 'react';

// An editor for test outputs. Outputs are named and typed, and
// are *always* optional -- each defined output corresponds to
// an assertion, and each test contains zero or more assertions.
//
// This component aggregates all outputs (assertions)
// for a single test.
export default function TestOutputsEditor(): ReactElement {
  return (
    <div>
      <i>Outputs/Assertions</i>
    </div>
  );
}
