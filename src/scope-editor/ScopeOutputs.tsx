import { type ReactElement } from 'react';
import ValueEditor from '../editors/ValueEditors';
import type { TestRunState } from './ScopeInputEditor';

type Props = {
  test_run_output: TestRunState;
};

export default function ScopeOutputs({ test_run_output }: Props): ReactElement {
  switch (test_run_output.status) {
    case 'success': {
      const outputs = test_run_output.results.test_outputs;
      let items = Array.from(outputs, ([outputName, v]) => {
        return (
          <>
            <span>{outputName}</span>
            <ValueEditor
              testIO={v}
              onValueChange={() => {}}
              currentPath={[]}
              diffs={[]}
              editable={false}
            />
          </>
        );
      });
      return <>{items}</>;
    }
    case 'error':
      return (
        <div>
          Error on test run: <pre>{test_run_output.message}</pre>
        </div>
      );
    default:
      return <div>No results to display</div>;
  }
}
