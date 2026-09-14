import { type ReactElement } from 'react';
import ValueEditor from '../editors/ValueEditors';
import { CompositeEditor, type EditorItem } from '../editors/CompositeEditor';
import type { TestRunState } from './ScopeInputEditor';

type Props = {
  test_run_output: TestRunState;
};

export default function ScopeOutputs({ test_run_output }: Props): ReactElement {
  switch (test_run_output.status) {
    case 'success': {
      const outputs = test_run_output.results.test_outputs;
      const items: EditorItem[] = Array.from(outputs, ([outputName, v]) => {
        const raw = v.value?.value.value;
        const count =
          v.typ.kind === 'TArray' && raw?.kind === 'Array'
            ? raw.value.length
            : undefined;
        return {
          key: outputName,
          label: outputName,
          type: v.typ,
          count,
          editor: (
            <ValueEditor
              testIO={v}
              onValueChange={() => {}}
              currentPath={[{ kind: 'StructField', value: outputName }]}
              diffs={[]}
              editable={false}
            />
          ),
        };
      });
      return <CompositeEditor items={items} atomicElements={true} />;
    }
    case 'error':
      return (
        <div className="scope-outputs-error">
          Error on test run: <pre>{test_run_output.message}</pre>
        </div>
      );
    default:
      return <div className="scope-outputs-empty">No results to display</div>;
  }
}
