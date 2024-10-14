import { type ReactElement } from 'react';
import { writeTestOutputs, type TestOutputs } from './generated/test_case';
import diff, { type Difference } from 'microdiff';

type Props = {
  expected: TestOutputs;
  actual: TestOutputs;
};

function renderDiff(diff: Difference[]): ReactElement {
  return (
    <>
      {diff.map((item, index) => {
        if (item.type === 'CHANGE') {
          return (
            <div key={index} className="diff-item">
              <span className="diff-path">{item.path.join('.')}</span>: expected{' '}
              <span className="diff-value">
                {JSON.stringify(item.oldValue)}
              </span>
              , but got{' '}
              <span className="diff-value">{JSON.stringify(item.value)}</span>
            </div>
          );
        }
        return null;
      })}
    </>
  );
}

export default function Results(props: Props): ReactElement {
  const diffResults = diff(
    writeTestOutputs(props.expected),
    writeTestOutputs(props.actual)
  );
  return (
    <div>
      {diffResults.length === 0 ? (
        <span>✅</span>
      ) : (
        <>
          <span>❌</span>
          {renderDiff(diffResults)}
        </>
      )}
    </div>
  );
}
