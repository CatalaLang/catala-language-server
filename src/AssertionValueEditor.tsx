import { type ReactElement } from 'react';
import { FormattedMessage } from 'react-intl';
import type { TestIo, Diff, PathSegment } from './generated/test_case';
import ValueEditor from './editors/ValueEditors';
import { renderAtomicValue } from './testCaseUtils';
import './styles/assertions-editor.css';

type Props = {
  testIO: TestIo;
  onValueChange: (newValue: TestIo) => void;
  onAssertionDeletion: () => void;
  diffs?: Diff[];
  currentPath?: PathSegment[];
};

/**
 * Creates a hook function that highlights editors with diffs
 */
function createDiffHighlightHook(
  diffs: Diff[],
  currentPath: PathSegment[] = []
) {
  return (editor: ReactElement): ReactElement => {
    // Check if current path matches any diff path
    const matchingDiff = diffs.find((diff) => {
      // For exact path match (atomic values)
      if (diff.path.length === currentPath.length) {
        return currentPath.every(
          (segment, i) =>
            JSON.stringify(segment) === JSON.stringify(diff.path[i])
        );
      }
      return false;
    });

    // Check if this is a parent of a diff path (for containers)
    const isParentOfDiff =
      !matchingDiff &&
      diffs.some((diff) => {
        // If the diff path is longer than current path, check if current path is a prefix
        if (diff.path.length > currentPath.length) {
          return currentPath.every(
            (segment, i) =>
              JSON.stringify(segment) === JSON.stringify(diff.path[i])
          );
        }
        return false;
      });

    if (matchingDiff) {
      // For atomic values, show expected vs actual
      return (
        <div className="diff-highlight atomic-diff">
          <div className="diff-expected">
            <FormattedMessage id="diff.expected" />:{' '}
            {renderAtomicValue(matchingDiff.expected)}
          </div>
          <div className="diff-actual">
            <FormattedMessage id="diff.actual" />:{' '}
            {renderAtomicValue(matchingDiff.actual)}
          </div>
          {editor}
        </div>
      );
    } else if (isParentOfDiff) {
      // For containers that have diffs somewhere inside them
      return <div className="diff-highlight container-diff">{editor}</div>;
    }

    return editor;
  };
}

export default function AssertionValueEditor({
  testIO,
  onValueChange,
  onAssertionDeletion,
  diffs = [],
  currentPath = [],
}: Props): ReactElement {
  // Create the diff highlight hook if we have diffs
  const editorHook =
    diffs.length > 0 ? createDiffHighlightHook(diffs, currentPath) : undefined;

  return (
    <div className="assertion-value-editor">
      <ValueEditor
        testIO={testIO}
        onValueChange={onValueChange}
        editorHook={editorHook}
      />
      <button
        className="assertion-delete"
        title="Delete assertion"
        onClick={onAssertionDeletion}
      >
        <span className="codicon codicon-trash"></span>
      </button>
    </div>
  );
}

export { createDiffHighlightHook };
