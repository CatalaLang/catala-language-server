import { type ReactElement } from 'react';
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
            Expected: {renderAtomicValue(matchingDiff.expected)}
          </div>
          <div className="diff-actual">
            Actual: {renderAtomicValue(matchingDiff.actual)}
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

/**
 * Creates a path segment for a struct field
 */
function createStructFieldSegment(fieldName: string): PathSegment {
  return { kind: 'StructField', value: fieldName };
}

/**
 * Creates a path segment for an array index
 */
function createArrayIndexSegment(index: number): PathSegment {
  return { kind: 'ListIndex', value: index };
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

// Export these utility functions for use in other components
export {
  createDiffHighlightHook,
  createStructFieldSegment,
  createArrayIndexSegment,
};
