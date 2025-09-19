import { type ReactElement } from 'react';
import { FormattedMessage } from 'react-intl';
import type {
  TestIo,
  Diff,
  PathSegment,
  RuntimeValue,
} from './generated/test_case';
import ValueEditor from './editors/ValueEditors';
import { renderAtomicValue } from './testCaseUtils';
import './styles/assertions-editor.css';
import { findMatchingDiff, isParentOfAnyDiff } from './diff/highlight';

type Props = {
  testIO: TestIo;
  onValueChange: (newValue: TestIo) => void;
  onAssertionDeletion: () => void;
  diffs?: Diff[];
  currentPath: PathSegment[];
  highlightDiffs?: boolean;
};

/**
 * Renders an empty value indicator for array elements
 */
function renderEmptyValueIndicator(isExpected: boolean): ReactElement {
  return (
    <div
      className={`empty-value-indicator ${isExpected ? 'expected' : 'actual'}`}
    >
      <FormattedMessage
        id={isExpected ? 'diff.emptyExpected' : 'diff.emptyActual'}
        defaultMessage="Empty"
      />
    </div>
  );
}

/**
 * Checks if a runtime value represents an empty value
 */
function isEmptyValue(value: RuntimeValue): boolean {
  return value.value.kind === 'Empty';
}

/**
 * Creates a hook function that highlights editors with diffs
 */
function createDiffHighlightHook(diffs: Diff[]) {
  return (editor: ReactElement, path: PathSegment[]): ReactElement => {
    // Check if current path matches any diff path (exact match)
    const matchingDiff = findMatchingDiff(diffs, path);

    // Check if this is a parent of a diff path (for containers)
    const isParentOfDiff = !matchingDiff && isParentOfAnyDiff(diffs, path);

    if (matchingDiff) {
      const expectedIsEmpty = isEmptyValue(matchingDiff.expected);
      const actualIsEmpty = isEmptyValue(matchingDiff.actual);

      if (expectedIsEmpty || actualIsEmpty) {
        return (
          <div className="diff-highlight atomic-diff">
            {expectedIsEmpty ? (
              <>
                {renderEmptyValueIndicator(true)}
                <div className="diff-actual">
                  <FormattedMessage id="diff.actual" />:{' '}
                  {renderAtomicValue(matchingDiff.actual)}
                </div>
              </>
            ) : (
              <>
                <div className="diff-expected">
                  <FormattedMessage id="diff.expected" />:{' '}
                  {renderAtomicValue(matchingDiff.expected)}
                </div>
                {renderEmptyValueIndicator(false)}
              </>
            )}
            {editor}
          </div>
        );
      } else {
        // For regular atomic values, show expected vs actual
        return (
          <div className="diff-highlight atomic-diff">
            {editor}
            <div className="diff-actual">
              <FormattedMessage id="diff.actual" />:{' '}
              {renderAtomicValue(matchingDiff.actual)}
            </div>
          </div>
        );
      }
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
  currentPath,
  highlightDiffs = true,
}: Props): ReactElement {
  // Array item phantom rendering is handled inside ArrayEditor based on diffs.

  // Create the diff highlight hook if we have diffs
  const editorHook =
    diffs.length > 0 && highlightDiffs
      ? createDiffHighlightHook(diffs)
      : undefined;

  return (
    <div className="assertion-value-editor">
      <ValueEditor
        testIO={testIO}
        onValueChange={onValueChange}
        editorHook={editorHook}
        currentPath={currentPath}
        diffs={diffs}
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
