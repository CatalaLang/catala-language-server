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

type Props = {
  testIO: TestIo;
  onValueChange: (newValue: TestIo) => void;
  onAssertionDeletion: () => void;
  diffs?: Diff[];
  currentPath: PathSegment[];
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
 * Compare two paths for exact equality.
 */
function pathEquals(a: PathSegment[], b: PathSegment[]): boolean {
  return (
    a.length === b.length &&
    a.every((seg, i) => JSON.stringify(seg) === JSON.stringify(b[i]))
  );
}

/**
 * Returns true if 'prefix' is a prefix of 'full'.
 */
function isPathPrefix(prefix: PathSegment[], full: PathSegment[]): boolean {
  if (prefix.length > full.length) return false;
  return prefix.every(
    (seg, i) => JSON.stringify(seg) === JSON.stringify(full[i])
  );
}

/**
 * Creates a hook function that highlights editors with diffs
 */
function createDiffHighlightHook(diffs: Diff[]) {
  return (editor: ReactElement, path: PathSegment[]): ReactElement => {
    // Check if current path matches any diff path (exact match)
    const matchingDiff = diffs.find((diff) => pathEquals(diff.path, path));

    // Check if this is a parent of a diff path (for containers)
    const isParentOfDiff =
      !matchingDiff &&
      diffs.some(
        (diff) =>
          diff.path.length > path.length && isPathPrefix(path, diff.path)
      );

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
}: Props): ReactElement {
  // Array item phantom rendering is handled inside ArrayEditor based on diffs.

  // Create the diff highlight hook if we have diffs
  const editorHook =
    diffs.length > 0 ? createDiffHighlightHook(diffs) : undefined;

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
