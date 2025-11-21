import { type ReactElement } from 'react';
import { FormattedMessage } from 'react-intl';
import type { TestIo, Diff, PathSegment } from './generated/test_case';
import ValueEditor from './editors/ValueEditors';
import { renderAtomicValue } from './testCaseUtils';
import './styles/assertions-editor.css';
import { findMatchingDiff, isParentOfAnyDiff } from './diff/highlight';
import { isAtomicRuntime } from './diff/diff';

type Props = {
  testIO: TestIo;
  onValueChange: (newValue: TestIo) => void;
  diffs?: Diff[];
  currentPath: PathSegment[];
  highlightDiffs?: boolean;
  onDiffResolved?: (path: PathSegment[]) => void;
  onInvalidateDiffs?: (pathPrefix: PathSegment[]) => void;
};

/**
 * Creates a hook function that highlights editors with diffs
 */
function createDiffHighlightHook(diffs: Diff[]) {
  return (editor: ReactElement, path: PathSegment[]): ReactElement => {
    // Check if current path matches any diff path (exact match)
    const matchingDiff = findMatchingDiff(diffs, path);

    // Special-case: for enums where expected is a simple label (no payload)
    // and actual has a complex payload, let the EnumEditor render an actual
    // preview (read-only) instead of the atomic banner here.

    if (
      matchingDiff?.actual.value.kind === 'Enum' &&
      matchingDiff?.expected.value.kind === 'Enum'
    ) {
      const [, [, payload]] = matchingDiff.actual.value.value;
      const actualPayload = payload?.value;
      const expectedIsSimple = matchingDiff.expected.value.value[1][1] === null;
      const actualPayloadIsComplex =
        !!actualPayload && !isAtomicRuntime(actualPayload);
      if (expectedIsSimple && actualPayloadIsComplex) {
        // Defer to EnumEditor preview UI
        return editor;
      }
    }

    // Check if this is a parent of a diff path (for containers)
    const isParentOfDiff = !matchingDiff && isParentOfAnyDiff(diffs, path);

    if (matchingDiff) {
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
  diffs = [],
  currentPath,
  highlightDiffs = true,
  onDiffResolved,
  onInvalidateDiffs,
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
        onDiffResolved={onDiffResolved}
        onInvalidateDiffs={onInvalidateDiffs}
      />
    </div>
  );
}
