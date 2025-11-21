import type { ReactElement } from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import { useState } from 'react';
import { getDefaultValue } from '../defaults';
import type {
  RuntimeValue,
  RuntimeValueRaw,
  Typ,
  ValueDef,
  PathSegment,
  Diff,
} from '../generated/test_case';
import ValueEditor, { createRuntimeValue } from './ValueEditors';
import { assertUnreachable } from '../util';
import { isPathPrefix } from '../diff/highlight';
import { confirm } from '../messaging/confirm';

/**
 * Diffs are consumed only by ArrayEditor to compute "phantom" indices
 * (expected Empty vs actual Something). We do NOT propagate diffs into
 * phantom children (we pass [] there) so recursion stops at the first
 * Empty-vs-Actual boundary.
 */
type ArrayEditorProps = {
  elementType: Typ;
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
  editorHook?: (editor: ReactElement, path: PathSegment[]) => ReactElement;
  currentPath: PathSegment[];
  diffs: Diff[];
  onDiffResolved?: (path: PathSegment[]) => void;
  onInvalidateDiffs?: (pathPrefix: PathSegment[]) => void;
  editable?: boolean;
};

// We introspect the array type to understand whether
// there are any nested arrays down the line.
//
// If no nesting happens, then we display the array elements
// as 'cards' within a 2D layout to maximize space use --
// otherwise, we arrange elements vertically to keep horizontal
// space for further nesting.
//
// `hasNestedArrays` is exported as this introspection capability
// is also needed by CompositeEditor
export function hasNestedArrays(typ: Typ): boolean {
  if (typ.kind === 'TArray') {
    return true;
  } else if (
    typ.kind !== 'TEnum' &&
    typ.kind !== 'TOption' &&
    typ.kind !== 'TStruct' &&
    typ.kind !== 'TTuple'
  ) {
    return false;
  } else if (typ.kind === 'TEnum') {
    return Array.from(typ.value.constructors.values())
      .filter((val) => val !== null)
      .map((val) => val?.value)
      .some(hasNestedArrays);
  } else if (typ.kind === 'TOption') {
    return hasNestedArrays(typ.value);
  } else if (typ.kind === 'TStruct') {
    return Array.from(typ.value.fields.values()).some(hasNestedArrays);
  } else if (typ.kind === 'TTuple') {
    return typ.value.some(hasNestedArrays);
  }
  assertUnreachable(typ);
}

/**
 * Checks if a runtime value represents an empty value
 * (aligned with AssertionValueEditor logic).
 */
function isEmptyValue(value: RuntimeValue): boolean {
  return value.value.kind === 'Empty';
}

// Get a display name for a type that can be used in UI messages
export function getTypeDisplayName(
  typ: Typ,
  intl: ReturnType<typeof useIntl>
): string {
  switch (typ.kind) {
    case 'TBool':
      return intl.formatMessage({ id: 'type.boolean' });
    case 'TInt':
      return intl.formatMessage({ id: 'type.integer' });
    case 'TRat':
      return intl.formatMessage({ id: 'type.decimal' });
    case 'TMoney':
      return intl.formatMessage({ id: 'type.money' });
    case 'TDate':
      return intl.formatMessage({ id: 'type.date' });
    case 'TDuration':
      return intl.formatMessage({ id: 'type.duration' });
    case 'TArray': {
      const baseType = getTypeDisplayName(typ.value, intl);
      return intl.formatMessage({ id: 'type.array' }, { baseType });
    }
    case 'TOption': {
      const baseType = getTypeDisplayName(typ.value, intl);
      return intl.formatMessage({ id: 'type.optional' }, { baseType });
    }
    case 'TTuple':
      return intl.formatMessage({ id: 'type.tuple' });
    case 'TStruct': {
      // For structs, extract just the name part without the package
      const fullName = typ.value.struct_name;
      const nameParts = fullName.split('.');
      return nameParts[nameParts.length - 1];
    }
    case 'TEnum': {
      // For enums, extract just the name part without the package
      const fullName = typ.value.enum_name;
      const nameParts = fullName.split('.');
      return nameParts[nameParts.length - 1];
    }
    default:
      assertUnreachable(typ);
  }
}

export function ArrayEditor(props: ArrayEditorProps): ReactElement {
  const {
    elementType,
    valueDef,
    onValueChange,
    editorHook,
    currentPath,
    editable = true,
  } = props;
  const runtimeValue = valueDef?.value;
  const currentArray =
    runtimeValue?.value.kind === 'Array' ? runtimeValue.value.value : [];

  const updateParent = (newArray: RuntimeValue[]): void => {
    const newValueRaw: RuntimeValueRaw = { kind: 'Array', value: newArray };
    onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
  };

  // Invalidate diffs under this array after any structural edit.
  // Low-complexity strategy: wipe diffs (for this array subtree) and require manual re-run.
  const invalidateArrayDiffs = (): void => {
    props.onInvalidateDiffs?.(currentPath);
  };

  const handleAdd = (): void => {
    const newElementValue = getDefaultValue(elementType);
    // Add a unique ID attribute when creating a new element
    // (this is required by React)
    // https://react.dev/learn/rendering-lists#keeping-list-items-in-order-with-key
    const newElement: RuntimeValue = {
      ...newElementValue,
      attrs: [
        ...(newElementValue.attrs ?? []), // Preserve existing attrs if any
        {
          kind: 'Uid',
          value: String(self.crypto.randomUUID()),
        },
      ],
    };
    updateParent([...currentArray, newElement]);
    invalidateArrayDiffs();
  };

  const handleUpdate = (index: number, updatedElement: RuntimeValue): void => {
    const newArray = [...currentArray];
    newArray[index] = updatedElement;
    updateParent(newArray);
  };

  const handleDelete = (index: number): void => {
    const newArray = currentArray.filter((_, i) => i !== index);
    updateParent(newArray);
    invalidateArrayDiffs();
  };

  const handleMove = (fromIndex: number, toIndex: number): void => {
    if (
      toIndex < 0 ||
      toIndex >= currentArray.length ||
      fromIndex === toIndex
    ) {
      return;
    }
    const newArray = [...currentArray];
    const [movedItem] = newArray.splice(fromIndex, 1);
    newArray.splice(toIndex, 0, movedItem);
    updateParent(newArray);
    invalidateArrayDiffs();
  };

  const intl = useIntl();
  const elementTypeName = getTypeDisplayName(elementType, intl);
  // if the array has nested subarrays, we lay it out vertically,
  // otherwise we use a flowing 'card' layout.
  const isVertical = hasNestedArrays(elementType);

  // State for drag and drop
  const [draggedIndex, setDraggedIndex] = useState<number | null>(null);
  const [dragOverIndex, setDragOverIndex] = useState<number | null>(null);

  // Handle drag start
  const handleDragStart = (e: React.DragEvent, index: number): void => {
    // Check if the drag started from the controls area but not from a button
    const target = e.target as HTMLElement;
    if (target.tagName === 'BUTTON' || target.closest('button')) {
      e.preventDefault();
      return;
    }
    setDraggedIndex(index);
    // Set a ghost drag image
    if (e.dataTransfer) {
      const dragElement = document.createElement('div');
      dragElement.textContent = `Item ${index + 1}`;
      dragElement.className = 'drag-ghost';
      document.body.appendChild(dragElement);
      e.dataTransfer.setDragImage(dragElement, 0, 0);
      setTimeout(() => {
        document.body.removeChild(dragElement);
      }, 0);
    }
  };

  // Handle drag over
  const handleDragOver = (e: React.DragEvent, index: number): void => {
    e.preventDefault();
    if (draggedIndex !== null && draggedIndex !== index) {
      setDragOverIndex(index);
    }
  };

  // Handle drop
  const handleDrop = (e: React.DragEvent, index: number): void => {
    e.preventDefault();
    if (draggedIndex !== null && draggedIndex !== index) {
      handleMove(draggedIndex, index);
      setDraggedIndex(null);
      setDragOverIndex(null);
    }
  };

  // Handle drag end
  const handleDragEnd = (): void => {
    setDraggedIndex(null);
    setDragOverIndex(null);
  };

  return (
    <div className="array-editor">
      <div
        className={`array-items ${isVertical ? 'array-items-nested' : 'array-items-non-nested'}`}
      >
        {((): ReactElement[] => {
          const baseIndices = Array.from(
            { length: currentArray.length },
            (_, i) => i
          );
          const extraIndices = (props.diffs ?? [])
            .filter(
              (d) =>
                isPathPrefix(currentPath, d.path) &&
                d.path.length === currentPath.length + 1 &&
                d.path[currentPath.length].kind === 'ListIndex' &&
                // Only create phantom indices when expected is Empty and actual has a value
                isEmptyValue(d.expected) &&
                !isEmptyValue(d.actual)
            )
            .map(
              (d) =>
                (
                  d.path[currentPath.length] as Extract<
                    PathSegment,
                    { kind: 'ListIndex' }
                  >
                ).value
            );
          const indicesToRender = Array.from(
            new Set([...baseIndices, ...extraIndices])
          ).sort((a, b) => a - b);

          return indicesToRender.map((index) => {
            const item = currentArray[index];
            const isPhantom = item === undefined; // "phantom"

            // Determine key
            let itemKey: string | number;
            if (!isPhantom) {
              const uidAttr = item.attrs?.find((attr) => attr.kind === 'Uid');
              if (uidAttr?.kind === 'Uid') {
                itemKey = uidAttr.value;
              } else {
                console.warn(
                  `Array item at index ${index} is missing a UID attribute. Falling back to index key.`
                );
                itemKey = index;
              }
            } else {
              itemKey = `phantom-${index}`;
            }

            const childIndexSeg: PathSegment = {
              kind: 'ListIndex',
              value: index,
            };
            const childPath: PathSegment[] = [...currentPath, childIndexSeg];
            const indexDiff = (props.diffs ?? []).find(
              (d) =>
                isPathPrefix(currentPath, d.path) &&
                d.path.length === currentPath.length + 1 &&
                d.path[currentPath.length].kind === 'ListIndex' &&
                d.path[currentPath.length].value === index
            );

            return (
              <div
                key={itemKey}
                className={`array-item ${
                  dragOverIndex === index ? 'drag-over' : ''
                } ${draggedIndex === index ? 'dragging' : ''}`}
                onDragOver={(e) => handleDragOver(e, index)}
                onDrop={(e) => handleDrop(e, index)}
                onDragEnd={handleDragEnd}
              >
                <div className="array-item-content">
                  {isVertical && (
                    <h2 className="array-item-header heading-h2">
                      {getTypeDisplayName(elementType, intl)}
                    </h2>
                  )}
                  {isPhantom &&
                  indexDiff &&
                  isEmptyValue(indexDiff.expected) &&
                  !isEmptyValue(indexDiff.actual) ? (
                    <>
                      <div className="empty-value-indicator expected">
                        <FormattedMessage
                          id="diff.emptyExpected"
                          defaultMessage="Empty"
                        />
                      </div>
                      <div className="phantom-actual-value">
                        <div className="phantom-actual-label">
                          <FormattedMessage
                            id="diff.actualValue"
                            defaultMessage="Actual value"
                          />
                        </div>
                        <button
                          className="array-phantom-action array-phantom-add"
                          onClick={() => {
                            if (editable === false || !indexDiff) return;
                            const elementToInsert =
                              indexDiff.actual as RuntimeValue;
                            const newArray = [...currentArray];
                            newArray.splice(index, 0, elementToInsert);
                            updateParent(newArray);
                            invalidateArrayDiffs();
                          }}
                          disabled={editable === false}
                        >
                          <span className="codicon codicon-check"></span>
                          <FormattedMessage
                            id="diff.addToExpected"
                            defaultMessage="Add to expected"
                          />
                        </button>
                        <div className="phantom-actual-content">
                          <ValueEditor
                            testIO={{
                              typ: elementType,
                              value: { value: indexDiff.actual },
                            }}
                            onValueChange={() => {
                              // Non-editable
                            }}
                            editorHook={undefined}
                            currentPath={childPath}
                            diffs={[]}
                            editable={false}
                          />
                        </div>
                      </div>
                    </>
                  ) : (
                    <>
                      {indexDiff &&
                        isEmptyValue(indexDiff.actual) &&
                        !isEmptyValue(indexDiff.expected) && (
                          <>
                            <div className="empty-value-indicator actual">
                              <FormattedMessage
                                id="diff.emptyActual"
                                defaultMessage="Empty"
                              />
                            </div>
                            <button
                              className="array-phantom-action array-phantom-remove"
                              onClick={async () => {
                                if (editable === false) return;
                                const confirmed =
                                  await confirm('DeleteArrayElement');
                                if (!confirmed) return;
                                const newArray = [...currentArray];
                                newArray.splice(index, 1);
                                updateParent(newArray);
                                invalidateArrayDiffs();
                              }}
                              disabled={editable === false}
                            >
                              <span className="codicon codicon-trash"></span>
                              <FormattedMessage
                                id="diff.removeFromExpected"
                                defaultMessage="Remove from expected"
                              />
                            </button>
                          </>
                        )}
                      {!isPhantom && (
                        <ValueEditor
                          testIO={{
                            typ: elementType,
                            value: { value: item as RuntimeValue },
                          }}
                          onValueChange={(newItemTestIo) => {
                            if (newItemTestIo.value) {
                              handleUpdate(index, newItemTestIo.value.value);
                            }
                          }}
                          editorHook={editorHook}
                          currentPath={childPath}
                          diffs={props.diffs}
                          editable={editable}
                          onDiffResolved={props.onDiffResolved}
                        />
                      )}
                    </>
                  )}
                </div>
                <div
                  className="array-item-controls"
                  draggable={editable && !isPhantom}
                  onDragStart={(e) => {
                    if (!isPhantom) handleDragStart(e, index);
                  }}
                >
                  {editable && (
                    <>
                      <button
                        className="array-move-prev"
                        onClick={() => handleMove(index, index - 1)}
                        disabled={isPhantom || index === 0}
                        title={intl.formatMessage({
                          id: 'arrayEditor.movePrevious',
                        })}
                      >
                        <span
                          className={`codicon ${isVertical ? 'codicon-arrow-up' : 'codicon-arrow-left'}`}
                        ></span>
                      </button>
                      <button
                        className="array-move-next"
                        onClick={() => handleMove(index, index + 1)}
                        disabled={
                          isPhantom || index === currentArray.length - 1
                        }
                        title={intl.formatMessage({
                          id: 'arrayEditor.moveNext',
                        })}
                      >
                        <span
                          className={`codicon ${isVertical ? 'codicon-arrow-down' : 'codicon-arrow-right'}`}
                        ></span>
                      </button>
                    </>
                  )}
                  {editable && !isPhantom && (
                    <button
                      className="array-delete"
                      onClick={async () => {
                        if (!(await confirm('DeleteArrayElement'))) return;
                        handleDelete(index);
                      }}
                      title={intl.formatMessage({
                        id: 'arrayEditor.deleteElement',
                      })}
                    >
                      <span className="codicon codicon-trash"></span>
                    </button>
                  )}
                </div>
              </div>
            );
          });
        })()}
      </div>
      {editable && (
        <button className="button-action-dvp body-b3" onClick={handleAdd}>
          <span className="codicon codicon-add"></span>
          <FormattedMessage
            id="arrayEditor.addElement"
            values={{ elementType: elementTypeName }}
          />
        </button>
      )}
    </div>
  );
}
