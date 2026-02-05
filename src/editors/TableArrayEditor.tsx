import { type ReactElement, useMemo, useState, useRef } from 'react';
import type React from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import type {
  RuntimeValue,
  Typ,
  ValueDef,
  PathSegment,
  Diff,
  TestIo,
} from '../generated/catala_types';
import ValueEditor from './ValueEditors';
import { confirm } from '../messaging/confirm';
import { ContextMenu } from '../ContextMenu';
import {
  computeActualOnlyIndices,
  indicesToRender as computeIndicesToRender,
  findChildIndexDiff,
  canAcceptAppend,
  canRemoveLast,
  isActualOnly,
  isExpectedOnly,
} from '../diff/arrayPresence';
import {
  getArrayItemLabel,
  isStructRow,
  getNestedValue,
  computeSubArrays,
  buildCellPath,
  tryCreateTableSchema,
  type SubArrayItem,
  type TableSchema,
} from './tableArrayUtils';
import { extractSimpleName, getTypeName } from './typeNameUtils';
import { useTableArrayHandlers } from './useTableArrayHandlers';

// Animation timing constants
const ANIMATION = {
  FLASH_DURATION_MS: 1000,
  SCROLL_SETTLE_MS: 500,
  RENDER_DELAY_MS: 50,
  NAV_DELAY_MS: 100,
} as const;

// Flash an element briefly to draw attention
function flashElement(element: Element | null, delayMs: number = 0): void {
  if (!element) return;
  setTimeout(() => {
    element.classList.add('flash-highlight');
    setTimeout(
      () => element.classList.remove('flash-highlight'),
      ANIMATION.FLASH_DURATION_MS
    );
  }, delayMs);
}

// Flash multiple elements
function flashElements(
  elements: NodeListOf<Element> | Element[],
  delayMs: number = 0
): void {
  if (!elements.length) return;
  setTimeout(() => {
    elements.forEach((el) => el.classList.add('flash-highlight'));
    setTimeout(
      () => elements.forEach((el) => el.classList.remove('flash-highlight')),
      ANIMATION.FLASH_DURATION_MS
    );
  }, delayMs);
}

// Metadata for each row in flattened sub-table mode
type RowMetadata = {
  parentRowIndex: number;
  parentLabel?: string; // Parent's ArrayItemLabel if defined
  itemIndexWithinParent: number;
  siblingCount: number; // Total items in this parent's sub-array
  isPhantom?: boolean; // True for actual-only diff items
  onNavigateToParent: () => void;
  onMoveItem: (fromIndex: number, toIndex: number) => void;
  onDeleteItem: (index: number) => Promise<void>;
  onDuplicateItem: (index: number, position: 'before' | 'after') => void;
  onAddItem: () => void; // Add new item to same parent's sub-array
  onLabelChange: (index: number, newLabel: string) => void; // Update item's label
};

type TableArrayEditorProps = {
  elementType: Typ;
  /** Pre-computed schema from tryCreateTableSchema. If not provided, computed internally. */
  schema?: TableSchema;
  valueDef?: ValueDef;
  onValueChange(newValue: RuntimeValue): void;
  editorHook?: (editor: ReactElement, path: PathSegment[]) => ReactElement;
  currentPath: PathSegment[];
  diffs: Diff[];
  onDiffResolved?: (path: PathSegment[]) => void;
  onInvalidateDiffs?: (pathPrefix: PathSegment[]) => void;
  editable?: boolean;
  // Sub-table mode props
  isSubTable?: boolean;
  rowMetadata?: RowMetadata[]; // For flattened sub-tables where each row has different parent
  // View toggle
  onSwitchToTreeView?: () => void; // Callback to switch to tree/card view
};

// Render a table cell using ValueEditor
function renderTableCell(
  fieldType: Typ,
  value: RuntimeValue | undefined,
  onChange: (newValue: RuntimeValue) => void,
  cellPath: PathSegment[],
  editorHook:
    | ((editor: ReactElement, path: PathSegment[]) => ReactElement)
    | undefined,
  diffs: Diff[],
  editable: boolean,
  onDiffResolved?: (path: PathSegment[]) => void,
  onInvalidateDiffs?: (pathPrefix: PathSegment[]) => void
): ReactElement {
  const testIO: TestIo = {
    typ: fieldType,
    value: { value: value ?? { value: { kind: 'Unset' }, attrs: [] } },
  };

  return (
    <ValueEditor
      testIO={testIO}
      onValueChange={(newTestIO) => {
        if (newTestIO.value) {
          onChange(newTestIO.value.value);
        }
      }}
      editorHook={editorHook}
      currentPath={cellPath}
      diffs={diffs}
      editable={editable}
      onDiffResolved={onDiffResolved}
      onInvalidateDiffs={onInvalidateDiffs}
    />
  );
}

export function TableArrayEditor(props: TableArrayEditorProps): ReactElement {
  const {
    elementType,
    valueDef,
    onValueChange,
    currentPath,
    editable = true,
    isSubTable = false,
    rowMetadata,
  } = props;
  const intl = useIntl();

  // Derive structType from elementType - no duplication
  const structType =
    elementType.kind === 'TStruct' ? elementType.value : undefined;

  const runtimeValue = valueDef?.value;
  const currentArray =
    runtimeValue?.value.kind === 'Array' ? runtimeValue.value.value : [];

  // Get all data mutation handlers from the hook
  const handlers = useTableArrayHandlers({
    currentArray,
    runtimeValue,
    elementType,
    onValueChange,
    onInvalidateDiffs: props.onInvalidateDiffs,
    currentPath,
  });

  // Store refs to row elements for sub-table navigation
  // Each TableArrayEditor maintains refs to its own rows, so nested sub-tables
  // can correctly navigate to their immediate parent rows (not the top-level).
  const rowRefs = useRef<Map<number, HTMLTableRowElement>>(new Map());

  // Helper: Navigate to sub-table and flash child rows
  const navigateAndFlashSubTable = (
    subTableId: string,
    parentRowIndex: number
  ): void => {
    const element = document.getElementById(subTableId);
    element?.scrollIntoView({ behavior: 'smooth', block: 'start' });

    // Query all rows with matching parent index, then filter to only include
    // direct children (not grandchildren in nested sub-tables).
    // Nested sub-tables also have rows with data-parent-row="0" etc., but those
    // refer to their own parent context, not ours.
    const allMatchingRows =
      element?.querySelectorAll(`[data-parent-row="${parentRowIndex}"]`) ?? [];
    const directChildRows = Array.from(allMatchingRows).filter(
      (row) => row.closest('.sub-table-section') === element
    );
    flashElements(directChildRows, ANIMATION.NAV_DELAY_MS);
  };

  // Use provided schema or compute it (for sub-tables).
  // Sub-tables always succeed because parent types were already validated.
  const schema: TableSchema = useMemo(() => {
    if (props.schema) return props.schema;
    const result = tryCreateTableSchema(elementType);
    if (!result.ok) {
      // This should never happen for sub-tables - parent validation ensures it
      throw new Error(
        `TableArrayEditor: unexpected unsupported type. Reasons: ${result.reasons.map((r) => r.reason).join(', ')}`
      );
    }
    return result.schema;
  }, [props.schema, elementType]);

  const subArrays = useMemo(
    () =>
      computeSubArrays(
        currentArray,
        schema.subArrays,
        props.diffs,
        currentPath
      ),
    [currentArray, schema.subArrays, props.diffs, currentPath]
  );

  // Compute phantom row indices for actual-only diffs (only for main table, not sub-tables)
  const phantomRowIndices = useMemo(
    () =>
      !isSubTable ? computeActualOnlyIndices(props.diffs, currentPath) : [],
    [isSubTable, props.diffs, currentPath]
  );

  const rowIndicesToRender = useMemo(
    () =>
      !isSubTable
        ? computeIndicesToRender(currentArray.length, phantomRowIndices)
        : Array.from({ length: currentArray.length }, (_, i) => i),
    [isSubTable, currentArray.length, phantomRowIndices]
  );

  const [draggedIndex, setDraggedIndex] = useState<number | null>(null);
  const [addDropdownRowIndex, setAddDropdownRowIndex] = useState<number | null>(
    null
  );
  const [dropdownAnchor, setDropdownAnchor] = useState<HTMLElement | null>(
    null
  );
  const [rowContextMenu, setRowContextMenu] = useState<{
    rowIndex: number;
    position: { x: number; y: number };
  } | null>(null);

  // Add an item to a sub-array and navigate to it with flash
  const handleAddSubArrayItem = (
    parentRowIndex: number,
    arrayFieldPath: string[],
    arrayLabel: string,
    subElementType: Typ
  ): void => {
    handlers.handleAddSubArrayItem(
      parentRowIndex,
      arrayFieldPath,
      subElementType
    );

    // Close dropdown
    setAddDropdownRowIndex(null);
    setDropdownAnchor(null);

    // Navigate to sub-table and flash the new item
    const subTableId = `sub-table-${arrayLabel}`;
    navigateAndFlashSubTable(subTableId, parentRowIndex);
  };

  // Wrapper for handleDuplicate that adds flash effect
  const handleDuplicateWithFlash = (
    index: number,
    position: 'before' | 'after'
  ): void => {
    if (isSubTable && rowMetadata) {
      const metadata = rowMetadata[index];
      if (metadata) {
        metadata.onDuplicateItem(metadata.itemIndexWithinParent, position);
      }
    } else {
      const insertAt = handlers.handleDuplicate(index, position);
      if (insertAt !== null) {
        setTimeout(() => {
          flashElement(
            document.querySelector(`[data-row-index="${insertAt}"]`)
          );
        }, ANIMATION.RENDER_DELAY_MS);
      }
    }
  };

  // Only show empty state if there are no rows AND no phantom rows
  if (currentArray.length === 0 && phantomRowIndices.length === 0) {
    return (
      <div className="table-array-editor">
        <div className="empty-table-message">
          <FormattedMessage id="tableView.noData" defaultMessage="No data" />
        </div>
        {editable && (
          <button
            className="button-action-dvp body-b3"
            onClick={handlers.handleAdd}
          >
            <span className="codicon codicon-add"></span>
            <FormattedMessage
              id="arrayEditor.addElement"
              defaultMessage="Add {elementType}"
              values={{
                elementType: structType
                  ? extractSimpleName(structType.struct_name)
                  : getTypeName(elementType),
              }}
            />
          </button>
        )}
      </div>
    );
  }

  return (
    <div className="table-array-editor">
      {/* View toggle button - only for main table, not sub-tables */}
      {!isSubTable && props.onSwitchToTreeView && (
        <div className="table-view-toggle">
          <button
            className="table-control-btn"
            onClick={props.onSwitchToTreeView}
            title={intl.formatMessage({
              id: 'tableView.switchToTreeView',
              defaultMessage: 'Switch to card view',
            })}
          >
            <span className="codicon codicon-list-tree" />
          </button>
        </div>
      )}
      {/* Main Table */}
      <div className="table-wrapper">
        <table className="table-view">
          <thead>
            <tr>
              <th className="table-header-controls-combined">#</th>
              {schema.columns.map((col, colIdx) => (
                <th
                  key={col.label || colIdx}
                  className="table-header"
                  title={col.label || undefined}
                >
                  {col.label ? (
                    col.label.split('.').pop()
                  ) : (
                    <FormattedMessage
                      id="tableView.value"
                      defaultMessage="Value"
                    />
                  )}
                </th>
              ))}
              {schema.subArrays.map((arr) => (
                <th
                  key={arr.label}
                  className="table-header-sub-array"
                  title={arr.label}
                >
                  {arr.label.split('.').pop()}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {rowIndicesToRender.map((rowIndex, idx) => {
              const row = currentArray[rowIndex];
              const isPhantom = row === undefined;

              // Get metadata for this row if in flattened sub-table mode
              const metadata = rowMetadata?.[rowIndex];

              // For phantom rows in sub-tables, use metadata.isPhantom
              const isPhantomRow = isSubTable ? metadata?.isPhantom : isPhantom;

              // Find the diff for this row (phantom or expected-only)
              const rowDiff = findChildIndexDiff(
                props.diffs,
                currentPath,
                rowIndex
              );

              // Skip phantom rows that aren't actual-only
              if (isPhantomRow && !(rowDiff && isActualOnly(rowDiff))) {
                return null;
              }

              // Check if this is an expected-only diff (present in expected, empty in actual)
              const isExpectedOnlyRow =
                !isPhantomRow && rowDiff && isExpectedOnly(rowDiff);

              // For phantom rows, use the actual value from the diff
              const displayRow = isPhantomRow
                ? (rowDiff!.actual as RuntimeValue)
                : row;

              const uid = displayRow.attrs?.find((attr) => attr.kind === 'Uid');
              const key = isPhantomRow
                ? `phantom-${rowIndex}`
                : uid?.kind === 'Uid'
                  ? uid.value
                  : rowIndex;

              // Detect if this row starts a new parent group (for sub-tables)
              const prevRowIndex = idx > 0 ? rowIndicesToRender[idx - 1] : null;
              const prevMetadata =
                prevRowIndex !== null ? rowMetadata?.[prevRowIndex] : null;
              const isNewParentGroup =
                metadata &&
                (!prevMetadata ||
                  prevMetadata.parentRowIndex !== metadata.parentRowIndex);

              // Check if this is a successfully computed struct
              const isStruct = isStructRow(displayRow);
              const structData = isStruct
                ? displayRow.value.value[1]
                : undefined;

              // Build row className
              const rowClassName = [
                isSubTable
                  ? `sub-table-row ${metadata!.parentRowIndex % 2 === 0 ? 'parent-shade-even' : 'parent-shade-odd'}`
                  : 'table-row',
                isPhantomRow && 'phantom-row',
                isExpectedOnlyRow && 'expected-only-row',
                draggedIndex === rowIndex && 'dragging',
                isNewParentGroup && 'parent-group-start',
              ]
                .filter(Boolean)
                .join(' ');

              return (
                <tr
                  key={key}
                  ref={(el) => {
                    if (el) {
                      rowRefs.current.set(rowIndex, el);
                    }
                  }}
                  className={rowClassName}
                  id={isSubTable ? undefined : `parent-row-${rowIndex}`}
                  data-row-index={rowIndex}
                  data-parent-row={metadata?.parentRowIndex}
                  draggable={editable && !isSubTable && !isPhantomRow}
                  onDragStart={() =>
                    !isSubTable && !isPhantomRow && setDraggedIndex(rowIndex)
                  }
                  onDragEnd={() => !isSubTable && setDraggedIndex(null)}
                  onDragOver={(e) => !isSubTable && e.preventDefault()}
                  onDrop={() => {
                    if (!isSubTable && draggedIndex !== null && !isPhantomRow) {
                      handlers.handleMove(draggedIndex, rowIndex);
                    }
                  }}
                >
                  {/* Row controls (always present, includes row number) */}
                  <td
                    className="table-cell-controls"
                    onContextMenu={(e) => {
                      // Show context menu for non-phantom, non-expected-only rows
                      if (editable && !isPhantomRow && !isExpectedOnlyRow) {
                        e.preventDefault();
                        setRowContextMenu({
                          rowIndex,
                          position: { x: e.clientX, y: e.clientY },
                        });
                      }
                    }}
                  >
                    {isPhantomRow && (
                      <div className="phantom-row-indicator">
                        <div className="empty-value-indicator expected">
                          <FormattedMessage
                            id="diff.emptyExpected"
                            defaultMessage="Empty"
                          />
                        </div>
                        <div className="phantom-actual-label">
                          <FormattedMessage
                            id="diff.actualValue"
                            defaultMessage="Actual value"
                          />
                        </div>
                      </div>
                    )}
                    {isExpectedOnlyRow && (
                      <div className="expected-only-row-indicator">
                        <div className="empty-value-indicator actual">
                          <FormattedMessage
                            id="diff.emptyActual"
                            defaultMessage="Empty"
                          />
                        </div>
                      </div>
                    )}
                    <div className="table-row-controls">
                      {/* Phantom/expected-only rows: show action button instead of label */}
                      {editable && isPhantomRow && (
                        <>
                          {canAcceptAppend(currentArray.length, rowIndex) && (
                            <button
                              className="phantom-action-btn phantom-accept"
                              onClick={() => {
                                const elementToInsert = rowDiff!
                                  .actual as RuntimeValue;
                                const newArray = [
                                  ...currentArray,
                                  elementToInsert,
                                ];
                                handlers.updateParent(newArray);
                                props.onDiffResolved?.([
                                  ...currentPath,
                                  { kind: 'ListIndex', value: rowIndex },
                                ]);
                              }}
                            >
                              <span className="codicon codicon-check"></span>
                              <FormattedMessage
                                id="diff.addToExpected"
                                defaultMessage="Add to expected"
                              />
                            </button>
                          )}
                        </>
                      )}
                      {editable && isExpectedOnlyRow && (
                        <>
                          {canRemoveLast(currentArray.length, rowIndex) && (
                            <button
                              className="phantom-action-btn phantom-remove"
                              onClick={async () => {
                                if (!(await confirm('DeleteArrayElement')))
                                  return;
                                const newArray = currentArray.filter(
                                  (_, i) => i !== rowIndex
                                );
                                handlers.updateParent(newArray);
                                props.onDiffResolved?.([
                                  ...currentPath,
                                  { kind: 'ListIndex', value: rowIndex },
                                ]);
                              }}
                            >
                              <span className="codicon codicon-trash"></span>
                              <FormattedMessage
                                id="diff.removeFromExpected"
                                defaultMessage="Remove from expected"
                              />
                            </button>
                          )}
                        </>
                      )}
                      {/* Back arrow for sub-table rows with parent label */}
                      {!isPhantomRow && !isExpectedOnlyRow && metadata && (
                        <span
                          className="parent-nav-link"
                          onClick={metadata.onNavigateToParent}
                          title={intl.formatMessage(
                            {
                              id: 'tableEditor.goToParent',
                              defaultMessage: 'Go to parent: {parentLabel}',
                            },
                            { parentLabel: metadata.parentLabel ?? '' }
                          )}
                        >
                          <span className="codicon codicon-reply" />
                          {metadata.parentLabel && (
                            <span className="parent-nav-label">
                              {metadata.parentLabel}
                            </span>
                          )}
                        </span>
                      )}
                      {/* Editable label for all rows (top-level and sub-table) */}
                      {!isPhantomRow && !isExpectedOnlyRow && (
                        <input
                          type="text"
                          className="row-label-input"
                          value={getArrayItemLabel(displayRow.attrs) ?? ''}
                          onChange={(e) =>
                            metadata
                              ? metadata.onLabelChange(
                                  metadata.itemIndexWithinParent,
                                  e.target.value
                                )
                              : handlers.handleLabelChange(
                                  rowIndex,
                                  e.target.value
                                )
                          }
                          onKeyDown={(e) => {
                            if (e.key === 'Enter' || e.key === 'Escape') {
                              (e.target as HTMLInputElement).blur();
                            }
                          }}
                          placeholder={intl.formatMessage({
                            id: 'tableEditor.labelPlaceholder',
                            defaultMessage: 'Name...',
                          })}
                          disabled={!editable}
                        />
                      )}
                      {editable && !isPhantomRow && !isExpectedOnlyRow && (
                        <>
                          <button
                            className="table-control-btn"
                            onClick={() =>
                              metadata
                                ? metadata.onMoveItem(
                                    metadata.itemIndexWithinParent,
                                    metadata.itemIndexWithinParent - 1
                                  )
                                : handlers.handleMove(rowIndex, rowIndex - 1)
                            }
                            disabled={
                              metadata
                                ? metadata.itemIndexWithinParent === 0
                                : rowIndex === 0
                            }
                            title={intl.formatMessage({
                              id: 'arrayEditor.movePrevious',
                            })}
                          >
                            <span className="codicon codicon-arrow-up"></span>
                          </button>
                          <button
                            className="table-control-btn"
                            onClick={() =>
                              metadata
                                ? metadata.onMoveItem(
                                    metadata.itemIndexWithinParent,
                                    metadata.itemIndexWithinParent + 1
                                  )
                                : handlers.handleMove(rowIndex, rowIndex + 1)
                            }
                            disabled={
                              metadata
                                ? metadata.itemIndexWithinParent ===
                                  metadata.siblingCount - 1
                                : rowIndex === currentArray.length - 1
                            }
                            title={intl.formatMessage({
                              id: 'arrayEditor.moveNext',
                            })}
                          >
                            <span className="codicon codicon-arrow-down"></span>
                          </button>
                          <button
                            className="table-control-btn table-control-delete"
                            onClick={() =>
                              metadata
                                ? metadata.onDeleteItem(
                                    metadata.itemIndexWithinParent
                                  )
                                : handlers.handleDelete(rowIndex)
                            }
                            title={intl.formatMessage({
                              id: 'arrayEditor.deleteElement',
                            })}
                          >
                            <span className="codicon codicon-trash"></span>
                          </button>
                        </>
                      )}
                      {editable &&
                        !isPhantomRow &&
                        !isExpectedOnlyRow &&
                        schema.subArrays.length > 0 && (
                          <>
                            <button
                              className="add-subarray-pill"
                              onClick={(e) => {
                                if (addDropdownRowIndex === rowIndex) {
                                  setAddDropdownRowIndex(null);
                                  setDropdownAnchor(null);
                                } else {
                                  setAddDropdownRowIndex(rowIndex);
                                  setDropdownAnchor(e.currentTarget);
                                }
                              }}
                              title={intl.formatMessage({
                                id: 'tableView.addSubArrayItem',
                              })}
                            >
                              +
                            </button>
                            <ContextMenu
                              isOpen={addDropdownRowIndex === rowIndex}
                              onClose={() => {
                                setAddDropdownRowIndex(null);
                                setDropdownAnchor(null);
                              }}
                              anchorElement={dropdownAnchor}
                            >
                              {schema.subArrays.map((arr) => {
                                const arrElemType =
                                  arr.arrayType.kind === 'TArray'
                                    ? arr.arrayType.value
                                    : arr.arrayType;
                                const typeName = getTypeName(arrElemType);
                                const fieldName = arr.fieldPath.at(-1);
                                return (
                                  <div
                                    key={arr.label}
                                    className="context-menu-item"
                                    onClick={() => {
                                      handleAddSubArrayItem(
                                        rowIndex,
                                        arr.fieldPath,
                                        arr.label,
                                        arrElemType
                                      );
                                    }}
                                  >
                                    <FormattedMessage
                                      id="tableView.addNewItemIn"
                                      defaultMessage="Add new {typeName} in {fieldName}"
                                      values={{ typeName, fieldName }}
                                    />
                                  </div>
                                );
                              })}
                            </ContextMenu>
                          </>
                        )}
                    </div>
                  </td>

                  {/* Atomic columns */}
                  {schema.columns.map((col, colIdx) => {
                    const cellPath = buildCellPath(
                      currentPath,
                      rowIndex,
                      col.fieldPath
                    );

                    // Use schema to get cell value
                    // For Unset/Invalid/etc.: show parent state for all cells
                    const value =
                      isStruct || col.fieldPath.length === 0
                        ? schema.getCellValue(displayRow, col)
                        : {
                            value: displayRow.value,
                            attrs: displayRow.attrs || [],
                          };

                    const onChange = (newValue: RuntimeValue): void =>
                      handlers.handleCellUpdate(
                        rowIndex,
                        col.fieldPath,
                        newValue
                      );

                    return (
                      <td
                        key={col.label || colIdx}
                        className="table-cell"
                        // Disable default VS Code context menu on data cells - it's non-functional
                        // and confusing. Context menu is only available on row headers.
                        onContextMenu={(e) => e.preventDefault()}
                      >
                        {renderTableCell(
                          col.fieldType,
                          value,
                          onChange,
                          cellPath,
                          props.editorHook,
                          isPhantomRow ? [] : props.diffs, // Stop diff propagation for phantom rows
                          editable && !isPhantomRow && !isExpectedOnlyRow, // Phantom and expected-only cells are read-only
                          props.onDiffResolved,
                          props.onInvalidateDiffs
                        )}
                      </td>
                    );
                  })}

                  {/* Sub-array count badges and add buttons */}
                  {schema.subArrays.map((arr) => {
                    const arrayValue = structData
                      ? getNestedValue(structData, arr.fieldPath)
                      : undefined;
                    const count =
                      arrayValue?.value.kind === 'Array'
                        ? arrayValue.value.value.length
                        : 0;

                    const subTableId = `sub-table-${arr.label}`;
                    const arrElemType =
                      arr.arrayType.kind === 'TArray'
                        ? arr.arrayType.value
                        : arr.arrayType;

                    return (
                      <td
                        key={arr.label}
                        className="table-cell-sub-array-count"
                        // Disable default VS Code context menu - it shows non-functional
                        // cut/copy/paste. Context menu is only available on row headers.
                        onContextMenu={(e) => e.preventDefault()}
                      >
                        <div className="sub-array-cell-content">
                          {count > 0 ? (
                            <button
                              className="count-badge count-badge-clickable"
                              onClick={() => {
                                navigateAndFlashSubTable(subTableId, rowIndex);
                              }}
                              title={`Go to ${arr.label} for row #${rowIndex + 1}`}
                            >
                              {count}
                            </button>
                          ) : (
                            <span className="count-badge count-badge-zero">
                              0
                            </span>
                          )}
                          {editable && !isPhantomRow && !isExpectedOnlyRow && (
                            <button
                              className="add-subarray-pill"
                              onClick={() => {
                                handleAddSubArrayItem(
                                  rowIndex,
                                  arr.fieldPath,
                                  arr.label,
                                  arrElemType
                                );
                              }}
                              title={intl.formatMessage({
                                id: 'tableView.addSubArrayItem',
                              })}
                            >
                              +
                            </button>
                          )}
                        </div>
                      </td>
                    );
                  })}
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>

      {/* Row context menu (right-click) */}
      <ContextMenu
        isOpen={rowContextMenu !== null}
        onClose={() => setRowContextMenu(null)}
        position={rowContextMenu?.position}
      >
        <div
          className="context-menu-item"
          onClick={() => {
            if (rowContextMenu) {
              handleDuplicateWithFlash(rowContextMenu.rowIndex, 'before');
              setRowContextMenu(null);
            }
          }}
        >
          <span className="codicon codicon-copy"></span>
          <FormattedMessage
            id="tableView.duplicateBefore"
            defaultMessage="Duplicate above"
          />
        </div>
        <div
          className="context-menu-item"
          onClick={() => {
            if (rowContextMenu) {
              handleDuplicateWithFlash(rowContextMenu.rowIndex, 'after');
              setRowContextMenu(null);
            }
          }}
        >
          <span className="codicon codicon-copy"></span>
          <FormattedMessage
            id="tableView.duplicateAfter"
            defaultMessage="Duplicate below"
          />
        </div>
        <div
          className="context-menu-item"
          onClick={() => {
            if (rowContextMenu) {
              if (isSubTable && rowMetadata) {
                rowMetadata[rowContextMenu.rowIndex]?.onAddItem();
              } else {
                handlers.handleAdd();
              }
              setRowContextMenu(null);
            }
          }}
        >
          <span className="codicon codicon-add"></span>
          <FormattedMessage
            id="tableView.addNewItem"
            defaultMessage="Add new item"
          />
        </div>
      </ContextMenu>

      {/* Add button - only show for top-level arrays, not sub-tables */}
      {editable && !isSubTable && (
        <button
          className="button-action-dvp body-b3"
          onClick={handlers.handleAdd}
        >
          <span className="codicon codicon-add"></span>
          <FormattedMessage
            id="arrayEditor.addElement"
            defaultMessage="Add {elementType}"
            values={{
              elementType: structType
                ? extractSimpleName(structType.struct_name)
                : getTypeName(elementType),
            }}
          />
        </button>
      )}

      {/* Sub-array tables - grouped by field, all parent rows combined */}
      {subArrays.map((subArray, idx) => {
        if (subArray.arrayType.kind !== 'TArray') return null;
        const subElementType = subArray.arrayType.value;

        // Check if array elements are structs
        if (subElementType.kind === 'TStruct') {
          const subTableId = `sub-table-${subArray.label}`;
          const subStructDecl = subElementType.value;

          // Group items by parent row for add buttons
          const itemsByParent = new Map<number, SubArrayItem[]>();
          subArray.items.forEach((item) => {
            const existing = itemsByParent.get(item.parentRowIndex) ?? [];
            existing.push(item);
            itemsByParent.set(item.parentRowIndex, existing);
          });

          return (
            <div key={idx} id={subTableId} className="sub-table-section">
              <div className="sub-table-header">
                {subArray.label}
                <span className="count-badge">{subArray.items.length}</span>
              </div>
              <div className="sub-table-content">
                <TableArrayEditor
                  elementType={subElementType}
                  valueDef={{
                    value: {
                      value: {
                        kind: 'Array',
                        value: subArray.items.map((item) => item.value),
                      },
                      attrs: [],
                    },
                  }}
                  onValueChange={(newValue) =>
                    handlers.handleSubTableChange(
                      subArray,
                      subStructDecl,
                      newValue
                    )
                  }
                  currentPath={[
                    ...currentPath,
                    ...subArray.fieldPath.map((f) => ({
                      kind: 'StructField' as const,
                      value: f,
                    })),
                  ]}
                  diffs={props.diffs}
                  onDiffResolved={props.onDiffResolved}
                  onInvalidateDiffs={props.onInvalidateDiffs}
                  editable={editable}
                  isSubTable={true}
                  rowMetadata={((): RowMetadata[] => {
                    // Compute sibling counts per parent
                    const siblingCounts = new Map<number, number>();
                    for (const item of subArray.items) {
                      siblingCounts.set(
                        item.parentRowIndex,
                        (siblingCounts.get(item.parentRowIndex) ?? 0) + 1
                      );
                    }
                    return subArray.items.map((item) => ({
                      parentRowIndex: item.parentRowIndex,
                      parentLabel: getArrayItemLabel(
                        currentArray[item.parentRowIndex]?.attrs
                      ),
                      itemIndexWithinParent: item.itemIndex,
                      siblingCount: siblingCounts.get(item.parentRowIndex) ?? 0,
                      isPhantom: item.isPhantom,
                      onNavigateToParent: (): void => {
                        // Use the ref to the parent row element directly.
                        // This ensures we navigate to the correct parent at each
                        // nesting level, not the top-level main table row.
                        const parentRow = rowRefs.current.get(
                          item.parentRowIndex
                        );
                        if (parentRow) {
                          parentRow.scrollIntoView({
                            behavior: 'smooth',
                            block: 'center',
                          });
                          flashElement(parentRow, ANIMATION.SCROLL_SETTLE_MS);
                        }
                      },
                      onMoveItem: (
                        fromIndex: number,
                        toIndex: number
                      ): void => {
                        handlers.handleSubArrayItemMove(
                          item.parentRowIndex,
                          subArray.fieldPath,
                          fromIndex,
                          toIndex
                        );
                      },
                      onDeleteItem: async (index: number): Promise<void> => {
                        // For phantom items, resolve the diff instead of deleting
                        if (item.isPhantom) {
                          const subArrayPath = [
                            ...currentPath,
                            {
                              kind: 'ListIndex' as const,
                              value: item.parentRowIndex,
                            },
                            ...subArray.fieldPath.map(
                              (f): PathSegment => ({
                                kind: 'StructField',
                                value: f,
                              })
                            ),
                            { kind: 'ListIndex' as const, value: index },
                          ];
                          props.onDiffResolved?.(subArrayPath);
                        } else {
                          await handlers.handleSubArrayItemDelete(
                            item.parentRowIndex,
                            subArray.fieldPath,
                            index
                          );
                        }
                      },
                      onDuplicateItem: (
                        index: number,
                        position: 'before' | 'after'
                      ): void => {
                        handlers.handleSubArrayItemDuplicate(
                          item.parentRowIndex,
                          subArray.fieldPath,
                          index,
                          position
                        );
                      },
                      onAddItem: (): void => {
                        handlers.handleAddSubArrayItem(
                          item.parentRowIndex,
                          subArray.fieldPath,
                          subElementType
                        );
                      },
                      onLabelChange: (
                        index: number,
                        newLabel: string
                      ): void => {
                        handlers.handleSubArrayItemLabelChange(
                          item.parentRowIndex,
                          subArray.fieldPath,
                          index,
                          newLabel
                        );
                      },
                    }));
                  })()}
                  editorHook={props.editorHook}
                />
              </div>
            </div>
          );
        } else {
          // Non-struct sub-arrays (primitives, enums) - use TableArrayEditor in simple mode
          const subTableId = `sub-table-${subArray.label}`;

          // Compute sibling counts per parent
          const siblingCounts = new Map<number, number>();
          for (const item of subArray.items) {
            siblingCounts.set(
              item.parentRowIndex,
              (siblingCounts.get(item.parentRowIndex) ?? 0) + 1
            );
          }

          return (
            <div key={idx} id={subTableId} className="sub-table-section">
              <div className="sub-table-header">
                {subArray.label}
                <span className="count-badge">{subArray.items.length}</span>
              </div>
              <div className="sub-table-content">
                <TableArrayEditor
                  elementType={subElementType}
                  // No structType - simple single-column mode
                  valueDef={{
                    value: {
                      value: {
                        kind: 'Array',
                        value: subArray.items.map((item) => item.value),
                      },
                      attrs: [],
                    },
                  }}
                  onValueChange={(newValue) => {
                    // Distribute updated values back to parent rows
                    if (newValue.value.kind !== 'Array') return;
                    const newValues = newValue.value.value;
                    // Group updates by parent row
                    const updatesByParent = new Map<number, RuntimeValue[]>();
                    subArray.items.forEach((item, idx) => {
                      const parentUpdates =
                        updatesByParent.get(item.parentRowIndex) ?? [];
                      parentUpdates[item.itemIndex] = newValues[idx];
                      updatesByParent.set(item.parentRowIndex, parentUpdates);
                    });
                    // Apply updates to each parent
                    updatesByParent.forEach((updates, parentRowIndex) => {
                      handlers.handleParentSubArrayUpdate(
                        parentRowIndex,
                        subArray.fieldPath,
                        updates
                      );
                    });
                  }}
                  currentPath={[
                    ...currentPath,
                    ...subArray.fieldPath.map((f) => ({
                      kind: 'StructField' as const,
                      value: f,
                    })),
                  ]}
                  diffs={props.diffs}
                  onDiffResolved={props.onDiffResolved}
                  onInvalidateDiffs={props.onInvalidateDiffs}
                  editable={editable}
                  isSubTable={true}
                  rowMetadata={subArray.items.map((item) => ({
                    parentRowIndex: item.parentRowIndex,
                    parentLabel: getArrayItemLabel(
                      currentArray[item.parentRowIndex]?.attrs
                    ),
                    itemIndexWithinParent: item.itemIndex,
                    siblingCount: siblingCounts.get(item.parentRowIndex) ?? 0,
                    isPhantom: item.isPhantom,
                    onNavigateToParent: (): void => {
                      const parentRow = rowRefs.current.get(
                        item.parentRowIndex
                      );
                      if (parentRow) {
                        parentRow.scrollIntoView({
                          behavior: 'smooth',
                          block: 'center',
                        });
                        flashElement(parentRow, ANIMATION.SCROLL_SETTLE_MS);
                      }
                    },
                    onMoveItem: (fromIndex: number, toIndex: number): void => {
                      handlers.handleSubArrayItemMove(
                        item.parentRowIndex,
                        subArray.fieldPath,
                        fromIndex,
                        toIndex
                      );
                    },
                    onDeleteItem: async (index: number): Promise<void> => {
                      if (item.isPhantom) {
                        const subArrayPath = [
                          ...currentPath,
                          {
                            kind: 'ListIndex' as const,
                            value: item.parentRowIndex,
                          },
                          ...subArray.fieldPath.map(
                            (f): PathSegment => ({
                              kind: 'StructField',
                              value: f,
                            })
                          ),
                          { kind: 'ListIndex' as const, value: index },
                        ];
                        props.onDiffResolved?.(subArrayPath);
                      } else {
                        await handlers.handleSubArrayItemDelete(
                          item.parentRowIndex,
                          subArray.fieldPath,
                          index
                        );
                      }
                    },
                    onDuplicateItem: (
                      index: number,
                      position: 'before' | 'after'
                    ): void => {
                      handlers.handleSubArrayItemDuplicate(
                        item.parentRowIndex,
                        subArray.fieldPath,
                        index,
                        position
                      );
                    },
                    onAddItem: (): void => {
                      handlers.handleAddSubArrayItem(
                        item.parentRowIndex,
                        subArray.fieldPath,
                        subElementType
                      );
                    },
                    onLabelChange: (index: number, newLabel: string): void => {
                      handlers.handleSubArrayItemLabelChange(
                        item.parentRowIndex,
                        subArray.fieldPath,
                        index,
                        newLabel
                      );
                    },
                  }))}
                  editorHook={props.editorHook}
                />
              </div>
            </div>
          );
        }
      })}
    </div>
  );
}
