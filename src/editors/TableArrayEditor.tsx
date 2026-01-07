import { type ReactElement, useMemo, useState } from 'react';
import type React from 'react';
import { FormattedMessage, useIntl } from 'react-intl';
import type {
  RuntimeValue,
  RuntimeValueRaw,
  Typ,
  ValueDef,
  PathSegment,
  Diff,
  StructDeclaration,
  TestIo,
} from '../generated/catala_types';
import ValueEditor, {
  createRuntimeValue,
  getDefaultValue,
} from './ValueEditors';
import { confirm } from '../messaging/confirm';
import { ContextMenu } from '../ContextMenu';

// Color cycle for parent row indication (matches VS Code chart colors)
const PARENT_ROW_COLORS = [
  'var(--vscode-charts-blue)',
  'var(--vscode-charts-orange)',
  'var(--vscode-charts-green)',
  'var(--vscode-charts-purple)',
  'var(--vscode-charts-red)',
  'var(--vscode-charts-yellow)',
] as const;

// Metadata for each row in flattened sub-table mode
type RowMetadata = {
  parentRowIndex: number;
  parentColor: string;
  itemIndexWithinParent: number;
  onNavigateToParent: () => void;
  onMoveItem: (fromIndex: number, toIndex: number) => void;
  onDeleteItem: (index: number) => Promise<void>;
};

type TableArrayEditorProps = {
  elementType: Typ;
  structType: StructDeclaration;
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
};

// Flattened column descriptor with full path
interface IFlatColumn {
  label: string; // Display label (e.g., "personne.date_naissance")
  fieldPath: string[]; // Path through nested structs (e.g., ["personne", "date_naissance"])
  fieldType: Typ; // The atomic type
}

// Sub-array item with parent row tracking
interface ISubArrayItem {
  parentRowIndex: number;
  itemIndex: number; // Index within the parent's array
  value: RuntimeValue;
}

// Sub-array descriptor with items from all parent rows
interface ISubArrayDescriptor {
  label: string; // Display label (e.g., "personne.rôles")
  fieldPath: string[]; // Path to array field
  arrayType: Typ; // TArray type
  items: ISubArrayItem[]; // All items from all parent rows
}

// Get display name for a type
function getTypeName(typ: Typ): string {
  switch (typ.kind) {
    case 'TStruct':
      return typ.value.struct_name.split('.').pop() ?? typ.value.struct_name;
    case 'TEnum':
      return typ.value.enum_name.split('.').pop() ?? typ.value.enum_name;
    case 'TArray':
      return `${getTypeName(typ.value)}[]`;
    case 'TInt':
      return 'Integer';
    case 'TBool':
      return 'Boolean';
    case 'TRat':
      return 'Decimal';
    case 'TMoney':
      return 'Money';
    case 'TDate':
      return 'Date';
    case 'TDuration':
      return 'Duration';
    case 'TUnit':
      return 'Unit';
    case 'TTuple':
      return 'Tuple';
    case 'TOption':
      return `${getTypeName(typ.value)}?`;
    case 'TArrow':
      return 'Function';
    default: {
      // Exhaustiveness check - ensures all Typ variants are handled
      const _exhaustive: never = typ;
      void _exhaustive;
      return 'Unknown';
    }
  }
}

// Deep flatten struct into atomic columns and array fields
function flattenStruct(
  structDecl: StructDeclaration,
  pathPrefix: string[] = []
): {
  atomicColumns: IFlatColumn[];
  arrayFields: { label: string; fieldPath: string[]; arrayType: Typ }[];
} {
  const atomicColumns: IFlatColumn[] = [];
  const arrayFields: { label: string; fieldPath: string[]; arrayType: Typ }[] =
    [];

  for (const [fieldName, fieldType] of structDecl.fields.entries()) {
    const fieldPath = [...pathPrefix, fieldName];
    const label = fieldPath.join('.');

    if (fieldType.kind === 'TArray') {
      // Extract array for sub-table
      arrayFields.push({ label, fieldPath, arrayType: fieldType });
    } else if (fieldType.kind === 'TStruct') {
      // Recurse into nested struct
      const nested = flattenStruct(fieldType.value, fieldPath);
      atomicColumns.push(...nested.atomicColumns);
      arrayFields.push(...nested.arrayFields);
    } else if (fieldType.kind === 'TEnum') {
      // Enums are atomic for now (show in main table)
      atomicColumns.push({ label, fieldPath, fieldType });
    } else {
      // Atomic type (Int, Bool, Date, Money, etc.)
      atomicColumns.push({ label, fieldPath, fieldType });
    }
  }

  return { atomicColumns, arrayFields };
}

// Get nested field value from struct by path
function getNestedValue(
  structData: Map<string, RuntimeValue>,
  fieldPath: string[]
): RuntimeValue | undefined {
  if (fieldPath.length === 0) return undefined;

  const [first, ...rest] = fieldPath;
  const value = structData.get(first);

  if (!value || rest.length === 0) {
    return value;
  }

  // Navigate deeper
  if (value.value.kind === 'Struct') {
    return getNestedValue(value.value.value[1], rest);
  }

  return undefined;
}

// Set nested field value in struct by path
function setNestedValue(
  structData: Map<string, RuntimeValue>,
  fieldPath: string[],
  newValue: RuntimeValue,
  structType: StructDeclaration
): Map<string, RuntimeValue> {
  if (fieldPath.length === 0) return structData;

  const [first, ...rest] = fieldPath;
  const newMap = new Map(structData);

  if (rest.length === 0) {
    // Leaf: set the value directly
    newMap.set(first, newValue);
  } else {
    // Intermediate: navigate deeper
    const currentValue = structData.get(first);
    const fieldType = structType.fields.get(first);

    if (fieldType?.kind === 'TStruct') {
      const nestedStructDecl = fieldType.value;
      const currentNestedData =
        currentValue?.value.kind === 'Struct'
          ? currentValue.value.value[1]
          : new Map<string, RuntimeValue>();

      const updatedNestedData = setNestedValue(
        currentNestedData,
        rest,
        newValue,
        nestedStructDecl
      );

      newMap.set(first, {
        value: {
          kind: 'Struct',
          value: [nestedStructDecl, updatedNestedData],
        },
        attrs: currentValue?.attrs ?? [],
      });
    }
  }

  return newMap;
}

// Compute sub-arrays from all rows (grouped by field)
function computeSubArrays(
  rows: RuntimeValue[],
  arrayFields: { label: string; fieldPath: string[]; arrayType: Typ }[]
): ISubArrayDescriptor[] {
  // Group by field name
  const groupedByField = new Map<string, ISubArrayDescriptor>();

  arrayFields.forEach(({ label, fieldPath, arrayType }) => {
    const items: ISubArrayItem[] = [];

    // Collect items from all parent rows for this field
    rows.forEach((row, parentRowIndex) => {
      if (row.value.kind !== 'Struct') return;
      const structData = row.value.value[1];
      const arrayValue = getNestedValue(structData, fieldPath);

      if (arrayValue?.value.kind === 'Array') {
        // Add all items from this parent row's array
        arrayValue.value.value.forEach((value, itemIndex) => {
          items.push({ parentRowIndex, itemIndex, value });
        });
      }
    });

    // Only create descriptor if there are items
    if (items.length > 0) {
      groupedByField.set(label, {
        label,
        fieldPath,
        arrayType,
        items,
      });
    }
  });

  return Array.from(groupedByField.values());
}

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
    structType,
    valueDef,
    onValueChange,
    currentPath,
    editable = true,
    isSubTable = false,
    rowMetadata,
  } = props;
  const intl = useIntl();

  const runtimeValue = valueDef?.value;
  const currentArray =
    runtimeValue?.value.kind === 'Array' ? runtimeValue.value.value : [];

  const updateParent = (newArray: RuntimeValue[]): void => {
    const newValueRaw: RuntimeValueRaw = { kind: 'Array', value: newArray };
    onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
  };

  const invalidateArrayDiffs = (): void => {
    props.onInvalidateDiffs?.(currentPath);
  };

  const handleAdd = (): void => {
    const newElementValue = getDefaultValue(props.elementType);
    const newElement: RuntimeValue = {
      ...newElementValue,
      attrs: [
        ...(newElementValue.attrs ?? []),
        { kind: 'Uid', value: String(crypto.randomUUID()) },
      ],
    };
    updateParent([...currentArray, newElement]);
    invalidateArrayDiffs();
  };

  const handleDelete = async (index: number): Promise<void> => {
    if (!(await confirm('DeleteArrayElement'))) return;
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

  const handleCellUpdate = (
    rowIndex: number,
    fieldPath: string[],
    newValue: RuntimeValue
  ): void => {
    const row = currentArray[rowIndex];
    if (row?.value.kind !== 'Struct') return;

    const [structDecl, structData] = row.value.value;
    const newMap = setNestedValue(structData, fieldPath, newValue, structDecl);

    const newRow: RuntimeValue = {
      ...row,
      value: { kind: 'Struct', value: [structDecl, newMap] },
    };

    const newArray = [...currentArray];
    newArray[rowIndex] = newRow;
    updateParent(newArray);
  };

  // Update a specific parent row's sub-array
  const handleParentSubArrayUpdate = (
    parentRowIndex: number,
    fieldPath: string[],
    newArrayValue: RuntimeValue[]
  ): void => {
    const row = currentArray[parentRowIndex];
    if (row?.value.kind !== 'Struct') return;

    const [structDecl, structData] = row.value.value;
    const newArrayRuntimeValue: RuntimeValue = {
      value: { kind: 'Array', value: newArrayValue },
      attrs: [],
    };
    const newMap = setNestedValue(
      structData,
      fieldPath,
      newArrayRuntimeValue,
      structDecl
    );

    const newRow: RuntimeValue = {
      ...row,
      value: { kind: 'Struct', value: [structDecl, newMap] },
    };

    const newArray = [...currentArray];
    newArray[parentRowIndex] = newRow;
    updateParent(newArray);
    invalidateArrayDiffs();
  };

  // Update a cell in a sub-array item
  const handleSubArrayItemCellUpdate = (
    parentRowIndex: number,
    arrayFieldPath: string[],
    itemIndex: number,
    itemFieldPath: string[],
    newValue: RuntimeValue,
    itemStructDecl: StructDeclaration
  ): void => {
    const row = currentArray[parentRowIndex];
    if (row?.value.kind !== 'Struct') return;

    const [, structData] = row.value.value;
    const arrayValue = getNestedValue(structData, arrayFieldPath);
    if (arrayValue?.value.kind !== 'Array') return;

    const currentSubArray = arrayValue.value.value;
    const item = currentSubArray[itemIndex];
    if (item?.value.kind !== 'Struct') return;

    const [, itemStructData] = item.value.value;
    const newItemMap = setNestedValue(
      itemStructData,
      itemFieldPath,
      newValue,
      itemStructDecl
    );

    const newItem: RuntimeValue = {
      ...item,
      value: { kind: 'Struct', value: [itemStructDecl, newItemMap] },
    };

    const newSubArray = [...currentSubArray];
    newSubArray[itemIndex] = newItem;
    handleParentSubArrayUpdate(parentRowIndex, arrayFieldPath, newSubArray);
  };

  // Delete an item from a sub-array
  const handleSubArrayItemDelete = async (
    parentRowIndex: number,
    arrayFieldPath: string[],
    itemIndex: number
  ): Promise<void> => {
    if (!(await confirm('DeleteArrayElement'))) return;

    const row = currentArray[parentRowIndex];
    if (row?.value.kind !== 'Struct') return;

    const [, structData] = row.value.value;
    const arrayValue = getNestedValue(structData, arrayFieldPath);
    if (arrayValue?.value.kind !== 'Array') return;

    const currentSubArray = arrayValue.value.value;
    const newSubArray = currentSubArray.filter((_, i) => i !== itemIndex);
    handleParentSubArrayUpdate(parentRowIndex, arrayFieldPath, newSubArray);
  };

  // Move an item within a sub-array
  const handleSubArrayItemMove = (
    parentRowIndex: number,
    arrayFieldPath: string[],
    fromIndex: number,
    toIndex: number
  ): void => {
    const row = currentArray[parentRowIndex];
    if (row?.value.kind !== 'Struct') return;

    const [, structData] = row.value.value;
    const arrayValue = getNestedValue(structData, arrayFieldPath);
    if (arrayValue?.value.kind !== 'Array') return;

    const currentSubArray = arrayValue.value.value;
    if (toIndex < 0 || toIndex >= currentSubArray.length) return;

    const newSubArray = [...currentSubArray];
    const [movedItem] = newSubArray.splice(fromIndex, 1);
    newSubArray.splice(toIndex, 0, movedItem);
    handleParentSubArrayUpdate(parentRowIndex, arrayFieldPath, newSubArray);
  };

  const { atomicColumns, arrayFields } = useMemo(
    () => flattenStruct(structType),
    [structType]
  );

  const subArrays = useMemo(
    () => computeSubArrays(currentArray, arrayFields),
    [currentArray, arrayFields]
  );

  const [draggedIndex, setDraggedIndex] = useState<number | null>(null);
  const [addDropdownRowIndex, setAddDropdownRowIndex] = useState<number | null>(
    null
  );
  const [dropdownAnchor, setDropdownAnchor] = useState<HTMLElement | null>(
    null
  );

  // Add an item to a sub-array and navigate to it with flash
  const handleAddSubArrayItem = (
    parentRowIndex: number,
    arrayFieldPath: string[],
    arrayLabel: string,
    elementType: Typ
  ): void => {
    const row = currentArray[parentRowIndex];
    if (row?.value.kind !== 'Struct') return;

    const [, structData] = row.value.value;
    const arrayValue = getNestedValue(structData, arrayFieldPath);

    const currentSubArray =
      arrayValue?.value.kind === 'Array' ? arrayValue.value.value : [];

    const newElementValue = getDefaultValue(elementType);
    const newElement: RuntimeValue = {
      ...newElementValue,
      attrs: [
        ...(newElementValue.attrs ?? []),
        { kind: 'Uid', value: String(crypto.randomUUID()) },
      ],
    };

    const newSubArray = [...currentSubArray, newElement];
    handleParentSubArrayUpdate(parentRowIndex, arrayFieldPath, newSubArray);

    // Close dropdown
    setAddDropdownRowIndex(null);
    setDropdownAnchor(null);

    // Navigate to sub-table and flash the new item
    setTimeout(() => {
      const subTableId = `sub-table-${arrayLabel}`;
      const element = document.getElementById(subTableId);
      element?.scrollIntoView({ behavior: 'smooth', block: 'start' });

      // Flash the newly added row (last item from this parent)
      setTimeout(() => {
        const newItemIndex = currentSubArray.length;
        const childRows = document.querySelectorAll(
          `[data-parent-row="${parentRowIndex}"]`
        );
        // Find the row for the new item (it's the last one from this parent)
        const targetRow = Array.from(childRows).find((row) => {
          const rowElement = row as HTMLElement;
          // Check if this is the new item by matching the item index in the row number
          return rowElement.textContent?.includes(`#${newItemIndex + 1}`);
        });

        if (targetRow) {
          targetRow.classList.add('flash-highlight');
          setTimeout(() => {
            targetRow.classList.remove('flash-highlight');
          }, 1000);
        }
      }, 500);
    }, 100);
  };

  if (currentArray.length === 0) {
    return (
      <div className="table-array-editor">
        <div className="empty-table-message">
          <FormattedMessage id="tableView.noData" defaultMessage="No data" />
        </div>
        {editable && (
          <button className="button-action-dvp body-b3" onClick={handleAdd}>
            <span className="codicon codicon-add"></span>
            <FormattedMessage
              id="arrayEditor.addElement"
              defaultMessage="Add {elementType}"
              values={{ elementType: structType.struct_name.split('.').pop() }}
            />
          </button>
        )}
      </div>
    );
  }

  return (
    <div className="table-array-editor">
      {/* Main Table */}
      <div className="table-wrapper">
        <table className="table-view">
          <thead>
            <tr>
              {editable && (
                <th className="table-header-controls-combined">#</th>
              )}
              {!editable && <th className="table-header-number">#</th>}
              {atomicColumns.map((col) => (
                <th key={col.label} className="table-header" title={col.label}>
                  {col.label}
                </th>
              ))}
              {arrayFields.map((arr) => (
                <th
                  key={arr.label}
                  className="table-header-sub-array"
                  title={arr.label}
                >
                  {arr.label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {currentArray.map((row, rowIndex) => {
              const uid = row.attrs?.find((attr) => attr.kind === 'Uid');
              const key = uid?.kind === 'Uid' ? uid.value : rowIndex;

              // Get metadata for this row if in flattened sub-table mode
              const metadata = rowMetadata?.[rowIndex];

              // Use metadata color if available, otherwise cycle colors for main table
              const borderColor = metadata
                ? metadata.parentColor
                : PARENT_ROW_COLORS[rowIndex % PARENT_ROW_COLORS.length];

              // Check if this is a successfully computed struct
              const isStruct = row.value.kind === 'Struct';
              const structData =
                isStruct && row.value.kind === 'Struct'
                  ? row.value.value[1]
                  : undefined;

              return (
                <tr
                  key={key}
                  className={`${isSubTable ? 'sub-table-row' : 'table-row'} ${draggedIndex === rowIndex ? 'dragging' : ''}`}
                  id={isSubTable ? undefined : `parent-row-${rowIndex}`}
                  data-row-index={rowIndex}
                  data-parent-row={metadata?.parentRowIndex}
                  style={{ borderLeft: `4px solid ${borderColor}` }}
                  draggable={editable && !isSubTable}
                  onDragStart={() => !isSubTable && setDraggedIndex(rowIndex)}
                  onDragEnd={() => !isSubTable && setDraggedIndex(null)}
                  onDragOver={(e) => !isSubTable && e.preventDefault()}
                  onDrop={() => {
                    if (!isSubTable && draggedIndex !== null) {
                      handleMove(draggedIndex, rowIndex);
                    }
                  }}
                >
                  {/* Row controls (includes row number when editable) */}
                  {editable && (
                    <td className="table-cell-controls">
                      <div className="table-row-controls">
                        <span
                          className={`table-row-number-inline ${metadata?.onNavigateToParent ? 'clickable-row-number' : ''}`}
                          onClick={metadata?.onNavigateToParent}
                          title={
                            metadata
                              ? `From row #${metadata.parentRowIndex + 1} - Click to navigate`
                              : undefined
                          }
                        >
                          #
                          {metadata?.itemIndexWithinParent !== undefined
                            ? metadata.itemIndexWithinParent + 1
                            : rowIndex + 1}
                        </span>
                        <button
                          className="table-control-btn"
                          onClick={() =>
                            metadata
                              ? metadata.onMoveItem(
                                  metadata.itemIndexWithinParent,
                                  metadata.itemIndexWithinParent - 1
                                )
                              : handleMove(rowIndex, rowIndex - 1)
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
                              : handleMove(rowIndex, rowIndex + 1)
                          }
                          disabled={rowIndex === currentArray.length - 1}
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
                              : handleDelete(rowIndex)
                          }
                          title={intl.formatMessage({
                            id: 'arrayEditor.deleteElement',
                          })}
                        >
                          <span className="codicon codicon-trash"></span>
                        </button>
                        {arrayFields.length > 0 && (
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
                              title="Add item to sub-array"
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
                              {arrayFields.map((arr) => {
                                const elementType =
                                  arr.arrayType.kind === 'TArray'
                                    ? arr.arrayType.value
                                    : arr.arrayType;
                                const typeName = getTypeName(elementType);
                                const fieldName = arr.label.split('.').pop();
                                return (
                                  <div
                                    key={arr.label}
                                    className="context-menu-item"
                                    onClick={() => {
                                      handleAddSubArrayItem(
                                        rowIndex,
                                        arr.fieldPath,
                                        arr.label,
                                        elementType
                                      );
                                    }}
                                  >
                                    Add new {typeName} in '{fieldName}'
                                  </div>
                                );
                              })}
                            </ContextMenu>
                          </>
                        )}
                      </div>
                    </td>
                  )}

                  {/* Row number when not editable */}
                  {!editable && (
                    <td className="table-row-number">#{rowIndex + 1}</td>
                  )}

                  {/* Atomic columns */}
                  {isStruct && structData
                    ? // Render struct fields in separate columns
                      atomicColumns.map((col) => {
                        const value = getNestedValue(structData, col.fieldPath);
                        const cellPath = [
                          ...currentPath,
                          { kind: 'ListIndex' as const, value: rowIndex },
                          ...col.fieldPath.map((field) => ({
                            kind: 'StructField' as const,
                            value: field,
                          })),
                        ];
                        return (
                          <td key={col.label} className="table-cell">
                            {renderTableCell(
                              col.fieldType,
                              value,
                              (newValue) => {
                                handleCellUpdate(
                                  rowIndex,
                                  col.fieldPath,
                                  newValue
                                );
                              },
                              cellPath,
                              props.editorHook,
                              props.diffs,
                              editable,
                              props.onDiffResolved,
                              props.onInvalidateDiffs
                            )}
                          </td>
                        );
                      })
                    : // For Unset/Invalid/Impossible/etc., render individual cells for each field
                      // Each cell shows the same state (Unset/Invalid/Impossible/etc.) as the parent item
                      atomicColumns.map((col) => {
                        const cellPath = [
                          ...currentPath,
                          { kind: 'ListIndex' as const, value: rowIndex },
                          ...col.fieldPath.map((field) => ({
                            kind: 'StructField' as const,
                            value: field,
                          })),
                        ];

                        // Create a RuntimeValue with the same state as the parent item
                        // (Unset/Invalid/Impossible/Conflict/etc.) for this specific field
                        const cellValue: RuntimeValue = {
                          value: row.value, // Preserve the state (Unset/Invalid/Impossible/etc.)
                          attrs: row.attrs || [],
                        };

                        return (
                          <td key={col.label} className="table-cell">
                            {renderTableCell(
                              col.fieldType,
                              cellValue,
                              (newValue) => {
                                // When user provides a value for an Unset/Invalid/Impossible item,
                                // construct a proper struct with all fields initialized
                                const newStructData = new Map<
                                  string,
                                  RuntimeValue
                                >();
                                for (const [
                                  fieldName,
                                  fieldType,
                                ] of structType.fields.entries()) {
                                  newStructData.set(
                                    fieldName,
                                    getDefaultValue(fieldType)
                                  );
                                }

                                // Now set the edited field (potentially nested)
                                const finalStructData = setNestedValue(
                                  newStructData,
                                  col.fieldPath,
                                  newValue,
                                  structType
                                );

                                const newRow: RuntimeValue = {
                                  value: {
                                    kind: 'Struct',
                                    value: [structType, finalStructData],
                                  },
                                  attrs: row.attrs || [],
                                };

                                const newArray = [...currentArray];
                                newArray[rowIndex] = newRow;
                                updateParent(newArray);
                              },
                              cellPath,
                              props.editorHook,
                              props.diffs,
                              editable,
                              props.onDiffResolved,
                              props.onInvalidateDiffs
                            )}
                          </td>
                        );
                      })}

                  {/* Sub-array count badges */}
                  {arrayFields.map((arr) => {
                    const arrayValue = structData
                      ? getNestedValue(structData, arr.fieldPath)
                      : undefined;
                    const count =
                      arrayValue?.value.kind === 'Array'
                        ? arrayValue.value.value.length
                        : 0;

                    const subTableId = `sub-table-${arr.label}`;

                    return (
                      <td
                        key={arr.label}
                        className="table-cell-sub-array-count"
                      >
                        {count > 0 && (
                          <button
                            className="count-badge count-badge-clickable"
                            onClick={() => {
                              const element =
                                document.getElementById(subTableId);
                              element?.scrollIntoView({
                                behavior: 'smooth',
                                block: 'start',
                              });
                              // Flash child rows belonging to this parent AFTER scroll
                              setTimeout(() => {
                                const childRows = document.querySelectorAll(
                                  `[data-parent-row="${rowIndex}"]`
                                );
                                childRows.forEach((row) => {
                                  row.classList.add('flash-highlight');
                                });
                                setTimeout(() => {
                                  childRows.forEach((row) => {
                                    row.classList.remove('flash-highlight');
                                  });
                                }, 1000);
                              }, 500); // Wait for scroll animation
                            }}
                            title={`Go to ${arr.label} for row #${rowIndex + 1}`}
                          >
                            {count}
                          </button>
                        )}
                      </td>
                    );
                  })}
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>

      {/* Add button - only show for top-level arrays, not sub-tables */}
      {editable && !isSubTable && (
        <button className="button-action-dvp body-b3" onClick={handleAdd}>
          <span className="codicon codicon-add"></span>
          <FormattedMessage
            id="arrayEditor.addElement"
            defaultMessage="Add {elementType}"
            values={{ elementType: structType.struct_name.split('.').pop() }}
          />
        </button>
      )}

      {/* Sub-array tables - grouped by field, all parent rows combined */}
      {subArrays.map((subArray, idx) => {
        if (subArray.arrayType.kind !== 'TArray') return null;
        const elementType = subArray.arrayType.value;

        // Check if array elements are structs
        if (elementType.kind === 'TStruct') {
          const subTableId = `sub-table-${subArray.label}`;
          const itemStructDecl = elementType.value;

          // Group items by parent row for add buttons
          const itemsByParent = new Map<number, ISubArrayItem[]>();
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
                  elementType={elementType}
                  structType={itemStructDecl}
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
                    // Handle updates to the flattened array
                    if (newValue.value.kind !== 'Array') return;
                    const newItems = newValue.value.value;

                    // Distribute items back to their parent rows
                    subArray.items.forEach((item, flatIndex) => {
                      if (flatIndex < newItems.length) {
                        handleSubArrayItemCellUpdate(
                          item.parentRowIndex,
                          subArray.fieldPath,
                          item.itemIndex,
                          [],
                          newItems[flatIndex],
                          itemStructDecl
                        );
                      }
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
                  rowMetadata={subArray.items.map((item) => {
                    return {
                      parentRowIndex: item.parentRowIndex,
                      parentColor:
                        PARENT_ROW_COLORS[
                          item.parentRowIndex % PARENT_ROW_COLORS.length
                        ],
                      itemIndexWithinParent: item.itemIndex,
                      onNavigateToParent: (): void => {
                        const mainTableRow = document.getElementById(
                          `parent-row-${item.parentRowIndex}`
                        );
                        if (mainTableRow) {
                          mainTableRow.scrollIntoView({
                            behavior: 'smooth',
                            block: 'center',
                          });
                          setTimeout(() => {
                            mainTableRow.classList.add('flash-highlight');
                            setTimeout(
                              () =>
                                mainTableRow.classList.remove(
                                  'flash-highlight'
                                ),
                              1000
                            );
                          }, 500);
                        }
                      },
                      onMoveItem: (
                        fromIndex: number,
                        toIndex: number
                      ): void => {
                        handleSubArrayItemMove(
                          item.parentRowIndex,
                          subArray.fieldPath,
                          fromIndex,
                          toIndex
                        );
                      },
                      onDeleteItem: async (index: number): Promise<void> => {
                        await handleSubArrayItemDelete(
                          item.parentRowIndex,
                          subArray.fieldPath,
                          index
                        );
                      },
                    };
                  })}
                  editorHook={props.editorHook}
                />
              </div>
            </div>
          );
        } else {
          // For non-struct arrays (primitives), keep manual rendering for now
          return null; // TODO: implement primitive array rendering
        }
      })}
    </div>
  );
}
