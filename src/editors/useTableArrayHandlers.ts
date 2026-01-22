/**
 * Custom hook for TableArrayEditor data mutations.
 * Pure data logic - no DOM manipulation or UI effects.
 */

import type {
  RuntimeValue,
  RuntimeValueRaw,
  Typ,
  StructDeclaration,
  PathSegment,
} from '../generated/catala_types';
import { createRuntimeValue, getDefaultValue } from './ValueEditors';
import { confirm } from '../messaging/confirm';
import {
  cloneWithNewUid,
  setArrayItemLabel,
  getNestedValue,
  setNestedValue,
  updateOrConstructStruct,
  type SubArrayDescriptor,
} from './tableArrayUtils';

type RowMetadata = {
  itemIndexWithinParent: number;
  onDuplicateItem: (index: number, position: 'before' | 'after') => void;
};

type TableArrayHandlers = {
  updateParent: (newArray: RuntimeValue[]) => void;
  handleAdd: () => void;
  handleDelete: (index: number) => Promise<void>;
  handleMove: (fromIndex: number, toIndex: number) => void;
  handleDuplicate: (
    index: number,
    position: 'before' | 'after'
  ) => number | null;
  handleLabelChange: (rowIndex: number, newLabel: string) => void;
  handleCellUpdate: (
    rowIndex: number,
    fieldPath: string[],
    newValue: RuntimeValue
  ) => void;
  handleParentSubArrayUpdate: (
    parentRowIndex: number,
    fieldPath: string[],
    newArrayValue: RuntimeValue[]
  ) => void;
  handleSubArrayItemDelete: (
    parentRowIndex: number,
    arrayFieldPath: string[],
    itemIndex: number
  ) => Promise<void>;
  handleSubArrayItemMove: (
    parentRowIndex: number,
    arrayFieldPath: string[],
    fromIndex: number,
    toIndex: number
  ) => void;
  handleSubArrayItemDuplicate: (
    parentRowIndex: number,
    arrayFieldPath: string[],
    itemIndex: number,
    position: 'before' | 'after'
  ) => void;
  handleSubArrayItemLabelChange: (
    parentRowIndex: number,
    arrayFieldPath: string[],
    itemIndex: number,
    newLabel: string
  ) => void;
  handleSubTableChange: (
    subArray: SubArrayDescriptor,
    itemStructDecl: StructDeclaration,
    newValue: RuntimeValue
  ) => void;
  handleAddSubArrayItem: (
    parentRowIndex: number,
    arrayFieldPath: string[],
    subElementType: Typ
  ) => void;
};

type UseTableArrayHandlersProps = {
  currentArray: RuntimeValue[];
  runtimeValue: RuntimeValue | undefined;
  elementType: Typ;
  structType: StructDeclaration;
  onValueChange: (newValue: RuntimeValue) => void;
  onInvalidateDiffs?: (pathPrefix: PathSegment[]) => void;
  currentPath: PathSegment[];
  isSubTable: boolean;
  rowMetadata?: RowMetadata[];
};

export function useTableArrayHandlers(
  props: UseTableArrayHandlersProps
): TableArrayHandlers {
  const {
    currentArray,
    runtimeValue,
    elementType,
    structType,
    onValueChange,
    onInvalidateDiffs,
    currentPath,
    isSubTable,
    rowMetadata,
  } = props;

  const updateParent = (newArray: RuntimeValue[]): void => {
    const newValueRaw: RuntimeValueRaw = { kind: 'Array', value: newArray };
    onValueChange(createRuntimeValue(newValueRaw, runtimeValue));
  };

  const invalidateArrayDiffs = (): void => {
    onInvalidateDiffs?.(currentPath);
  };

  const handleAdd = (): void => {
    const newElement = cloneWithNewUid(getDefaultValue(elementType));
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

  // Returns insertAt index for caller to handle UI effects (flash)
  const handleDuplicate = (
    index: number,
    position: 'before' | 'after'
  ): number | null => {
    // For sub-tables, delegate to the parent via metadata callback
    if (isSubTable && rowMetadata) {
      const metadata = rowMetadata[index];
      if (metadata) {
        metadata.onDuplicateItem(metadata.itemIndexWithinParent, position);
        return null;
      }
    }

    const original = currentArray[index];
    if (!original) {
      throw new Error(`handleDuplicate: invalid index ${index}`);
    }

    const cloned = cloneWithNewUid(original);
    const insertAt = position === 'before' ? index : index + 1;
    const newArray = [
      ...currentArray.slice(0, insertAt),
      cloned,
      ...currentArray.slice(insertAt),
    ];
    updateParent(newArray);
    invalidateArrayDiffs();
    return insertAt;
  };

  const handleLabelChange = (rowIndex: number, newLabel: string): void => {
    const row = currentArray[rowIndex];
    if (!row) {
      throw new Error(`handleLabelChange: invalid rowIndex ${rowIndex}`);
    }

    const newRow: RuntimeValue = {
      ...row,
      attrs: setArrayItemLabel(row.attrs, newLabel),
    };

    const newArray = [...currentArray];
    newArray[rowIndex] = newRow;
    updateParent(newArray);
  };

  // Handles both existing struct rows and unset/invalid rows (constructs struct if needed)
  const handleCellUpdate = (
    rowIndex: number,
    fieldPath: string[],
    newValue: RuntimeValue
  ): void => {
    const row = currentArray[rowIndex];
    const newRow = updateOrConstructStruct(
      row,
      structType,
      fieldPath,
      newValue
    );
    const newArray = [...currentArray];
    newArray[rowIndex] = newRow;
    updateParent(newArray);
  };

  const handleParentSubArrayUpdate = (
    parentRowIndex: number,
    fieldPath: string[],
    newArrayValue: RuntimeValue[]
  ): void => {
    const row = currentArray[parentRowIndex];
    const newArrayRuntimeValue: RuntimeValue = {
      value: { kind: 'Array', value: newArrayValue },
      attrs: [],
    };

    let newRow: RuntimeValue;
    if (row.value.kind === 'Struct') {
      // Existing struct - update the sub-array field
      const [structDecl, structData] = row.value.value;
      const newMap = setNestedValue(
        structData,
        fieldPath,
        newArrayRuntimeValue,
        structDecl
      );
      newRow = {
        ...row,
        value: { kind: 'Struct', value: [structDecl, newMap] },
      };
    } else if (row.value.kind === 'Unset') {
      // Unset row - construct a full struct with defaults, then set the sub-array
      const newItemMap = new Map<string, RuntimeValue>();
      for (const [fieldName, fieldType] of structType.fields.entries()) {
        newItemMap.set(fieldName, getDefaultValue(fieldType));
      }
      const newMap = setNestedValue(
        newItemMap,
        fieldPath,
        newArrayRuntimeValue,
        structType
      );
      newRow = {
        value: { kind: 'Struct', value: [structType, newMap] },
        attrs: row.attrs ?? [],
      };
    } else {
      throw new Error(
        `handleParentSubArrayUpdate: expected Struct or Unset, got ${row.value.kind}`
      );
    }

    const newArray = [...currentArray];
    newArray[parentRowIndex] = newRow;
    updateParent(newArray);
    invalidateArrayDiffs();
  };

  const handleSubArrayItemDelete = async (
    parentRowIndex: number,
    arrayFieldPath: string[],
    itemIndex: number
  ): Promise<void> => {
    if (!(await confirm('DeleteArrayElement'))) return;

    const row = currentArray[parentRowIndex];
    if (row.value.kind !== 'Struct') {
      throw new Error(
        `handleSubArrayItemDelete: expected Struct, got ${row.value.kind}`
      );
    }

    const [, structData] = row.value.value;
    const arrayValue = getNestedValue(structData, arrayFieldPath);
    if (arrayValue?.value.kind !== 'Array') {
      throw new Error(
        `handleSubArrayItemDelete: expected Array at ${arrayFieldPath.join('.')}`
      );
    }

    const currentSubArray = arrayValue.value.value;
    const newSubArray = currentSubArray.filter((_, i) => i !== itemIndex);
    handleParentSubArrayUpdate(parentRowIndex, arrayFieldPath, newSubArray);
  };

  const handleSubArrayItemMove = (
    parentRowIndex: number,
    arrayFieldPath: string[],
    fromIndex: number,
    toIndex: number
  ): void => {
    const row = currentArray[parentRowIndex];
    if (row.value.kind !== 'Struct') {
      throw new Error(
        `handleSubArrayItemMove: expected Struct, got ${row.value.kind}`
      );
    }

    const [, structData] = row.value.value;
    const arrayValue = getNestedValue(structData, arrayFieldPath);
    if (arrayValue?.value.kind !== 'Array') {
      throw new Error(
        `handleSubArrayItemMove: expected Array at ${arrayFieldPath.join('.')}`
      );
    }

    const currentSubArray = arrayValue.value.value;
    if (toIndex < 0 || toIndex >= currentSubArray.length) return;

    const newSubArray = [...currentSubArray];
    const [movedItem] = newSubArray.splice(fromIndex, 1);
    newSubArray.splice(toIndex, 0, movedItem);
    handleParentSubArrayUpdate(parentRowIndex, arrayFieldPath, newSubArray);
  };

  const handleSubArrayItemDuplicate = (
    parentRowIndex: number,
    arrayFieldPath: string[],
    itemIndex: number,
    position: 'before' | 'after'
  ): void => {
    const row = currentArray[parentRowIndex];
    if (row.value.kind !== 'Struct') {
      throw new Error(
        `handleSubArrayItemDuplicate: expected Struct, got ${row.value.kind}`
      );
    }

    const [, structData] = row.value.value;
    const arrayValue = getNestedValue(structData, arrayFieldPath);
    if (arrayValue?.value.kind !== 'Array') {
      throw new Error(
        `handleSubArrayItemDuplicate: expected Array at ${arrayFieldPath.join('.')}`
      );
    }

    const currentSubArray = arrayValue.value.value;
    const original = currentSubArray[itemIndex];
    if (!original) {
      throw new Error(
        `handleSubArrayItemDuplicate: invalid itemIndex ${itemIndex}`
      );
    }

    const cloned = cloneWithNewUid(original);
    const insertAt = position === 'before' ? itemIndex : itemIndex + 1;
    const newSubArray = [
      ...currentSubArray.slice(0, insertAt),
      cloned,
      ...currentSubArray.slice(insertAt),
    ];
    handleParentSubArrayUpdate(parentRowIndex, arrayFieldPath, newSubArray);
  };

  const handleSubArrayItemLabelChange = (
    parentRowIndex: number,
    arrayFieldPath: string[],
    itemIndex: number,
    newLabel: string
  ): void => {
    const row = currentArray[parentRowIndex];
    if (row.value.kind !== 'Struct') {
      throw new Error(
        `handleSubArrayItemLabelChange: expected Struct, got ${row.value.kind}`
      );
    }

    const [, structData] = row.value.value;
    const arrayValue = getNestedValue(structData, arrayFieldPath);
    if (arrayValue?.value.kind !== 'Array') {
      throw new Error(
        `handleSubArrayItemLabelChange: expected Array at ${arrayFieldPath.join('.')}`
      );
    }

    const currentSubArray = arrayValue.value.value;
    const item = currentSubArray[itemIndex];
    if (!item) {
      throw new Error(
        `handleSubArrayItemLabelChange: invalid itemIndex ${itemIndex}`
      );
    }

    const newItem: RuntimeValue = {
      ...item,
      attrs: setArrayItemLabel(item.attrs, newLabel),
    };

    const newSubArray = [...currentSubArray];
    newSubArray[itemIndex] = newItem;
    handleParentSubArrayUpdate(parentRowIndex, arrayFieldPath, newSubArray);
  };

  const handleSubTableChange = (
    subArray: SubArrayDescriptor,
    itemStructDecl: StructDeclaration,
    newValue: RuntimeValue
  ): void => {
    if (newValue.value.kind !== 'Array') {
      throw new Error(
        `handleSubTableChange: expected Array, got ${newValue.value.kind}`
      );
    }
    const newItems = newValue.value.value;

    // Group items by parent row
    const updatesByParent = new Map<
      number,
      { fieldPath: string[]; updates: Map<number, RuntimeValue> }
    >();

    subArray.items.forEach((item, flatIndex) => {
      if (flatIndex < newItems.length) {
        if (!updatesByParent.has(item.parentRowIndex)) {
          updatesByParent.set(item.parentRowIndex, {
            fieldPath: subArray.fieldPath,
            updates: new Map(),
          });
        }
        updatesByParent
          .get(item.parentRowIndex)!
          .updates.set(item.itemIndex, newItems[flatIndex]);
      }
    });

    // Apply all updates in a single batch
    const newMainArray = [...currentArray];

    updatesByParent.forEach(({ fieldPath, updates }, parentRowIndex) => {
      const row = newMainArray[parentRowIndex];
      if (row.value.kind !== 'Struct') {
        throw new Error(
          `handleSubTableChange: expected Struct at index ${parentRowIndex}, got ${row.value.kind}`
        );
      }

      const [, structData] = row.value.value;
      const arrayValue = getNestedValue(structData, fieldPath);
      if (arrayValue?.value.kind !== 'Array') {
        throw new Error(
          `handleSubTableChange: expected Array at ${fieldPath.join('.')}`
        );
      }

      const currentSubArray = arrayValue.value.value;
      const newSubArray = [...currentSubArray];

      updates.forEach((newVal, itemIdx) => {
        const item = currentSubArray[itemIdx];
        newSubArray[itemIdx] = updateOrConstructStruct(
          item,
          itemStructDecl,
          [],
          newVal
        );
      });

      const newArrayValue: RuntimeValue = createRuntimeValue(
        { kind: 'Array', value: newSubArray },
        arrayValue
      );
      const newStructData = setNestedValue(
        structData,
        fieldPath,
        newArrayValue,
        structType
      );
      newMainArray[parentRowIndex] = {
        ...row,
        value: { kind: 'Struct', value: [row.value.value[0], newStructData] },
      };
    });

    updateParent(newMainArray);
  };

  const handleAddSubArrayItem = (
    parentRowIndex: number,
    arrayFieldPath: string[],
    subElementType: Typ
  ): void => {
    const row = currentArray[parentRowIndex];
    if (row.value.kind !== 'Struct' && row.value.kind !== 'Unset') {
      throw new Error(
        `handleAddSubArrayItem: expected Struct or Unset, got ${row.value.kind}`
      );
    }

    // Get existing sub-array items if row is a Struct, otherwise start with empty array
    let currentSubArray: RuntimeValue[] = [];
    if (row.value.kind === 'Struct') {
      const [, structData] = row.value.value;
      const arrayValue = getNestedValue(structData, arrayFieldPath);
      if (arrayValue?.value.kind === 'Array') {
        currentSubArray = arrayValue.value.value;
      }
    }

    const newElement = cloneWithNewUid(getDefaultValue(subElementType));
    const newSubArray = [...currentSubArray, newElement];
    handleParentSubArrayUpdate(parentRowIndex, arrayFieldPath, newSubArray);
  };

  return {
    updateParent,
    handleAdd,
    handleDelete,
    handleMove,
    handleDuplicate,
    handleLabelChange,
    handleCellUpdate,
    handleParentSubArrayUpdate,
    handleSubArrayItemDelete,
    handleSubArrayItemMove,
    handleSubArrayItemDuplicate,
    handleSubArrayItemLabelChange,
    handleSubTableChange,
    handleAddSubArrayItem,
  };
}
