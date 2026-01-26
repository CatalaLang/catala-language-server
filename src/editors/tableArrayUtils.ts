/**
 * Utility functions for the TableArrayEditor component.
 * These pure functions handle data transformations, cloning, and nested value access.
 * Extracted for testability and reusability.
 */

import type {
  RuntimeValue,
  RuntimeValueRaw,
  Typ,
  PathSegment,
  Diff,
  StructDeclaration,
  EnumDeclaration,
  AttrDef,
} from '../generated/catala_types';
import { getDefaultValue } from './ValueEditors';
import {
  computeActualOnlyIndices,
  findChildIndexDiff,
  isActualOnly,
} from '../diff/arrayPresence';

// ============================================================================
// Type Definitions
// ============================================================================

/**
 * Flattened column descriptor with full path through nested structs
 */
export interface FlatColumn {
  label: string; // Display label (e.g., "person.date_of_birth")
  fieldPath: string[]; // Path through nested structs (e.g., ["person", "date_of_birth"])
  fieldType: Typ; // The atomic type
}

/**
 * Sub-array item with parent row tracking
 */
export interface SubArrayItem {
  parentRowIndex: number; // Index of the parent row in the main table
  itemIndex: number; // Index within the parent's array
  value: RuntimeValue; // The array item value
  isPhantom?: boolean; // True for actual-only diff items
}

/**
 * Sub-array descriptor with items from all parent rows
 */
export interface SubArrayDescriptor {
  label: string; // Display label (e.g., "person.roles")
  fieldPath: string[]; // Path to array field
  arrayType: Typ; // TArray type
  items: SubArrayItem[]; // All items from all parent rows
}

// ============================================================================
// Table View Eligibility Detection
// ============================================================================

/**
 * Recursively checks if a type is or contains an array at any depth.
 * Used to detect arrays hidden inside wrapper types (enums, options, tuples).
 */
function typeContainsArray(typ: Typ): boolean {
  switch (typ.kind) {
    case 'TArray':
      return true;
    case 'TStruct':
      return Array.from(typ.value.fields.values()).some(typeContainsArray);
    case 'TEnum':
      return enumPayloadContainsArray(typ.value);
    case 'TOption':
      return typeContainsArray(typ.value);
    case 'TTuple':
      return typ.value.some(typeContainsArray);
    default:
      // Primitives (TBool, TInt, TRat, TMoney, TDate, TDuration, TUnit, TArrow)
      return false;
  }
}

/**
 * Checks if an enum has any constructor with a payload that contains an array.
 */
function enumPayloadContainsArray(enumDecl: EnumDeclaration): boolean {
  for (const ctorType of enumDecl.constructors.values()) {
    if (ctorType?.value && typeContainsArray(ctorType.value)) {
      return true;
    }
  }
  return false;
}

/**
 * Checks if an enum has any constructor with a nested/structured payload.
 * Nested means: struct, array, nested enum, option, or tuple.
 * Simple payloads (primitives like int, date, money) are fine for inline rendering.
 */
function enumHasNestedPayload(enumDecl: EnumDeclaration): boolean {
  for (const ctorType of enumDecl.constructors.values()) {
    if (ctorType?.value) {
      const payloadKind = ctorType.value.kind;
      if (
        payloadKind === 'TStruct' ||
        payloadKind === 'TArray' ||
        payloadKind === 'TEnum' ||
        payloadKind === 'TOption' ||
        payloadKind === 'TTuple'
      ) {
        return true;
      }
    }
  }
  return false;
}

/**
 * Checks if an array element type can be rendered in a sub-table.
 *
 * Sub-table rendering (TableArrayEditor) only handles TArray<TStruct>.
 * TArray<TEnum> with nested payloads won't render properly.
 *
 * @param elementType - The element type of the array
 * @returns true if the array element supports sub-table rendering
 */
function arrayElementIsFlattenable(elementType: Typ): boolean {
  if (elementType.kind === 'TEnum') {
    // TArray<TEnum> with nested payloads is not flattenable
    // because sub-table rendering returns null for non-struct arrays.
    // Simple enums (no payload or primitive payload) are fine.
    return !enumHasNestedPayload(elementType.value);
  }
  // TArray<TStruct> is fine (proper sub-table support)
  // TArray<primitive> is rare and also not fully supported, but we allow it
  return true;
}

/**
 * Checks if a struct field type can be rendered in a table cell or sub-table.
 *
 * - TArray<TStruct>: flattenable (handled as sub-table)
 * - TArray<TEnum> with nested payloads: not flattenable
 * - TStruct: recursively check nested fields
 * - TEnum: flattenable unless payload contains arrays
 * - TOption: flattenable unless inner type contains arrays
 * - TTuple: flattenable unless any element contains arrays
 * - Primitives: flattenable
 */
function fieldTypeIsFlattenable(typ: Typ): boolean {
  switch (typ.kind) {
    case 'TArray':
      // Arrays at struct field level normally become sub-tables.
      // BUT: TArray<TEnum> with nested payloads is not flattenable because
      // sub-table rendering only handles TArray<TStruct>, not TArray<TEnum>.
      // See TableArrayEditor.tsx lines 821-970.
      return arrayElementIsFlattenable(typ.value);
    case 'TStruct':
      // Recursively check nested struct fields
      return structIsFlattenable(typ.value);
    case 'TEnum':
      // Flattenable unless payload contains arrays
      return !enumPayloadContainsArray(typ.value);
    case 'TOption':
      // Flattenable unless option wraps an array
      return !typeContainsArray(typ.value);
    case 'TTuple':
      // Flattenable unless any tuple element contains an array
      return !typ.value.some(typeContainsArray);
    default:
      // Primitives are always flattenable
      return true;
  }
}

/**
 * Checks if a struct can be flattened into a table view.
 *
 * A struct is flattenable if all its fields can be rendered either:
 * - As table columns (primitives, simple enums)
 * - As sub-tables (arrays of structs)
 *
 * Not flattenable: arrays hidden inside enum payloads, option types, or tuples.
 * These would need to render inline in cells, which doesn't work well for arrays.
 *
 * @param structDecl - The struct declaration to analyze
 * @returns true if the struct can be rendered as a table
 *
 * @example
 * // Flattenable - arrays become sub-tables:
 * struct Order { id: int, items: Item[] }
 *
 * // Not flattenable - array hidden in enum payload:
 * struct Order { id: int, payment: PaymentMethod }
 * // where PaymentMethod = Cash | Installments(Payment[])
 *
 * ---
 * Future enhancement: Consider showing user feedback when falling back to tree view.
 * Could display a subtle indicator like "Displayed as tree (contains variable-structure fields)"
 * to help users understand why table view wasn't used. See docs/tabular-view-complex-types.md
 */
export function structIsFlattenable(structDecl: StructDeclaration): boolean {
  for (const fieldType of structDecl.fields.values()) {
    if (!fieldTypeIsFlattenable(fieldType)) {
      return false;
    }
  }
  return true;
}

// ============================================================================
// Runtime Value Cloning
// ============================================================================

/**
 * Deep clone a RuntimeValue, properly handling Map objects and nested structures.
 *
 * @param value - The RuntimeValue to clone
 * @returns A deep copy of the RuntimeValue
 *
 * @example
 * const original = { value: { kind: 'Struct', value: [...] }, attrs: [...] };
 * const cloned = deepCloneRuntimeValue(original);
 * // cloned is a completely independent copy
 */
function deepCloneRuntimeValue(value: RuntimeValue): RuntimeValue {
  const cloneValue = (raw: RuntimeValueRaw): RuntimeValueRaw => {
    switch (raw.kind) {
      case 'Struct': {
        const [structDecl, structData] = raw.value;
        const clonedData = new Map<string, RuntimeValue>();
        for (const [key, val] of structData.entries()) {
          clonedData.set(key, deepCloneRuntimeValue(val));
        }
        // StructDeclaration contains Maps too, but it's type metadata - share reference
        return { kind: 'Struct', value: [structDecl, clonedData] };
      }
      case 'Array':
        return {
          kind: 'Array',
          value: raw.value.map((item) => deepCloneRuntimeValue(item)),
        };
      case 'Enum': {
        const [enumDecl, [variantName, innerValue]] = raw.value;
        const clonedInner = innerValue
          ? { value: deepCloneRuntimeValue(innerValue.value) }
          : null;
        return {
          kind: 'Enum',
          value: [enumDecl, [variantName, clonedInner]],
        };
      }
      default:
        // Primitive types (Bool, Money, Integer, Decimal, Date, Duration, Unset, Empty)
        return { ...raw };
    }
  };

  // Regenerate UID if present, preserve other attrs
  const clonedAttrs = value.attrs
    ? value.attrs.map((attr) =>
        attr.kind === 'Uid'
          ? { kind: 'Uid' as const, value: String(crypto.randomUUID()) }
          : attr
      )
    : [];

  return {
    value: cloneValue(value.value),
    attrs: clonedAttrs,
  };
}

/**
 * Clone a RuntimeValue and assign a fresh UID attribute.
 * Removes any existing UID attribute and generates a new one.
 *
 * @param original - The RuntimeValue to clone
 * @returns A cloned RuntimeValue with a new UID
 *
 * @example
 * const original = { value: { kind: 'Struct', ... }, attrs: [{ kind: 'Uid', value: 'old-id' }] };
 * const cloned = cloneWithNewUid(original);
 * // cloned.attrs contains a new Uid with a fresh UUID
 */
export function cloneWithNewUid(original: RuntimeValue): RuntimeValue {
  const cloned = deepCloneRuntimeValue(original);
  cloned.attrs = [
    ...(cloned.attrs ?? []).filter((attr) => attr.kind !== 'Uid'),
    { kind: 'Uid', value: String(crypto.randomUUID()) },
  ];
  return cloned;
}

// ============================================================================
// Array Item Label Management
// ============================================================================

/**
 * Get the ArrayItemLabel attribute value from a RuntimeValue's attributes.
 *
 * @param attrs - The attributes array to search
 * @returns The label string if found, undefined otherwise
 *
 * @example
 * const label = getArrayItemLabel(runtimeValue.attrs);
 * // => "John Doe" or undefined
 */
export function getArrayItemLabel(
  attrs: AttrDef[] | undefined
): string | undefined {
  const label = attrs?.find((attr) => attr.kind === 'ArrayItemLabel');
  return label?.kind === 'ArrayItemLabel' ? label.value : undefined;
}

/**
 * Set or update the ArrayItemLabel attribute in an attributes array.
 * If label is empty, removes the ArrayItemLabel attribute.
 *
 * @param attrs - The current attributes array
 * @param label - The new label string
 * @returns A new attributes array with the label set or removed
 *
 * @example
 * const newAttrs = setArrayItemLabel(attrs, "Jane Doe");
 * // => [...otherAttrs, { kind: 'ArrayItemLabel', value: 'Jane Doe' }]
 *
 * const clearedAttrs = setArrayItemLabel(attrs, "");
 * // => [...otherAttrs] (ArrayItemLabel removed)
 */
export function setArrayItemLabel(
  attrs: AttrDef[] | undefined,
  label: string
): AttrDef[] {
  const filtered = (attrs ?? []).filter(
    (attr) => attr.kind !== 'ArrayItemLabel'
  );
  if (label.trim() === '') {
    return filtered; // Remove label if empty
  }
  return [...filtered, { kind: 'ArrayItemLabel', value: label }];
}

// ============================================================================
// Type Guards
// ============================================================================

/**
 * Type guard: Check if a RuntimeValue is a Struct.
 *
 * @param row - The RuntimeValue to check
 * @returns True if the value is a Struct, false otherwise
 *
 * @example
 * if (isStructRow(runtimeValue)) {
 *   // TypeScript knows value.value.value is [StructDeclaration, Map<...>]
 *   const [structDecl, structData] = runtimeValue.value.value;
 * }
 */
export function isStructRow(
  row: RuntimeValue | undefined
): row is RuntimeValue & {
  value: {
    kind: 'Struct';
    value: [StructDeclaration, Map<string, RuntimeValue>];
  };
} {
  return row?.value.kind === 'Struct';
}

// ============================================================================
// Struct Flattening
// ============================================================================

/**
 * Recursively flatten a struct declaration into atomic columns and array fields.
 *
 * Atomic columns are scalar/enum fields suitable for display in the main table.
 * Array fields are recursively extracted for rendering as sub-tables.
 * Nested structs are flattened recursively with dot-notation paths.
 *
 * @param structDecl - The struct declaration to flatten
 * @param pathPrefix - Current path prefix for nested recursion (internal use)
 * @returns Object containing atomicColumns and arrayFields
 *
 * @example
 * const { atomicColumns, arrayFields } = flattenStruct(personStruct);
 * // atomicColumns => [{ label: "name", fieldPath: ["name"], fieldType: TString }]
 * // arrayFields => [{ label: "addresses", fieldPath: ["addresses"], arrayType: TArray<Address> }]
 *
 * @example
 * // With nested struct:
 * const { atomicColumns } = flattenStruct(orderStruct);
 * // atomicColumns => [
 * //   { label: "customer.name", fieldPath: ["customer", "name"], ... },
 * //   { label: "customer.email", fieldPath: ["customer", "email"], ... }
 * // ]
 */
export function flattenStruct(
  structDecl: StructDeclaration,
  pathPrefix: string[] = []
): {
  atomicColumns: FlatColumn[];
  arrayFields: { label: string; fieldPath: string[]; arrayType: Typ }[];
} {
  const atomicColumns: FlatColumn[] = [];
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

// ============================================================================
// Nested Value Access
// ============================================================================

/**
 * Get a nested field value from a struct by following a path.
 *
 * @param structData - The struct's Map of field values
 * @param fieldPath - Array of field names representing the path
 * @returns The RuntimeValue at the path, or undefined if not found
 *
 * @example
 * const structData = new Map([
 *   ['person', { value: { kind: 'Struct', value: [decl, new Map([
 *     ['name', { value: { kind: 'String', value: 'Alice' } }]
 *   ])] } }]
 * ]);
 *
 * const name = getNestedValue(structData, ['person', 'name']);
 * // => { value: { kind: 'String', value: 'Alice' } }
 */
export function getNestedValue(
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

/**
 * Set a nested field value in a struct by following a path.
 * Returns a new Map with the updated value (immutable update).
 *
 * @param structData - The struct's Map of field values
 * @param fieldPath - Array of field names representing the path
 * @param newValue - The new RuntimeValue to set at the path
 * @param structType - The struct declaration for type information
 * @returns A new Map with the updated value
 *
 * @example
 * const updatedData = setNestedValue(
 *   structData,
 *   ['person', 'name'],
 *   { value: { kind: 'String', value: 'Bob' }, attrs: [] },
 *   personStructDecl
 * );
 * // Returns new Map with person.name updated to 'Bob'
 */
export function setNestedValue(
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
      let currentNestedData: Map<string, RuntimeValue>;

      if (currentValue?.value.kind === 'Struct') {
        currentNestedData = currentValue.value.value[1];
      } else {
        // Initialize all fields of the nested struct with defaults
        currentNestedData = new Map<string, RuntimeValue>();
        for (const [fieldName, fieldTyp] of nestedStructDecl.fields.entries()) {
          currentNestedData.set(fieldName, getDefaultValue(fieldTyp));
        }
      }

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

/**
 * Update a struct field or construct a full struct if the item is unset.
 * Handles both cases:
 * 1. Item is already a Struct: updates the specific field
 * 2. Item is Unset/Invalid: constructs a full struct with all fields initialized
 *
 * @param item - The current RuntimeValue (may be undefined or Unset)
 * @param structDecl - The struct declaration
 * @param fieldPath - Path to the field to update (empty array = replace entire item)
 * @param newValue - The new value to set
 * @returns Updated or newly constructed RuntimeValue
 *
 * @example
 * // Update existing struct
 * const updated = updateOrConstructStruct(
 *   existingItem,
 *   personStructDecl,
 *   ['name'],
 *   { value: { kind: 'String', value: 'Alice' }, attrs: [] }
 * );
 *
 * @example
 * // Construct new struct from unset
 * const newItem = updateOrConstructStruct(
 *   { value: { kind: 'Unset' }, attrs: [] },
 *   personStructDecl,
 *   ['name'],
 *   { value: { kind: 'String', value: 'Bob' }, attrs: [] }
 * );
 * // Creates full struct with all fields initialized, name set to 'Bob'
 */
export function updateOrConstructStruct(
  item: RuntimeValue | undefined,
  structDecl: StructDeclaration,
  fieldPath: string[],
  newValue: RuntimeValue
): RuntimeValue {
  // If fieldPath is empty, we're replacing the entire item, not updating a field
  if (fieldPath.length === 0) {
    // newValue should be a complete RuntimeValue (not just the inner value)
    return {
      ...newValue,
      attrs: item?.attrs ?? newValue.attrs,
    };
  }

  if (item?.value.kind === 'Struct') {
    // Existing struct - update the specific field
    const [, itemStructData] = item.value.value;
    const newItemMap = setNestedValue(
      itemStructData,
      fieldPath,
      newValue,
      structDecl
    );

    return {
      ...item,
      value: { kind: 'Struct', value: [structDecl, newItemMap] },
    };
  } else {
    // Unset/Invalid/etc. - construct a full struct with all fields initialized
    const newItemMap = new Map<string, RuntimeValue>();
    for (const [fieldName, fieldType] of structDecl.fields.entries()) {
      newItemMap.set(fieldName, getDefaultValue(fieldType));
    }

    // Set the edited field
    const finalItemMap = setNestedValue(
      newItemMap,
      fieldPath,
      newValue,
      structDecl
    );

    return {
      value: { kind: 'Struct', value: [structDecl, finalItemMap] },
      attrs: item?.attrs ?? [],
    };
  }
}

// ============================================================================
// Sub-Array Computation
// ============================================================================

/**
 * Compute sub-arrays from all rows, grouped by field.
 * Also collects phantom items from actual-only diffs.
 *
 * This function flattens arrays across all parent rows for rendering in sub-tables.
 * Each sub-array item tracks its parent row index and position within that parent.
 *
 * @param rows - Array of parent row RuntimeValues
 * @param arrayFields - Array field descriptors from flattenStruct
 * @param diffs - Diffs for phantom item detection
 * @param currentPath - Current path in the value tree
 * @returns Array of SubArrayDescriptors, one per array field with items
 *
 * @example
 * const subArrays = computeSubArrays(
 *   [person1, person2, person3],
 *   [{ label: 'addresses', fieldPath: ['addresses'], arrayType: ... }],
 *   diffs,
 *   []
 * );
 * // Returns:
 * // [{
 * //   label: 'addresses',
 * //   fieldPath: ['addresses'],
 * //   arrayType: TArray<Address>,
 * //   items: [
 * //     { parentRowIndex: 0, itemIndex: 0, value: address1 },
 * //     { parentRowIndex: 0, itemIndex: 1, value: address2 },
 * //     { parentRowIndex: 1, itemIndex: 0, value: address3 },
 * //     ...
 * //   ]
 * // }]
 */
export function computeSubArrays(
  rows: RuntimeValue[],
  arrayFields: { label: string; fieldPath: string[]; arrayType: Typ }[],
  diffs: Diff[],
  currentPath: PathSegment[]
): SubArrayDescriptor[] {
  // Group by field name
  const groupedByField = new Map<string, SubArrayDescriptor>();

  arrayFields.forEach(({ label, fieldPath, arrayType }) => {
    const items: SubArrayItem[] = [];

    // Collect items from all parent rows for this field
    rows.forEach((row, parentRowIndex) => {
      // Skip non-Struct rows since they have no fields to extract arrays from
      if (row.value.kind !== 'Struct') return;
      const structData = row.value.value[1];
      const arrayValue = getNestedValue(structData, fieldPath);

      if (arrayValue?.value.kind === 'Array') {
        // Add all items from this parent row's array
        arrayValue.value.value.forEach((value, itemIndex) => {
          items.push({ parentRowIndex, itemIndex, value, isPhantom: false });
        });
      }

      // Collect phantom items for this parent's sub-array
      const subArrayPath = [
        ...currentPath,
        { kind: 'ListIndex' as const, value: parentRowIndex },
        ...fieldPath.map(
          (f): PathSegment => ({ kind: 'StructField', value: f })
        ),
      ];
      const phantomIndices = computeActualOnlyIndices(diffs, subArrayPath);
      phantomIndices.forEach((phantomIdx) => {
        // Find the actual-only diff for this index
        const diff = findChildIndexDiff(diffs, subArrayPath, phantomIdx);
        if (diff && isActualOnly(diff)) {
          items.push({
            parentRowIndex,
            itemIndex: phantomIdx,
            value: diff.actual as RuntimeValue,
            isPhantom: true,
          });
        }
      });
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

// ============================================================================
// Path Building
// ============================================================================

/**
 * Build a PathSegment array for a specific row and field in the table.
 *
 * @param currentPath - The base path to the array
 * @param rowIndex - Index of the row in the array
 * @param fieldPath - Path to the field within the struct
 * @returns Complete PathSegment array for the cell
 *
 * @example
 * const cellPath = buildCellPath(
 *   [{ kind: 'StructField', value: 'people' }],
 *   2,
 *   ['person', 'name']
 * );
 * // => [
 * //   { kind: 'StructField', value: 'people' },
 * //   { kind: 'ListIndex', value: 2 },
 * //   { kind: 'StructField', value: 'person' },
 * //   { kind: 'StructField', value: 'name' }
 * // ]
 */
export function buildCellPath(
  currentPath: PathSegment[],
  rowIndex: number,
  fieldPath: string[]
): PathSegment[] {
  return [
    ...currentPath,
    { kind: 'ListIndex', value: rowIndex },
    ...fieldPath.map(
      (field): PathSegment => ({ kind: 'StructField', value: field })
    ),
  ];
}
