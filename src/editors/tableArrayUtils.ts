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
interface FlatColumn {
  label: string; // Display label (e.g., "person.date_of_birth")
  fieldPath: string[]; // Path through nested structs (e.g., ["person", "date_of_birth"])
  fieldType: Typ; // The atomic type
}

/**
 * Sub-array item with parent row tracking
 */
interface SubArrayItem {
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
// Table Schema Interface
// ============================================================================

/**
 * Abstraction for how to view array elements as table rows.
 * Allows TableArrayEditor to handle both structs and primitives/enums uniformly.
 */
export interface TableSchema {
  /** Column definitions for the table header */
  columns: FlatColumn[];
  /** Sub-arrays that should be rendered as separate sub-tables */
  subArrays: { label: string; fieldPath: string[]; arrayType: Typ }[];
  /** Extract cell value from a row */
  getCellValue(row: RuntimeValue, column: FlatColumn): RuntimeValue | undefined;
  /** Update cell value in a row, returning the updated row */
  updateCellValue(
    row: RuntimeValue,
    column: FlatColumn,
    newValue: RuntimeValue
  ): RuntimeValue;
}

/** Problem with a specific field that prevents table rendering */
interface FieldProblem {
  field: string;
  reason: string;
}

/** Result of attempting to create a table schema */
type SchemaResult =
  | { ok: true; schema: TableSchema }
  | { ok: false; reasons: FieldProblem[] };

/**
 * Create a TableSchema for struct elements.
 * Columns are derived from flattenStruct, values accessed via nested paths.
 */
function createStructSchema(structDecl: StructDeclaration): TableSchema {
  const { atomicColumns, arrayFields } = flattenStruct(structDecl);

  return {
    columns: atomicColumns,
    subArrays: arrayFields,

    getCellValue(
      row: RuntimeValue,
      column: FlatColumn
    ): RuntimeValue | undefined {
      if (row.value.kind !== 'Struct') return undefined;
      const structData = row.value.value[1];
      return getNestedValue(structData, column.fieldPath);
    },

    updateCellValue(
      row: RuntimeValue,
      column: FlatColumn,
      newValue: RuntimeValue
    ): RuntimeValue {
      if (row.value.kind !== 'Struct') return row;
      const [decl, structData] = row.value.value;
      const updatedData = setNestedValue(
        structData,
        column.fieldPath,
        newValue,
        decl
      );
      return {
        value: { kind: 'Struct', value: [decl, updatedData] },
        attrs: row.attrs,
      };
    },
  };
}

/**
 * Create a TableSchema for simple elements (primitives, simple enums).
 * Single "Value" column, row is the value itself.
 */
function createSimpleSchema(elementType: Typ): TableSchema {
  const column: FlatColumn = {
    label: '', // Empty label for simple values
    fieldPath: [],
    fieldType: elementType,
  };

  return {
    columns: [column],
    subArrays: [],

    getCellValue(row: RuntimeValue, _column: FlatColumn): RuntimeValue {
      return row; // Row IS the value
    },

    updateCellValue(
      _row: RuntimeValue,
      _column: FlatColumn,
      newValue: RuntimeValue
    ): RuntimeValue {
      return newValue; // No wrapping needed
    },
  };
}

/**
 * Attempt to create a TableSchema for an element type.
 *
 * Returns `{ ok: true, schema }` if the type can be rendered as a table,
 * or `{ ok: false, reasons }` with details about why not.
 *
 * This is the single source of truth for table view eligibility.
 */
export function tryCreateTableSchema(elementType: Typ): SchemaResult {
  if (elementType.kind === 'TStruct') {
    const problems = collectStructProblems(elementType.value);
    if (problems.length > 0) {
      return { ok: false, reasons: problems };
    }
    return { ok: true, schema: createStructSchema(elementType.value) };
  }
  // Simple types (primitives, enums) are always supported
  return { ok: true, schema: createSimpleSchema(elementType) };
}

// ============================================================================
// Table View Eligibility Detection
// ============================================================================

/**
 * Describes how a struct field should be rendered in table view.
 * Used by collectStructProblems() and flattenStruct() to ensure consistency.
 */
type FieldRenderStrategy =
  | { kind: 'cell' } // Render in a single table cell (atomics, simple enums)
  | { kind: 'flatten'; struct: StructDeclaration } // Flatten nested struct into columns
  | { kind: 'subTable'; elementType: Typ } // Render as sub-table (any array)
  | { kind: 'unsupported'; reason: string }; // Cannot render in table view

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
 * Determines the render strategy for an array element type.
 *
 * Arrays are rendered as sub-tables. The element type determines
 * whether that's supported:
 * - TStruct: full sub-table with columns
 * - Simple enum/primitive: simple sub-table with one value column
 * - Complex enum (with nested payloads): unsupported
 */
function getArrayElementStrategy(elementType: Typ): FieldRenderStrategy {
  if (elementType.kind === 'TStruct') {
    // Check if the nested struct is itself flattenable
    if (structIsFlattenable(elementType.value)) {
      return { kind: 'subTable', elementType };
    } else {
      return {
        kind: 'unsupported',
        reason: `Array element struct "${elementType.value.struct_name}" is not flattenable`,
      };
    }
  }

  if (elementType.kind === 'TEnum') {
    // Enums with nested payloads (struct, array, etc.) can't be rendered
    // in a simple sub-table cell
    if (enumHasNestedPayload(elementType.value)) {
      return {
        kind: 'unsupported',
        reason: `Array of enum "${elementType.value.enum_name}" has complex payloads`,
      };
    }
    // Simple enums (no payload or primitive payload) → simple sub-table
    return { kind: 'subTable', elementType };
  }

  // Primitives → simple sub-table with one column
  return { kind: 'subTable', elementType };
}

/**
 * Determines how a field type should be rendered in table view.
 */
function getFieldRenderStrategy(fieldType: Typ): FieldRenderStrategy {
  switch (fieldType.kind) {
    case 'TArray':
      return getArrayElementStrategy(fieldType.value);

    case 'TStruct':
      // Nested struct → flatten into columns (if the nested struct is flattenable)
      if (structIsFlattenable(fieldType.value)) {
        return { kind: 'flatten', struct: fieldType.value };
      }
      return {
        kind: 'unsupported',
        reason: `Nested struct "${fieldType.value.struct_name}" is not flattenable`,
      };

    case 'TEnum':
      // Enums with array payloads are problematic (arrays hidden inside enums)
      if (enumPayloadContainsArray(fieldType.value)) {
        return {
          kind: 'unsupported',
          reason: `Enum "${fieldType.value.enum_name}" contains arrays in payloads`,
        };
      }
      return { kind: 'cell' };

    case 'TOption':
      // Options wrapping arrays are unsupported
      if (typeContainsArray(fieldType.value)) {
        return {
          kind: 'unsupported',
          reason: 'Option type contains array',
        };
      }
      return { kind: 'cell' };

    case 'TTuple':
      // Tuples containing arrays are unsupported
      if (fieldType.value.some(typeContainsArray)) {
        return {
          kind: 'unsupported',
          reason: 'Tuple contains array',
        };
      }
      return { kind: 'cell' };

    default:
      // Primitives (TBool, TInt, TRat, TMoney, TDate, TDuration, TUnit, TArrow)
      return { kind: 'cell' };
  }
}

/**
 * Internal: Collect problems that would prevent a struct from being flattened.
 * Used by getFieldRenderStrategy (recursive checks) and tryCreateTableSchema.
 */
function collectStructProblems(structDecl: StructDeclaration): FieldProblem[] {
  const problems: FieldProblem[] = [];
  for (const [fieldName, fieldType] of structDecl.fields.entries()) {
    const strategy = getFieldRenderStrategy(fieldType);
    if (strategy.kind === 'unsupported') {
      problems.push({ field: fieldName, reason: strategy.reason });
    }
  }
  return problems;
}

/** Shorthand for recursive checks within getFieldRenderStrategy. */
export function structIsFlattenable(structDecl: StructDeclaration): boolean {
  return collectStructProblems(structDecl).length === 0;
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
function flattenStruct(
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

    const strategy = getFieldRenderStrategy(fieldType);

    switch (strategy.kind) {
      case 'cell':
        // Render in a single table cell
        atomicColumns.push({ label, fieldPath, fieldType });
        break;

      case 'flatten': {
        // Recurse into nested struct
        const nested = flattenStruct(strategy.struct, fieldPath);
        atomicColumns.push(...nested.atomicColumns);
        arrayFields.push(...nested.arrayFields);
        break;
      }

      case 'subTable':
        // Extract array for sub-table rendering
        if (fieldType.kind === 'TArray') {
          arrayFields.push({ label, fieldPath, arrayType: fieldType });
        }
        break;

      case 'unsupported':
        throw new Error(
          `flattenStruct: Unsupported field "${label}": ${strategy.reason}. ` +
            `This indicates tryCreateTableSchema() was not checked or has a bug.`
        );
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
