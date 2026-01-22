/**
 * Test for adding sub-array items to Unset parent rows.
 *
 * Bug: When clicking the "+" button to add a sub-array item on a row that is Unset,
 * nothing happens because handleAddSubArrayItem bails out early when the row is not a Struct.
 *
 * Expected: The handler should construct a proper Struct with default values and
 * add the new sub-array item to it.
 */

import { describe, it, expect, vi } from 'vitest';
import { renderHook, act } from '@testing-library/react';
import type {
  RuntimeValue,
  StructDeclaration,
  Typ,
} from '../../src/generated/catala_types';
import { useTableArrayHandlers } from '../../src/editors/useTableArrayHandlers';

describe('handleAddSubArrayItem with Unset parent row', () => {
  // Define test types
  const itemStructDecl: StructDeclaration = {
    struct_name: 'Item',
    fields: new Map<string, Typ>([['value', { kind: 'TInt' }]]),
  };

  const parentStructDecl: StructDeclaration = {
    struct_name: 'Parent',
    fields: new Map<string, Typ>([
      ['id', { kind: 'TInt' }],
      [
        'items',
        { kind: 'TArray', value: { kind: 'TStruct', value: itemStructDecl } },
      ],
    ]),
  };

  const elementType: Typ = { kind: 'TStruct', value: parentStructDecl };
  const subElementType: Typ = { kind: 'TStruct', value: itemStructDecl };

  it('should add a sub-array item when parent row is Unset', () => {
    // Create an Unset parent row (this is the bug scenario)
    const unsetRow: RuntimeValue = {
      value: { kind: 'Unset' },
      attrs: [{ kind: 'Uid', value: 'row-1' }],
    };

    const currentArray = [unsetRow];
    const onValueChange = vi.fn();

    const { result } = renderHook(() =>
      useTableArrayHandlers({
        currentArray,
        runtimeValue: {
          value: { kind: 'Array', value: currentArray },
          attrs: [],
        },
        elementType,
        structType: parentStructDecl,
        onValueChange,
        currentPath: [],
        isSubTable: false,
      })
    );

    // Try to add a sub-array item to the Unset row
    act(() => {
      result.current.handleAddSubArrayItem(0, ['items'], subElementType);
    });

    // BUG: With the current code, onValueChange is never called because
    // handleAddSubArrayItem returns early when row.value.kind !== 'Struct'
    expect(onValueChange).toHaveBeenCalled();

    // Verify the new value has the sub-array with one item
    const newValue = onValueChange.mock.calls[0][0] as RuntimeValue;
    expect(newValue.value.kind).toBe('Array');

    const newArray = (
      newValue.value as { kind: 'Array'; value: RuntimeValue[] }
    ).value;
    expect(newArray).toHaveLength(1);

    // The row should now be a Struct (constructed from defaults)
    const newRow = newArray[0];
    expect(newRow.value.kind).toBe('Struct');

    // The items sub-array should have one element
    const [, structData] = (newRow.value as any).value;
    const itemsArray = structData.get('items');
    expect(itemsArray.value.kind).toBe('Array');
    expect(itemsArray.value.value).toHaveLength(1);
  });

  it('should still work when parent row is already a Struct', () => {
    // Create a proper Struct parent row with empty sub-array
    const structRow: RuntimeValue = {
      value: {
        kind: 'Struct',
        value: [
          parentStructDecl,
          new Map<string, RuntimeValue>([
            ['id', { value: { kind: 'Integer', value: 42 }, attrs: [] }],
            ['items', { value: { kind: 'Array', value: [] }, attrs: [] }],
          ]),
        ],
      },
      attrs: [{ kind: 'Uid', value: 'row-1' }],
    };

    const currentArray = [structRow];
    const onValueChange = vi.fn();

    const { result } = renderHook(() =>
      useTableArrayHandlers({
        currentArray,
        runtimeValue: {
          value: { kind: 'Array', value: currentArray },
          attrs: [],
        },
        elementType,
        structType: parentStructDecl,
        onValueChange,
        currentPath: [],
        isSubTable: false,
      })
    );

    // Add a sub-array item to the existing Struct row
    act(() => {
      result.current.handleAddSubArrayItem(0, ['items'], subElementType);
    });

    expect(onValueChange).toHaveBeenCalled();

    const newValue = onValueChange.mock.calls[0][0] as RuntimeValue;
    const newArray = (
      newValue.value as { kind: 'Array'; value: RuntimeValue[] }
    ).value;
    const newRow = newArray[0];

    // The row should still be a Struct
    expect(newRow.value.kind).toBe('Struct');

    // The id field should be preserved
    const [, structData] = (newRow.value as any).value;
    const idValue = structData.get('id');
    expect(idValue.value.kind).toBe('Integer');
    expect(idValue.value.value).toBe(42);

    // The items sub-array should have one element
    const itemsArray = structData.get('items');
    expect(itemsArray.value.kind).toBe('Array');
    expect(itemsArray.value.value).toHaveLength(1);
  });
});
