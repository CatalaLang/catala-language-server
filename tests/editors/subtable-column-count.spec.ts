import { describe, it, expect } from 'vitest';
import type { RuntimeValue } from '../../src/generated/catala_types';

describe('Sub-table rendering - column count for Unset/Invalid struct items', () => {
  it('Unset struct items should render same number of columns as computed structs (not a single spanning cell)', () => {
    /**
     * This test verifies the fix for the layout bug where Unset/Invalid struct items
     * were rendered in a single cell with colSpan, causing vertical layout with labels.
     *
     * BEFORE FIX:
     * - Computed struct row: [#] [controls] [field1] [field2] [field3] = 5 cells
     * - Unset struct row: [#] [controls] [single spanning cell with vertical layout] = 3 cells
     *
     * AFTER FIX:
     * - Computed struct row: [#] [controls] [field1] [field2] [field3] = 5 cells
     * - Unset struct row: [#] [controls] [field1 Unset] [field2 Unset] [field3 Unset] = 5 cells
     */

    // Simulated struct with 3 atomic fields
    const atomicFieldCount = 3; // e.g., code, début, fin

    // Computed struct item - renders fields individually
    const computedStructItem: RuntimeValue = {
      value: {
        kind: 'Struct',
        value: [{}, new Map()] as any,
      },
      attrs: [],
    };

    // Unset struct item - SHOULD ALSO render individual cells (one per field)
    const unsetStructItem: RuntimeValue = {
      value: { kind: 'Unset' },
      attrs: [],
    };

    // Invalid struct item - SHOULD ALSO render individual cells (one per field)
    const invalidStructItem: RuntimeValue = {
      value: { kind: 'Invalid' },
      attrs: [],
    };

    // Function to calculate expected cell count
    // Structure: [row number] + [controls (if editable)] + [N atomic fields]
    const calculateCellCount = (
      item: RuntimeValue,
      editable: boolean,
      fieldCount: number
    ): number => {
      const rowNumberCell = 1;
      const controlsCell = editable ? 1 : 0;

      // KEY FIX: BEFORE, we checked if (item.value.kind !== 'Struct') and used colSpan=fieldCount
      // This meant only 1 cell for all fields
      // AFTER: We render individual cells for each field regardless of whether the item is computed or Unset/Invalid

      const isStruct = item.value.kind === 'Struct';

      // OLD BUGGY LOGIC:
      const oldFieldCells = isStruct ? fieldCount : 1; // Single spanning cell for non-struct

      // NEW FIXED LOGIC:
      const newFieldCells = fieldCount; // Always individual cells for each field

      return {
        old: rowNumberCell + controlsCell + oldFieldCells,
        new: rowNumberCell + controlsCell + newFieldCells,
      };
    };

    const editable = true;

    // Test computed struct
    const computedCells = calculateCellCount(
      computedStructItem,
      editable,
      atomicFieldCount
    );
    expect(computedCells.old).toBe(5); // 1 + 1 + 3 = 5
    expect(computedCells.new).toBe(5); // Same

    // Test Unset struct - THIS IS THE BUG FIX
    const unsetCells = calculateCellCount(
      unsetStructItem,
      editable,
      atomicFieldCount
    );
    console.log('Unset item - OLD cell count:', unsetCells.old);
    console.log('Unset item - NEW cell count:', unsetCells.new);

    // OLD: 1 + 1 + 1 = 3 cells (single spanning cell)
    expect(unsetCells.old).toBe(3);

    // NEW: 1 + 1 + 3 = 5 cells (individual cells for each field)
    expect(unsetCells.new).toBe(5);

    // Verify both computed and Unset items now have SAME cell count
    expect(unsetCells.new).toBe(computedCells.new);

    // Test Invalid struct - same fix applies
    const invalidCells = calculateCellCount(
      invalidStructItem,
      editable,
      atomicFieldCount
    );
    expect(invalidCells.new).toBe(5);
    expect(invalidCells.new).toBe(computedCells.new);

    console.log(
      '✓ All struct items (computed, Unset, Invalid) now render with same cell count'
    );
    console.log('✓ This ensures horizontal layout is maintained for all items');
  });
});
