/**
 * Tests that missing or extra array elements are displayed as phantom rows
 * in the TableArrayEditor diff view.
 *
 * - Extra elements: Items in actual but not expected (actual-only diffs)
 *   -> Displayed as "phantom rows" with an Accept button
 *
 * - Missing elements: Items in expected but not actual (expected-only diffs)
 *   -> Displayed as "expected-only rows" with a Remove button
 */
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, within, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../../src/locales/en.json';
import type {
  Diff,
  PathSegment,
  RuntimeValue,
  RuntimeValueRaw,
  StructDeclaration,
  ValueDef,
} from '../../src/generated/catala_types';
import { TableArrayEditor } from '../../src/editors/TableArrayEditor';

// Mock confirm to auto-approve destructive actions
vi.mock('../../src/messaging/confirm', () => ({
  confirm: async () => true,
}));

// Helper to create a RuntimeValue
function rv(value: RuntimeValueRaw): RuntimeValue {
  return { value, attrs: [] };
}

// Helper to create a RuntimeValue with a UID attribute
function rvWithUid(value: RuntimeValueRaw, uid: string): RuntimeValue {
  return { value, attrs: [{ kind: 'Uid', value: uid }] };
}

// Helper to create an array ValueDef
function arrayValueDef(items: RuntimeValue[]): ValueDef {
  return { value: rv({ kind: 'Array', value: items }) };
}

// Helper to create an integer RuntimeValue
function intRV(n: number): RuntimeValue {
  return rv({ kind: 'Integer', value: n });
}

// Helper to create an Empty RuntimeValue (used for missing values in diffs)
function emptyRV(): RuntimeValue {
  return rv({ kind: 'Empty' });
}

// Helper to create a struct RuntimeValue
function structRV(
  structDecl: StructDeclaration,
  fields: Map<string, RuntimeValue>,
  uid?: string
): RuntimeValue {
  const value: RuntimeValueRaw = {
    kind: 'Struct',
    value: [structDecl, fields],
  };
  return uid ? rvWithUid(value, uid) : rv(value);
}

// Render helper with IntlProvider
function renderTableEditor(props: {
  structType: StructDeclaration;
  valueDef: ValueDef;
  diffs: Diff[];
  onValueChange?: (newValue: RuntimeValue) => void;
  onDiffResolved?: (path: PathSegment[]) => void;
}) {
  const currentPath: PathSegment[] = [];
  return render(
    <IntlProvider locale="en" messages={enMessages}>
      <TableArrayEditor
        elementType={{ kind: 'TStruct', value: props.structType }}
        structType={props.structType}
        valueDef={props.valueDef}
        onValueChange={props.onValueChange ?? (() => {})}
        currentPath={currentPath}
        diffs={props.diffs}
        editable={true}
        onDiffResolved={props.onDiffResolved}
      />
    </IntlProvider>
  );
}

describe('TableArrayEditor - Missing and Extra Elements in Diff View', () => {
  const employeeStruct: StructDeclaration = {
    struct_name: 'Employee',
    fields: new Map([
      ['id', { kind: 'TInt' }],
      ['salary', { kind: 'TInt' }],
    ]),
  };

  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('Extra elements (actual-only diffs)', () => {
    it('shows phantom row for one extra element at end of array', () => {
      // Current array has 2 items, but actual has 3 (one extra at index 2)
      const item1 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(1)],
          ['salary', intRV(50000)],
        ]),
        'emp-1'
      );
      const item2 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(2)],
          ['salary', intRV(60000)],
        ]),
        'emp-2'
      );
      const extraItem = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(3)],
          ['salary', intRV(70000)],
        ]),
        'emp-extra'
      );

      const diffs: Diff[] = [
        {
          path: [{ kind: 'ListIndex', value: 2 }],
          expected: emptyRV(), // Not expected
          actual: extraItem, // But exists in actual
        },
      ];

      const { container } = renderTableEditor({
        structType: employeeStruct,
        valueDef: arrayValueDef([item1, item2]),
        diffs,
      });

      // Should have 2 regular rows + 1 phantom row
      const allRows = container.querySelectorAll('tbody tr');
      expect(allRows.length).toBe(3);

      // Verify phantom row exists with correct styling
      const phantomRows = container.querySelectorAll('.phantom-row');
      expect(phantomRows).toHaveLength(1);

      // Phantom row should have the indicator showing "Empty" for expected
      const phantomIndicator = container.querySelector(
        '.phantom-row-indicator'
      );
      expect(phantomIndicator).toBeTruthy();

      // Should show "Actual value" label
      expect(screen.getByText('Actual value')).toBeInTheDocument();
    });

    it('shows phantom rows for multiple extra elements', () => {
      // Current array has 1 item, but actual has 3 (two extra)
      const item1 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(1)],
          ['salary', intRV(50000)],
        ]),
        'emp-1'
      );
      const extraItem1 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(2)],
          ['salary', intRV(60000)],
        ])
      );
      const extraItem2 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(3)],
          ['salary', intRV(70000)],
        ])
      );

      const diffs: Diff[] = [
        {
          path: [{ kind: 'ListIndex', value: 1 }],
          expected: emptyRV(),
          actual: extraItem1,
        },
        {
          path: [{ kind: 'ListIndex', value: 2 }],
          expected: emptyRV(),
          actual: extraItem2,
        },
      ];

      const { container } = renderTableEditor({
        structType: employeeStruct,
        valueDef: arrayValueDef([item1]),
        diffs,
      });

      // Should have 1 regular row + 2 phantom rows
      const phantomRows = container.querySelectorAll('.phantom-row');
      expect(phantomRows).toHaveLength(2);
    });

    it('displays the actual values in phantom row cells', () => {
      // Phantom row with specific values that should be displayed
      const extraItem = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(999)],
          ['salary', intRV(123456)],
        ])
      );

      const diffs: Diff[] = [
        {
          path: [{ kind: 'ListIndex', value: 0 }],
          expected: emptyRV(),
          actual: extraItem,
        },
      ];

      renderTableEditor({
        structType: employeeStruct,
        valueDef: arrayValueDef([]), // Empty array
        diffs,
      });

      // The phantom row should display the actual values in input fields
      expect(screen.getByDisplayValue('999')).toBeInTheDocument();
      expect(screen.getByDisplayValue('123456')).toBeInTheDocument();
    });

    it('shows Accept button only for appendable phantom rows', () => {
      // Phantom at index 2 when array length is 2 -> appendable (index === length)
      const item1 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(1)],
          ['salary', intRV(50000)],
        ])
      );
      const item2 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(2)],
          ['salary', intRV(60000)],
        ])
      );
      const extraItem = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(3)],
          ['salary', intRV(70000)],
        ])
      );

      const diffs: Diff[] = [
        {
          path: [{ kind: 'ListIndex', value: 2 }], // index 2, array length 2 -> appendable
          expected: emptyRV(),
          actual: extraItem,
        },
      ];

      const { container } = renderTableEditor({
        structType: employeeStruct,
        valueDef: arrayValueDef([item1, item2]),
        diffs,
      });

      // Accept button should be present (index === arrayLength)
      const acceptButton = container.querySelector('.phantom-accept');
      expect(acceptButton).toBeTruthy();
    });

    it('does not show Accept button for non-appendable phantom rows', () => {
      // Phantom at index 1 when array length is 2 -> NOT appendable (would insert in middle)
      const item1 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(1)],
          ['salary', intRV(50000)],
        ])
      );
      const item2 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(2)],
          ['salary', intRV(60000)],
        ])
      );
      const extraItemInMiddle = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(99)],
          ['salary', intRV(99999)],
        ])
      );

      // Diff at index 1, but there are items at index 0 and 1 in current array
      // This scenario: actual has item at index 1 that current doesn't expect
      // Since array has length 2, index 1 is NOT appendable (1 !== 2)
      const diffs: Diff[] = [
        {
          path: [{ kind: 'ListIndex', value: 3 }], // index 3, but array length is 2
          expected: emptyRV(),
          actual: extraItemInMiddle,
        },
      ];

      const { container } = renderTableEditor({
        structType: employeeStruct,
        valueDef: arrayValueDef([item1, item2]),
        diffs,
      });

      // This phantom is at index 3, array length is 2, so NOT appendable (3 !== 2)
      const acceptButton = container.querySelector('.phantom-accept');
      expect(acceptButton).toBeFalsy();
    });
  });

  describe('Missing elements (expected-only diffs)', () => {
    it('shows expected-only styling for element present in expected but not actual', () => {
      // Array has 2 items, but diff says index 1 should be removed (expected has value, actual is empty)
      const item1 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(1)],
          ['salary', intRV(50000)],
        ]),
        'emp-1'
      );
      const itemToRemove = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(2)],
          ['salary', intRV(60000)],
        ]),
        'emp-2'
      );

      const diffs: Diff[] = [
        {
          path: [{ kind: 'ListIndex', value: 1 }],
          expected: itemToRemove, // Expected to exist
          actual: emptyRV(), // But doesn't in actual
        },
      ];

      const { container } = renderTableEditor({
        structType: employeeStruct,
        valueDef: arrayValueDef([item1, itemToRemove]),
        diffs,
      });

      // Should have expected-only row styling
      const expectedOnlyRows = container.querySelectorAll('.expected-only-row');
      expect(expectedOnlyRows).toHaveLength(1);

      // Should have expected-only indicator
      const expectedOnlyIndicator = container.querySelector(
        '.expected-only-row-indicator'
      );
      expect(expectedOnlyIndicator).toBeTruthy();
    });

    it('shows Remove button only for last element in expected-only rows', () => {
      // Array has 2 items, expected-only diff at index 1 (last) -> removable
      const item1 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(1)],
          ['salary', intRV(50000)],
        ])
      );
      const itemToRemove = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(2)],
          ['salary', intRV(60000)],
        ])
      );

      const diffs: Diff[] = [
        {
          path: [{ kind: 'ListIndex', value: 1 }], // Last index (length - 1)
          expected: itemToRemove,
          actual: emptyRV(),
        },
      ];

      const { container } = renderTableEditor({
        structType: employeeStruct,
        valueDef: arrayValueDef([item1, itemToRemove]),
        diffs,
      });

      // Remove button should be present for last item
      const removeButton = container.querySelector('.phantom-remove');
      expect(removeButton).toBeTruthy();
    });

    it('does not show Remove button for non-last expected-only rows', () => {
      // Array has 3 items, expected-only diff at index 1 (middle) -> NOT removable
      const item1 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(1)],
          ['salary', intRV(50000)],
        ])
      );
      const middleItem = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(2)],
          ['salary', intRV(60000)],
        ])
      );
      const item3 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(3)],
          ['salary', intRV(70000)],
        ])
      );

      const diffs: Diff[] = [
        {
          path: [{ kind: 'ListIndex', value: 1 }], // Middle index, not removable
          expected: middleItem,
          actual: emptyRV(),
        },
      ];

      const { container } = renderTableEditor({
        structType: employeeStruct,
        valueDef: arrayValueDef([item1, middleItem, item3]),
        diffs,
      });

      // Remove button should NOT be present for middle item
      const removeButton = container.querySelector('.phantom-remove');
      expect(removeButton).toBeFalsy();
    });

    it('expected-only rows have no move buttons (read-only)', () => {
      const item1 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(1)],
          ['salary', intRV(50000)],
        ])
      );

      const diffs: Diff[] = [
        {
          path: [{ kind: 'ListIndex', value: 0 }],
          expected: item1,
          actual: emptyRV(),
        },
      ];

      const { container } = renderTableEditor({
        structType: employeeStruct,
        valueDef: arrayValueDef([item1]),
        diffs,
      });

      const expectedOnlyRow = container.querySelector('.expected-only-row');
      expect(expectedOnlyRow).toBeTruthy();

      // Should not have move up/down buttons
      const rowElement = expectedOnlyRow as HTMLElement;
      const moveUpButtons =
        within(rowElement).queryAllByTitle(/move.*previous/i);
      const moveDownButtons = within(rowElement).queryAllByTitle(/move.*next/i);

      expect(moveUpButtons).toHaveLength(0);
      expect(moveDownButtons).toHaveLength(0);
    });
  });

  describe('Mixed scenario: both extra and missing elements', () => {
    it('shows both phantom and expected-only rows in same table', () => {
      // Array: [item1, item2]
      // Diff 1: index 1 is expected-only (item2 expected but not in actual)
      // Diff 2: index 2 is actual-only (extraItem in actual but not expected)
      const item1 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(1)],
          ['salary', intRV(50000)],
        ]),
        'emp-1'
      );
      const item2 = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(2)],
          ['salary', intRV(60000)],
        ]),
        'emp-2'
      );
      const extraItem = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(3)],
          ['salary', intRV(70000)],
        ])
      );

      const diffs: Diff[] = [
        {
          path: [{ kind: 'ListIndex', value: 1 }],
          expected: item2,
          actual: emptyRV(), // Expected-only
        },
        {
          path: [{ kind: 'ListIndex', value: 2 }],
          expected: emptyRV(),
          actual: extraItem, // Actual-only (phantom)
        },
      ];

      const { container } = renderTableEditor({
        structType: employeeStruct,
        valueDef: arrayValueDef([item1, item2]),
        diffs,
      });

      // Should have both types of special rows
      const phantomRows = container.querySelectorAll('.phantom-row');
      const expectedOnlyRows = container.querySelectorAll('.expected-only-row');

      expect(phantomRows).toHaveLength(1);
      expect(expectedOnlyRows).toHaveLength(1);
    });
  });

  describe('Accepting extra elements', () => {
    it('calls onValueChange with new element when Accept is clicked', () => {
      const onValueChange = vi.fn();
      const onDiffResolved = vi.fn();

      const extraItem = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(42)],
          ['salary', intRV(100000)],
        ])
      );

      const diffs: Diff[] = [
        {
          path: [{ kind: 'ListIndex', value: 0 }],
          expected: emptyRV(),
          actual: extraItem,
        },
      ];

      const { container } = renderTableEditor({
        structType: employeeStruct,
        valueDef: arrayValueDef([]), // Empty array, phantom at index 0 = appendable
        diffs,
        onValueChange,
        onDiffResolved,
      });

      const acceptButton = container.querySelector('.phantom-accept');
      expect(acceptButton).toBeTruthy();

      fireEvent.click(acceptButton!);

      // Should have called onValueChange with array containing the new item
      expect(onValueChange).toHaveBeenCalledTimes(1);
      const newValue = onValueChange.mock.calls[0][0];
      expect(newValue.value.kind).toBe('Array');
      expect(newValue.value.value).toHaveLength(1);

      // Should have called onDiffResolved with the path
      expect(onDiffResolved).toHaveBeenCalledWith([
        { kind: 'ListIndex', value: 0 },
      ]);
    });
  });

  describe('Removing expected-only elements', () => {
    it('calls onValueChange to remove element when Remove is clicked', async () => {
      const onValueChange = vi.fn();
      const onDiffResolved = vi.fn();

      const itemToRemove = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(1)],
          ['salary', intRV(50000)],
        ])
      );

      const diffs: Diff[] = [
        {
          path: [{ kind: 'ListIndex', value: 0 }], // Only item, so removable
          expected: itemToRemove,
          actual: emptyRV(),
        },
      ];

      const { container } = renderTableEditor({
        structType: employeeStruct,
        valueDef: arrayValueDef([itemToRemove]),
        diffs,
        onValueChange,
        onDiffResolved,
      });

      const removeButton = container.querySelector('.phantom-remove');
      expect(removeButton).toBeTruthy();

      fireEvent.click(removeButton!);

      // Wait for the async confirm mock to resolve
      await vi.waitFor(() => {
        expect(onValueChange).toHaveBeenCalledTimes(1);
      });

      // Should have called onValueChange with empty array
      const newValue = onValueChange.mock.calls[0][0];
      expect(newValue.value.kind).toBe('Array');
      expect(newValue.value.value).toHaveLength(0);

      // Should have called onDiffResolved with the path
      expect(onDiffResolved).toHaveBeenCalledWith([
        { kind: 'ListIndex', value: 0 },
      ]);
    });
  });

  describe('Empty array with only phantom elements', () => {
    it('does not show "No data" message when phantom rows exist', () => {
      const extraItem = structRV(
        employeeStruct,
        new Map([
          ['id', intRV(1)],
          ['salary', intRV(50000)],
        ])
      );

      const diffs: Diff[] = [
        {
          path: [{ kind: 'ListIndex', value: 0 }],
          expected: emptyRV(),
          actual: extraItem,
        },
      ];

      renderTableEditor({
        structType: employeeStruct,
        valueDef: arrayValueDef([]), // Empty array
        diffs,
      });

      // Should NOT show "No data" message since phantom rows exist
      expect(screen.queryByText('No data')).not.toBeInTheDocument();

      // Should show the table with phantom row
      expect(screen.getByRole('table')).toBeInTheDocument();
    });

    it('shows "No data" message only when no items AND no phantom rows', () => {
      renderTableEditor({
        structType: employeeStruct,
        valueDef: arrayValueDef([]),
        diffs: [], // No diffs
      });

      // Should show "No data" message
      expect(screen.getByText('No data')).toBeInTheDocument();
    });
  });
});
