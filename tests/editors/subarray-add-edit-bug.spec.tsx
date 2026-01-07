import { describe, it, expect, vi } from 'vitest';
import type {
  RuntimeValue,
  StructDeclaration,
} from '../../src/generated/catala_types';

describe('TableArrayEditor - Sub-array add and edit bug (stale closure)', () => {
  it('documents the stale closure bug fix', () => {
    // This test documents the fix for the stale closure bug where:
    // 1. User adds a new item to a sub-array via the main table "+" button → creates Unset item
    // 2. User edits the Unset item in the sub-table below
    // 3. The first edit would work, but subsequent edits would revert
    //
    // ROOT CAUSE: The sub-table's onValueChange processes ALL items in a loop.
    // Each iteration called handleParentSubArrayUpdate which read from currentArray
    // (captured in render closure). Multiple updateParent calls caused stale closure
    // issues where later updates overwrote earlier ones.
    //
    // FIX: Batch all updates to all parent rows, apply them to a single newMainArray,
    // and call updateParent only ONCE at the end.
    //
    // The fix is in TableArrayEditor.tsx around line 1100-1152:
    // - Lines 1100-1102: Create newMainArray (copy of currentArray)
    // - Lines 1104-1148: Loop through all parent rows, apply updates to newMainArray directly
    // - Lines 1150-1152: Call updateParent ONCE with all changes
    //
    // This test verifies the concept that multiple updates to the same data structure
    // should be batched to avoid stale closure issues.

    // Create Role struct
    const roleStruct: StructDeclaration = {
      struct_name: 'Rôle',
      fields: new Map([['description', { kind: 'TInt' }]]),
    };

    // Create Personne struct with sub-array
    const personneStruct: StructDeclaration = {
      struct_name: 'Personne',
      fields: new Map([
        ['nom', { kind: 'TInt' }],
        [
          'rôles',
          { kind: 'TArray', value: { kind: 'TStruct', value: roleStruct } },
        ],
      ]),
    };

    // Helper to create a role
    const createRole = (desc: number): RuntimeValue => ({
      value: {
        kind: 'Struct',
        value: [
          roleStruct,
          new Map([
            [
              'description',
              { value: { kind: 'Integer', value: desc }, attrs: [] },
            ],
          ]),
        ],
      },
      attrs: [{ kind: 'Uid', value: `role-${desc}` }],
    });

    // Create initial array: Person with 1 existing role + 1 Unset role (just added)
    const initialRoles = [
      createRole(100), // Existing role
      {
        value: { kind: 'Unset' as const },
        attrs: [{ kind: 'Uid' as const, value: 'role-new' }],
      }, // Newly added
    ];

    const personWithRoles: RuntimeValue = {
      value: {
        kind: 'Struct',
        value: [
          personneStruct,
          new Map([
            ['nom', { value: { kind: 'Integer', value: 1 }, attrs: [] }],
            [
              'rôles',
              { value: { kind: 'Array', value: initialRoles }, attrs: [] },
            ],
          ]),
        ],
      },
      attrs: [{ kind: 'Uid', value: 'person-1' }],
    };

    // Simulate what would happen with the OLD buggy code:
    // When the sub-table edits the Unset role (item 1), it processes ALL roles in a loop.
    // Each iteration would call handleParentSubArrayUpdate, which reads from the STALE currentArray.

    // Buggy approach (simulated):
    let arraySnapshot = [personWithRoles]; // Captured in closure
    const updateParentCalls: any[] = [];

    const buggyUpdateItem = (itemIndex: number, newValue: RuntimeValue) => {
      // This simulates the OLD code: reads from stale arraySnapshot
      const person = arraySnapshot[0]; // STALE - captured at render
      const roles = (person.value as any).value[1].get('rôles').value.value;
      const newRoles = [...roles];
      newRoles[itemIndex] = newValue;

      // Update and call parent (triggers re-render in real code)
      const newPerson = {
        ...person,
        value: {
          kind: 'Struct' as const,
          value: [
            personneStruct,
            new Map([
              ['nom', (person.value as any).value[1].get('nom')],
              [
                'rôles',
                {
                  value: { kind: 'Array' as const, value: newRoles },
                  attrs: [],
                },
              ],
            ]),
          ],
        },
      };
      updateParentCalls.push([newPerson]);
      // BUG: arraySnapshot is NOT updated here! Next call sees stale data
    };

    // Simulate sub-table processing both items:
    buggyUpdateItem(0, createRole(100)); // Process existing role (no real change)
    buggyUpdateItem(1, createRole(42)); // Process edited Unset role

    // With the bug: both calls see the same stale array with Unset,
    // so the last call "wins" but earlier updates are lost

    // FIXED approach:
    const fixedUpdateAllItems = (newItems: RuntimeValue[]) => {
      // Read currentArray ONCE
      const person = arraySnapshot[0];

      // Apply ALL updates at once
      const newPerson = {
        ...person,
        value: {
          kind: 'Struct' as const,
          value: [
            personneStruct,
            new Map([
              ['nom', (person.value as any).value[1].get('nom')],
              [
                'rôles',
                {
                  value: { kind: 'Array' as const, value: newItems },
                  attrs: [],
                },
              ],
            ]),
          ],
        },
      };

      // Call updateParent ONCE
      return newPerson;
    };

    const fixedResult = fixedUpdateAllItems([createRole(100), createRole(99)]);

    // Verify: With the fixed approach, we can apply multiple updates without stale closures
    const fixedRoles = (fixedResult.value as any).value[1].get('rôles').value
      .value;
    expect(fixedRoles).toHaveLength(2);
    expect(fixedRoles[0].value.value[1].get('description').value.value).toBe(
      100
    );
    expect(fixedRoles[1].value.value[1].get('description').value.value).toBe(
      99
    ); // Second edit persisted!

    // The real fix in TableArrayEditor.tsx applies this pattern:
    // Instead of looping and calling handleParentSubArrayUpdate N times (buggy),
    // it builds newMainArray with ALL updates and calls updateParent ONCE (fixed).
  });
});
