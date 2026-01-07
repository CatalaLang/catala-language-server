import { describe, it, expect } from 'vitest';
import type { RuntimeValue } from '../../src/generated/catala_types';

describe('Sub-table rendering filter bug', () => {
  it('should demonstrate that the badge counts all items but rendering filters some out', () => {
    // Simulate what happens in the UI:
    // 1. computeSubArrays collects ALL items
    // 2. Badge shows count of ALL items
    // 3. But rendering filters out non-struct items with: if (subItem.value.value.kind !== 'Struct') return null;

    const items: RuntimeValue[] = [
      // Person 1: 2 struct roles (will render)
      {
        value: {
          kind: 'Struct',
          value: [{} as any, new Map()],
        },
        attrs: [],
      },
      {
        value: {
          kind: 'Struct',
          value: [{} as any, new Map()],
        },
        attrs: [],
      },
      // Person 2: 2 struct roles (will render)
      {
        value: {
          kind: 'Struct',
          value: [{} as any, new Map()],
        },
        attrs: [],
      },
      {
        value: {
          kind: 'Struct',
          value: [{} as any, new Map()],
        },
        attrs: [],
      },
      // Person 4: 1 struct role (will render)
      {
        value: {
          kind: 'Struct',
          value: [{} as any, new Map()],
        },
        attrs: [],
      },
      // Person 5: 3 NON-STRUCT items (counted but NOT rendered!)
      {
        value: { kind: 'Enum', value: ['CJT', null] },
        attrs: [],
      },
      {
        value: { kind: 'Enum', value: ['ENF', null] },
        attrs: [],
      },
      {
        value: { kind: 'Enum', value: ['PAC', null] },
        attrs: [],
      },
    ];

    // Badge count: all items
    const badgeCount = items.length;
    expect(badgeCount).toBe(8);

    // Rendered rows: only struct items (simulating the filter in rendering code)
    const renderedItems = items.filter((item) => item.value.kind === 'Struct');
    const renderedCount = renderedItems.length;
    expect(renderedCount).toBe(5);

    // BUG: Badge shows 8, but only 5 rows render!
    expect(badgeCount).not.toBe(renderedCount);
    expect(badgeCount).toBe(8);
    expect(renderedCount).toBe(5);

    console.log('Badge shows:', badgeCount);
    console.log('Rows rendered:', renderedCount);
    console.log('Missing rows:', badgeCount - renderedCount);
  });
});
