/**
 * Tests for table view eligibility detection.
 *
 * These tests verify that:
 * 1. structIsFlattenable correctly identifies types that would break table rendering
 * 2. ArrayEditor falls back to tree/card view for structs with problematic types
 */

import { describe, it, expect, vi } from 'vitest';
import { render, screen } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import type {
  Typ,
  StructDeclaration,
  EnumDeclaration,
  RuntimeValue,
  RuntimeValueRaw,
  ValueDef,
  Option,
} from '../../src/generated/test_case';
import { ArrayEditor } from '../../src/editors/ArrayEditor';
import { structIsFlattenable } from '../../src/editors/tableArrayUtils';
import enMessages from '../../src/locales/en.json';

// ============================================================================
// Test Helpers
// ============================================================================

function rv(raw: RuntimeValueRaw): RuntimeValue {
  return { value: raw, attrs: [{ kind: 'Uid', value: crypto.randomUUID() }] };
}

function intVal(n: number): RuntimeValue {
  return rv({ kind: 'Integer', value: n });
}

function arrayVal(items: RuntimeValue[]): RuntimeValue {
  return rv({ kind: 'Array', value: items });
}

function structVal(
  decl: StructDeclaration,
  fields: Map<string, RuntimeValue>
): RuntimeValue {
  return rv({ kind: 'Struct', value: [decl, fields] });
}

function enumVal(
  decl: EnumDeclaration,
  label: string,
  payload?: RuntimeValue
): RuntimeValue {
  return rv({
    kind: 'Enum',
    value: [decl, [label, payload ? { value: payload } : null]],
  });
}

function renderArrayEditor(
  elementType: Typ,
  items: RuntimeValue[]
): ReturnType<typeof render> {
  const valueDef: ValueDef = {
    value: arrayVal(items),
  };

  return render(
    <IntlProvider locale="en" messages={enMessages}>
      <ArrayEditor
        elementType={elementType}
        valueDef={valueDef}
        onValueChange={vi.fn()}
        currentPath={[]}
        diffs={[]}
        editable={true}
      />
    </IntlProvider>
  );
}

// ============================================================================
// Unit Tests: structIsFlattenable
// ============================================================================

describe('structIsFlattenable - detection logic', () => {
  describe('flattenable cases (can use table view)', () => {
    it('returns true for struct with only primitive fields', () => {
      const structDecl: StructDeclaration = {
        struct_name: 'Simple',
        fields: new Map<string, Typ>([
          ['id', { kind: 'TInt' }],
          ['amount', { kind: 'TMoney' }],
          ['date', { kind: 'TDate' }],
          ['active', { kind: 'TBool' }],
        ]),
      };

      expect(structIsFlattenable(structDecl)).toBe(true);
    });

    it('returns true for struct with array field (arrays become sub-tables)', () => {
      const itemDecl: StructDeclaration = {
        struct_name: 'Item',
        fields: new Map<string, Typ>([['value', { kind: 'TInt' }]]),
      };

      const structDecl: StructDeclaration = {
        struct_name: 'Order',
        fields: new Map<string, Typ>([
          ['id', { kind: 'TInt' }],
          [
            'items',
            { kind: 'TArray', value: { kind: 'TStruct', value: itemDecl } },
          ],
        ]),
      };

      expect(structIsFlattenable(structDecl)).toBe(true);
    });

    it('returns true for struct with enum having only simple payloads', () => {
      // Status = Active | Inactive | Pending(date)
      const statusEnum: EnumDeclaration = {
        enum_name: 'Status',
        constructors: new Map<string, Option<Typ>>([
          ['Active', null],
          ['Inactive', null],
          ['Pending', { value: { kind: 'TDate' } }],
        ]),
      };

      const structDecl: StructDeclaration = {
        struct_name: 'Record',
        fields: new Map<string, Typ>([
          ['id', { kind: 'TInt' }],
          ['status', { kind: 'TEnum', value: statusEnum }],
        ]),
      };

      expect(structIsFlattenable(structDecl)).toBe(true);
    });

    it('returns true for nested struct with simple fields', () => {
      const addressDecl: StructDeclaration = {
        struct_name: 'Address',
        fields: new Map<string, Typ>([['zip', { kind: 'TInt' }]]),
      };

      const personDecl: StructDeclaration = {
        struct_name: 'Person',
        fields: new Map<string, Typ>([
          ['id', { kind: 'TInt' }],
          ['address', { kind: 'TStruct', value: addressDecl }],
        ]),
      };

      expect(structIsFlattenable(personDecl)).toBe(true);
    });
  });

  describe('not flattenable (enum with array payload)', () => {
    it('returns false for struct with enum containing array payload', () => {
      // Item { value: int }
      const itemDecl: StructDeclaration = {
        struct_name: 'Item',
        fields: new Map<string, Typ>([['value', { kind: 'TInt' }]]),
      };

      // PaymentMethod = Cash | Installments(payments: Item[])
      const paymentEnum: EnumDeclaration = {
        enum_name: 'PaymentMethod',
        constructors: new Map<string, Option<Typ>>([
          ['Cash', null],
          [
            'Installments',
            {
              value: {
                kind: 'TArray',
                value: { kind: 'TStruct', value: itemDecl },
              },
            },
          ],
        ]),
      };

      const orderDecl: StructDeclaration = {
        struct_name: 'Order',
        fields: new Map<string, Typ>([
          ['id', { kind: 'TInt' }],
          ['payment', { kind: 'TEnum', value: paymentEnum }],
        ]),
      };

      expect(structIsFlattenable(orderDecl)).toBe(false);
    });

    it('returns false for deeply nested enum with array payload', () => {
      // Item { value: int }
      const itemDecl: StructDeclaration = {
        struct_name: 'Item',
        fields: new Map<string, Typ>([['value', { kind: 'TInt' }]]),
      };

      // Status = Active(items: Item[]) | Inactive
      const statusEnum: EnumDeclaration = {
        enum_name: 'Status',
        constructors: new Map<string, Option<Typ>>([
          [
            'Active',
            {
              value: {
                kind: 'TArray',
                value: { kind: 'TStruct', value: itemDecl },
              },
            },
          ],
          ['Inactive', null],
        ]),
      };

      // Inner { status: Status }
      const innerDecl: StructDeclaration = {
        struct_name: 'Inner',
        fields: new Map<string, Typ>([
          ['status', { kind: 'TEnum', value: statusEnum }],
        ]),
      };

      // Outer { data: Inner }
      const outerDecl: StructDeclaration = {
        struct_name: 'Outer',
        fields: new Map<string, Typ>([
          ['id', { kind: 'TInt' }],
          ['data', { kind: 'TStruct', value: innerDecl }],
        ]),
      };

      expect(structIsFlattenable(outerDecl)).toBe(false);
    });
  });

  describe('not flattenable (TOption wrapping array)', () => {
    it('returns false for struct with TOption containing array', () => {
      const itemDecl: StructDeclaration = {
        struct_name: 'Item',
        fields: new Map<string, Typ>([['value', { kind: 'TInt' }]]),
      };

      const structDecl: StructDeclaration = {
        struct_name: 'Record',
        fields: new Map<string, Typ>([
          ['id', { kind: 'TInt' }],
          [
            'optional_items',
            {
              kind: 'TOption',
              value: {
                kind: 'TArray',
                value: { kind: 'TStruct', value: itemDecl },
              },
            },
          ],
        ]),
      };

      expect(structIsFlattenable(structDecl)).toBe(false);
    });
  });

  describe('not flattenable (TTuple containing array)', () => {
    it('returns false for struct with TTuple containing array', () => {
      const itemDecl: StructDeclaration = {
        struct_name: 'Item',
        fields: new Map<string, Typ>([['value', { kind: 'TInt' }]]),
      };

      const structDecl: StructDeclaration = {
        struct_name: 'Record',
        fields: new Map<string, Typ>([
          ['id', { kind: 'TInt' }],
          [
            'pair',
            {
              kind: 'TTuple',
              value: [
                { kind: 'TInt' },
                { kind: 'TArray', value: { kind: 'TStruct', value: itemDecl } },
              ],
            },
          ],
        ]),
      };

      expect(structIsFlattenable(structDecl)).toBe(false);
    });
  });

  describe('not flattenable (TArray<TEnum> with nested payloads)', () => {
    it('returns false for struct with array of enums having struct payloads', () => {
      // Info { code: int, amount: money }
      const infoDecl: StructDeclaration = {
        struct_name: 'Info',
        fields: new Map<string, Typ>([
          ['code', { kind: 'TInt' }],
          ['amount', { kind: 'TMoney' }],
        ]),
      };

      // Event = Started(Info) | Stopped | Error(Info)
      const eventEnum: EnumDeclaration = {
        enum_name: 'Event',
        constructors: new Map<string, Option<Typ>>([
          ['Started', { value: { kind: 'TStruct', value: infoDecl } }],
          ['Stopped', null],
          ['Error', { value: { kind: 'TStruct', value: infoDecl } }],
        ]),
      };

      // Log { timestamp: date, events: Event[] }
      const logDecl: StructDeclaration = {
        struct_name: 'Log',
        fields: new Map<string, Typ>([
          ['timestamp', { kind: 'TDate' }],
          [
            'events',
            { kind: 'TArray', value: { kind: 'TEnum', value: eventEnum } },
          ],
        ]),
      };

      expect(structIsFlattenable(logDecl)).toBe(false);
    });

    it('returns true for struct with array of enums having only simple payloads', () => {
      // Status = Active | Inactive | PendingUntil(date)
      const statusEnum: EnumDeclaration = {
        enum_name: 'Status',
        constructors: new Map<string, Option<Typ>>([
          ['Active', null],
          ['Inactive', null],
          ['PendingUntil', { value: { kind: 'TDate' } }],
        ]),
      };

      // Record { id: int, statuses: Status[] }
      const recordDecl: StructDeclaration = {
        struct_name: 'Record',
        fields: new Map<string, Typ>([
          ['id', { kind: 'TInt' }],
          [
            'statuses',
            { kind: 'TArray', value: { kind: 'TEnum', value: statusEnum } },
          ],
        ]),
      };

      // Simple enum payloads (date) are fine - flattenable
      expect(structIsFlattenable(recordDecl)).toBe(true);
    });
  });
});

// ============================================================================
// Integration Tests: TArray<non-struct> sub-table rendering
// ============================================================================

describe('TableArrayEditor - TArray<enum> sub-table rendering', () => {
  /**
   * Bug reproduction test:
   * When a struct has a TArray<simple enum> field, the table view shows a count badge
   * and "+" button, but no sub-table is rendered. Items added via "+" are invisible.
   *
   * Example from Prime_de_naissance_Calcul.catala_fr:
   * - Personne_PDN has droits_au_séjour: liste de Code_titre_droit_sejour
   * - Code_titre_droit_sejour is a simple enum with just P and D variants
   */
  it('renders sub-table for TArray<simple enum> with visible values', async () => {
    // Code_titre_droit_sejour = P | D (simple enum, no payloads)
    const droitSejourEnum: EnumDeclaration = {
      enum_name: 'Code_titre_droit_sejour',
      constructors: new Map<string, Option<Typ>>([
        ['P', null],
        ['D', null],
      ]),
    };

    // Personne_PDN { id: int, droits_au_séjour: Code_titre_droit_sejour[] }
    const personneDecl: StructDeclaration = {
      struct_name: 'Personne_PDN',
      fields: new Map<string, Typ>([
        ['id', { kind: 'TInt' }],
        [
          'droits_au_séjour',
          { kind: 'TArray', value: { kind: 'TEnum', value: droitSejourEnum } },
        ],
      ]),
    };

    const elementType: Typ = { kind: 'TStruct', value: personneDecl };

    // Create a person with two droits_au_séjour items
    const rows = [
      structVal(
        personneDecl,
        new Map([
          ['id', intVal(1)],
          [
            'droits_au_séjour',
            arrayVal([
              enumVal(droitSejourEnum, 'P'),
              enumVal(droitSejourEnum, 'D'),
            ]),
          ],
        ])
      ),
    ];

    const valueDef: ValueDef = {
      value: arrayVal(rows),
    };

    render(
      <IntlProvider locale="en" messages={enMessages}>
        <ArrayEditor
          elementType={elementType}
          valueDef={valueDef}
          onValueChange={vi.fn()}
          currentPath={[]}
          diffs={[]}
          editable={true}
        />
      </IntlProvider>
    );

    // The main table should exist and use table view
    expect(document.querySelector('.table-view')).toBeInTheDocument();

    // There should be count badges showing 2 items (in main table and/or sub-table header)
    const countBadges = screen.getAllByText('2');
    expect(countBadges.length).toBeGreaterThan(0);

    // CRITICAL: The sub-table should be rendered with editors for the enum values.
    // The sub-table header should exist
    const subTableHeaders = document.querySelectorAll('.sub-table-header');
    expect(subTableHeaders.length).toBeGreaterThan(0);

    // There should be dropdown/select elements to edit the enum values
    const selects = screen.getAllByRole('combobox');
    expect(selects.length).toBeGreaterThanOrEqual(2); // At least 2 enum dropdowns for P and D
  });

  it('renders TArray<primitive> in sub-table', () => {
    // Record { id: int, scores: int[] }
    const recordDecl: StructDeclaration = {
      struct_name: 'Record',
      fields: new Map<string, Typ>([
        ['id', { kind: 'TInt' }],
        ['scores', { kind: 'TArray', value: { kind: 'TInt' } }],
      ]),
    };

    const elementType: Typ = { kind: 'TStruct', value: recordDecl };

    // Create a record with some scores
    const rows = [
      structVal(
        recordDecl,
        new Map([
          ['id', intVal(1)],
          ['scores', arrayVal([intVal(100), intVal(200), intVal(300)])],
        ])
      ),
    ];

    const valueDef: ValueDef = {
      value: arrayVal(rows),
    };

    render(
      <IntlProvider locale="en" messages={enMessages}>
        <ArrayEditor
          elementType={elementType}
          valueDef={valueDef}
          onValueChange={vi.fn()}
          currentPath={[]}
          diffs={[]}
          editable={true}
        />
      </IntlProvider>
    );

    // The main table should use table view
    expect(document.querySelector('.table-view')).toBeInTheDocument();

    // Count badges should show 3 items (in main table and/or sub-table header)
    const countBadges = screen.getAllByText('3');
    expect(countBadges.length).toBeGreaterThan(0);

    // CRITICAL: The sub-table should render the integer values as editable inputs
    expect(screen.getByDisplayValue('100')).toBeInTheDocument();
    expect(screen.getByDisplayValue('200')).toBeInTheDocument();
    expect(screen.getByDisplayValue('300')).toBeInTheDocument();
  });
});

// ============================================================================
// Integration Tests: ArrayEditor fallback behavior
// ============================================================================

describe('ArrayEditor - table view eligibility', () => {
  describe('uses table view for eligible structs', () => {
    it('renders table view for struct with simple fields', () => {
      const structDecl: StructDeclaration = {
        struct_name: 'Simple',
        fields: new Map<string, Typ>([
          ['id', { kind: 'TInt' }],
          ['amount', { kind: 'TMoney' }],
        ]),
      };

      const elementType: Typ = { kind: 'TStruct', value: structDecl };
      const rows = [
        structVal(
          structDecl,
          new Map([
            ['id', intVal(1)],
            ['amount', rv({ kind: 'Money', value: 1000 })],
          ])
        ),
      ];

      renderArrayEditor(elementType, rows);

      // Table view has .table-view class
      expect(document.querySelector('.table-view')).toBeInTheDocument();
      // Card/tree view has .array-editor class without table
      expect(
        document.querySelector('.array-editor > .array-items')
      ).not.toBeInTheDocument();
    });

    it('renders table view for struct with array field (sub-tables)', () => {
      const itemDecl: StructDeclaration = {
        struct_name: 'Item',
        fields: new Map<string, Typ>([['value', { kind: 'TInt' }]]),
      };

      const orderDecl: StructDeclaration = {
        struct_name: 'Order',
        fields: new Map<string, Typ>([
          ['id', { kind: 'TInt' }],
          [
            'items',
            { kind: 'TArray', value: { kind: 'TStruct', value: itemDecl } },
          ],
        ]),
      };

      const elementType: Typ = { kind: 'TStruct', value: orderDecl };
      const rows = [
        structVal(
          orderDecl,
          new Map([
            ['id', intVal(1)],
            [
              'items',
              arrayVal([
                structVal(itemDecl, new Map([['value', intVal(100)]])),
              ]),
            ],
          ])
        ),
      ];

      renderArrayEditor(elementType, rows);

      expect(document.querySelector('.table-view')).toBeInTheDocument();
    });
  });

  describe('falls back to tree/card view for problematic structs', () => {
    it('uses card view for struct with enum containing array payload', () => {
      // Item { value: int }
      const itemDecl: StructDeclaration = {
        struct_name: 'Item',
        fields: new Map<string, Typ>([['value', { kind: 'TInt' }]]),
      };

      // PaymentMethod = Cash | Installments(payments: Item[])
      const paymentEnum: EnumDeclaration = {
        enum_name: 'PaymentMethod',
        constructors: new Map<string, Option<Typ>>([
          ['Cash', null],
          [
            'Installments',
            {
              value: {
                kind: 'TArray',
                value: { kind: 'TStruct', value: itemDecl },
              },
            },
          ],
        ]),
      };

      const orderDecl: StructDeclaration = {
        struct_name: 'Order',
        fields: new Map<string, Typ>([
          ['id', { kind: 'TInt' }],
          ['payment', { kind: 'TEnum', value: paymentEnum }],
        ]),
      };

      const elementType: Typ = { kind: 'TStruct', value: orderDecl };
      const rows = [
        structVal(
          orderDecl,
          new Map([
            ['id', intVal(1)],
            ['payment', enumVal(paymentEnum, 'Cash')],
          ])
        ),
      ];

      renderArrayEditor(elementType, rows);

      // Should NOT use table view
      expect(document.querySelector('.table-view')).not.toBeInTheDocument();
      // Should use array-editor (card/tree view)
      expect(document.querySelector('.array-editor')).toBeInTheDocument();
    });

    it('uses card view for struct with TOption wrapping array', () => {
      const itemDecl: StructDeclaration = {
        struct_name: 'Item',
        fields: new Map<string, Typ>([['value', { kind: 'TInt' }]]),
      };

      const structDecl: StructDeclaration = {
        struct_name: 'Record',
        fields: new Map<string, Typ>([
          ['id', { kind: 'TInt' }],
          [
            'optional_items',
            {
              kind: 'TOption',
              value: {
                kind: 'TArray',
                value: { kind: 'TStruct', value: itemDecl },
              },
            },
          ],
        ]),
      };

      const elementType: Typ = { kind: 'TStruct', value: structDecl };
      const rows = [
        structVal(
          structDecl,
          new Map([
            ['id', intVal(1)],
            // TOption is unimplemented, so we just use Unset for now
            ['optional_items', rv({ kind: 'Unset' })],
          ])
        ),
      ];

      renderArrayEditor(elementType, rows);

      expect(document.querySelector('.table-view')).not.toBeInTheDocument();
      expect(document.querySelector('.array-editor')).toBeInTheDocument();
    });

    it('still renders data correctly in card view', () => {
      // Item { value: int }
      const itemDecl: StructDeclaration = {
        struct_name: 'Item',
        fields: new Map<string, Typ>([['value', { kind: 'TInt' }]]),
      };

      // PaymentMethod = Cash | Installments(payments: Item[])
      const paymentEnum: EnumDeclaration = {
        enum_name: 'PaymentMethod',
        constructors: new Map<string, Option<Typ>>([
          ['Cash', null],
          [
            'Installments',
            {
              value: {
                kind: 'TArray',
                value: { kind: 'TStruct', value: itemDecl },
              },
            },
          ],
        ]),
      };

      const orderDecl: StructDeclaration = {
        struct_name: 'Order',
        fields: new Map<string, Typ>([
          ['id', { kind: 'TInt' }],
          ['payment', { kind: 'TEnum', value: paymentEnum }],
        ]),
      };

      const elementType: Typ = { kind: 'TStruct', value: orderDecl };
      const rows = [
        structVal(
          orderDecl,
          new Map([
            ['id', intVal(42)],
            ['payment', enumVal(paymentEnum, 'Cash')],
          ])
        ),
        structVal(
          orderDecl,
          new Map([
            ['id', intVal(99)],
            [
              'payment',
              enumVal(
                paymentEnum,
                'Installments',
                arrayVal([
                  structVal(itemDecl, new Map([['value', intVal(100)]])),
                  structVal(itemDecl, new Map([['value', intVal(200)]])),
                ])
              ),
            ],
          ])
        ),
      ];

      renderArrayEditor(elementType, rows);

      // Verify data is still rendered
      expect(screen.getByDisplayValue('42')).toBeInTheDocument();
      expect(screen.getByDisplayValue('99')).toBeInTheDocument();
      // Enum values should show in dropdowns
      const selects = screen.getAllByRole('combobox');
      expect(selects.length).toBeGreaterThan(0);
    });

    it('uses card view for struct with TArray<TEnum> having complex payloads', () => {
      // This is the test_enum_array_payloads.catala_en case:
      // Log { timestamp: date, events: Event[] }
      // Event = Started(Info) | Stopped | Error(Info)
      // Info { code: int, amount: money }

      const infoDecl: StructDeclaration = {
        struct_name: 'Info',
        fields: new Map<string, Typ>([
          ['code', { kind: 'TInt' }],
          ['amount', { kind: 'TMoney' }],
        ]),
      };

      const eventEnum: EnumDeclaration = {
        enum_name: 'Event',
        constructors: new Map<string, Option<Typ>>([
          ['Started', { value: { kind: 'TStruct', value: infoDecl } }],
          ['Stopped', null],
          ['Error', { value: { kind: 'TStruct', value: infoDecl } }],
        ]),
      };

      const logDecl: StructDeclaration = {
        struct_name: 'Log',
        fields: new Map<string, Typ>([
          ['timestamp', { kind: 'TDate' }],
          [
            'events',
            { kind: 'TArray', value: { kind: 'TEnum', value: eventEnum } },
          ],
        ]),
      };

      const elementType: Typ = { kind: 'TStruct', value: logDecl };
      const rows = [
        structVal(
          logDecl,
          new Map([
            [
              'timestamp',
              rv({ kind: 'Date', value: { year: 2024, month: 1, day: 15 } }),
            ],
            [
              'events',
              arrayVal([
                enumVal(
                  eventEnum,
                  'Started',
                  structVal(
                    infoDecl,
                    new Map([
                      ['code', intVal(100)],
                      ['amount', rv({ kind: 'Money', value: 5000 })],
                    ])
                  )
                ),
                enumVal(eventEnum, 'Stopped'),
              ]),
            ],
          ])
        ),
      ];

      const { container } = renderArrayEditor(elementType, rows);

      // Top-level should use card/tree view, not table view
      const topLevel = container.firstElementChild!;
      expect(topLevel.classList.contains('array-editor')).toBe(true);
      expect(topLevel.classList.contains('table-view')).toBe(false);
      // Events should be visible and editable (enum dropdowns)
      const selects = screen.getAllByRole('combobox');
      expect(selects.length).toBeGreaterThan(0);
    });
  });
});
