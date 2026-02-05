/**
 * Tests for complex/nested data structures in TableArrayEditor.
 *
 * These tests verify:
 * - Supported types render correctly (all leaf values via proper editors, no JSON)
 * - Unsupported types are correctly rejected by tryCreateTableSchema
 * - Deeply nested but supported structures work in table view
 *
 * Note: Catala's runtime doesn't have a String type - we use Integer/Date/Money for leaf values.
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
import { TableArrayEditor } from '../../src/editors/TableArrayEditor';
import { tryCreateTableSchema } from '../../src/editors/tableArrayUtils';
import enMessages from '../../src/locales/en.json';

// ============================================================================
// Test Helpers
// ============================================================================

function rv(raw: RuntimeValueRaw): RuntimeValue {
  return { value: raw, attrs: [] };
}

function intVal(n: number): RuntimeValue {
  return rv({ kind: 'Integer', value: n });
}

function moneyVal(cents: number): RuntimeValue {
  return rv({ kind: 'Money', value: cents });
}

function dateVal(year: number, month: number, day: number): RuntimeValue {
  return rv({ kind: 'Date', value: { year, month, day } });
}

function boolVal(b: boolean): RuntimeValue {
  return rv({ kind: 'Bool', value: b });
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

function renderTableArrayEditor(
  structType: StructDeclaration,
  rows: RuntimeValue[],
  editable = true
): ReturnType<typeof render> {
  const elementType: Typ = { kind: 'TStruct', value: structType };
  const schemaResult = tryCreateTableSchema(elementType);
  if (!schemaResult.ok) {
    throw new Error(
      `Test setup error: struct "${structType.struct_name}" is not flattenable. ` +
        `Reasons: ${schemaResult.reasons.map((r) => `${r.field}: ${r.reason}`).join('; ')}`
    );
  }

  const valueDef: ValueDef = {
    value: arrayVal(rows),
  };

  return render(
    <IntlProvider locale="en" messages={enMessages}>
      <TableArrayEditor
        elementType={elementType}
        schema={schemaResult.schema}
        valueDef={valueDef}
        onValueChange={vi.fn()}
        currentPath={[]}
        diffs={[]}
        editable={editable}
      />
    </IntlProvider>
  );
}

// ============================================================================
// Tier 1: High-Risk Structures (Most Likely to Break)
// ============================================================================

describe('Tier 1: Array within enum payload', () => {
  // Item { value: int }
  const itemStructDecl: StructDeclaration = {
    struct_name: 'Item',
    fields: new Map<string, Typ>([['value', { kind: 'TInt' }]]),
  };

  // Status = Pending | Active(items: Item[])
  const statusEnumDecl: EnumDeclaration = {
    enum_name: 'Status',
    constructors: new Map<string, Option<Typ>>([
      ['Pending', null],
      [
        'Active',
        {
          value: {
            kind: 'TArray',
            value: { kind: 'TStruct', value: itemStructDecl },
          },
        },
      ],
    ]),
  };

  // Record { id: int, status: Status }
  const recordStructDecl: StructDeclaration = {
    struct_name: 'Record',
    fields: new Map<string, Typ>([
      ['id', { kind: 'TInt' }],
      ['status', { kind: 'TEnum', value: statusEnumDecl }],
    ]),
  };

  const createRecord = (
    id: number,
    status: 'Pending' | 'Active',
    itemValues?: number[]
  ): RuntimeValue => {
    const statusValue =
      status === 'Pending'
        ? enumVal(statusEnumDecl, 'Pending')
        : enumVal(
            statusEnumDecl,
            'Active',
            arrayVal(
              (itemValues ?? []).map((v) =>
                structVal(itemStructDecl, new Map([['value', intVal(v)]]))
              )
            )
          );

    return structVal(
      recordStructDecl,
      new Map([
        ['id', intVal(id)],
        ['status', statusValue],
      ])
    );
  };

  it('is correctly rejected by tryCreateTableSchema', () => {
    // Status = Pending | Active(items: Item[]) has an array payload,
    // so it's correctly classified as 'unsupported' by tryCreateTableSchema().
    // This struct should fall back to card/tree view.
    const elementType: Typ = { kind: 'TStruct', value: recordStructDecl };
    const result = tryCreateTableSchema(elementType);

    expect(result.ok).toBe(false);
    if (!result.ok) {
      expect(result.reasons).toHaveLength(1);
      expect(result.reasons[0].field).toBe('status');
      expect(result.reasons[0].reason).toContain('contains arrays in payloads');
    }
  });

  it('throws when renderTableArrayEditor is called with unsupported type', () => {
    // This verifies that directly calling TableArrayEditor with unsupported
    // types is an error. In production, ArrayEditor validates types first.
    const rows = [createRecord(1, 'Active', [42])];

    expect(() => renderTableArrayEditor(recordStructDecl, rows)).toThrow(
      /not flattenable/
    );
  });
});

describe('Tier 1: Sibling arrays in same struct', () => {
  // Child { age: int }
  const childStructDecl: StructDeclaration = {
    struct_name: 'Child',
    fields: new Map<string, Typ>([['age', { kind: 'TInt' }]]),
  };

  // Pet { weight: int }
  const petStructDecl: StructDeclaration = {
    struct_name: 'Pet',
    fields: new Map<string, Typ>([['weight', { kind: 'TInt' }]]),
  };

  // Person { code: int, children: Child[], pets: Pet[] }
  const personStructDecl: StructDeclaration = {
    struct_name: 'Person',
    fields: new Map<string, Typ>([
      ['code', { kind: 'TInt' }],
      [
        'children',
        { kind: 'TArray', value: { kind: 'TStruct', value: childStructDecl } },
      ],
      [
        'pets',
        { kind: 'TArray', value: { kind: 'TStruct', value: petStructDecl } },
      ],
    ]),
  };

  const createChild = (age: number): RuntimeValue =>
    structVal(childStructDecl, new Map([['age', intVal(age)]]));

  const createPet = (weight: number): RuntimeValue =>
    structVal(petStructDecl, new Map([['weight', intVal(weight)]]));

  const createPerson = (
    code: number,
    childAges: number[],
    petWeights: number[]
  ): RuntimeValue =>
    structVal(
      personStructDecl,
      new Map([
        ['code', intVal(code)],
        ['children', arrayVal(childAges.map(createChild))],
        ['pets', arrayVal(petWeights.map(createPet))],
      ])
    );

  it('renders without throwing', () => {
    const rows = [createPerson(1, [10, 8], [20, 15]), createPerson(2, [5], [])];

    expect(() => renderTableArrayEditor(personStructDecl, rows)).not.toThrow();
  });

  it('renders both array sub-table headers', () => {
    const rows = [createPerson(1, [10], [5])];

    renderTableArrayEditor(personStructDecl, rows);

    // Use getAllByText since the name appears in both column header and sub-table header
    expect(screen.getAllByText('children').length).toBeGreaterThanOrEqual(1);
    expect(screen.getAllByText('pets').length).toBeGreaterThanOrEqual(1);
  });

  it('renders values from both arrays', () => {
    const rows = [createPerson(42, [7], [99])];

    renderTableArrayEditor(personStructDecl, rows);

    expect(screen.getByDisplayValue('42')).toBeInTheDocument();
    expect(screen.getByDisplayValue('7')).toBeInTheDocument();
    expect(screen.getByDisplayValue('99')).toBeInTheDocument();
  });
});

describe('Tier 1: Array of enums with struct payloads', () => {
  // Info { code: int, amount: money }
  const infoStructDecl: StructDeclaration = {
    struct_name: 'Info',
    fields: new Map<string, Typ>([
      ['code', { kind: 'TInt' }],
      ['amount', { kind: 'TMoney' }],
    ]),
  };

  // Event = Started(info: Info) | Stopped | Error(info: Info)
  const eventEnumDecl: EnumDeclaration = {
    enum_name: 'Event',
    constructors: new Map<string, Option<Typ>>([
      ['Started', { value: { kind: 'TStruct', value: infoStructDecl } }],
      ['Stopped', null],
      ['Error', { value: { kind: 'TStruct', value: infoStructDecl } }],
    ]),
  };

  // Log { timestamp: date, events: Event[] }
  const logStructDecl: StructDeclaration = {
    struct_name: 'Log',
    fields: new Map<string, Typ>([
      ['timestamp', { kind: 'TDate' }],
      [
        'events',
        { kind: 'TArray', value: { kind: 'TEnum', value: eventEnumDecl } },
      ],
    ]),
  };

  const createInfo = (code: number, amountCents: number): RuntimeValue =>
    structVal(
      infoStructDecl,
      new Map([
        ['code', intVal(code)],
        ['amount', moneyVal(amountCents)],
      ])
    );

  const createLog = (
    year: number,
    month: number,
    day: number,
    events: RuntimeValue[]
  ): RuntimeValue =>
    structVal(
      logStructDecl,
      new Map([
        ['timestamp', dateVal(year, month, day)],
        ['events', arrayVal(events)],
      ])
    );

  it('is correctly rejected by tryCreateTableSchema', () => {
    // TArray<Enum with struct payloads> is correctly classified as 'unsupported'
    // by tryCreateTableSchema(). This struct should fall back to card/tree view.
    // (The actual fallback is tested in table-view-eligibility.spec.tsx)
    const elementType: Typ = { kind: 'TStruct', value: logStructDecl };
    const result = tryCreateTableSchema(elementType);

    expect(result.ok).toBe(false);
    if (!result.ok) {
      expect(result.reasons).toHaveLength(1);
      expect(result.reasons[0].field).toBe('events');
      expect(result.reasons[0].reason).toContain('complex payloads');
    }
  });

  it('throws when renderTableArrayEditor is called with unsupported type', () => {
    // This verifies that directly calling TableArrayEditor with unsupported
    // types is an error. In production, ArrayEditor validates types first
    // and falls back to card view for unsupported types.
    const rows = [
      createLog(2024, 6, 1, [
        enumVal(eventEnumDecl, 'Started', createInfo(1, 100)),
        enumVal(eventEnumDecl, 'Stopped'),
      ]),
    ];

    expect(() => renderTableArrayEditor(logStructDecl, rows)).toThrow(
      /not flattenable/
    );
  });
});

// ============================================================================
// Tier 2: Edge Cases
// ============================================================================

describe('Tier 2: Deeply nested structure (4 levels)', () => {
  // Role = Manager | Developer | Intern
  const roleEnumDecl: EnumDeclaration = {
    enum_name: 'Role',
    constructors: new Map<string, Option<Typ>>([
      ['Manager', null],
      ['Developer', null],
      ['Intern', null],
    ]),
  };

  // Member { id: int, role: Role }
  const memberStructDecl: StructDeclaration = {
    struct_name: 'Member',
    fields: new Map<string, Typ>([
      ['id', { kind: 'TInt' }],
      ['role', { kind: 'TEnum', value: roleEnumDecl }],
    ]),
  };

  // Team { team_id: int, members: Member[] }
  const teamStructDecl: StructDeclaration = {
    struct_name: 'Team',
    fields: new Map<string, Typ>([
      ['team_id', { kind: 'TInt' }],
      [
        'members',
        { kind: 'TArray', value: { kind: 'TStruct', value: memberStructDecl } },
      ],
    ]),
  };

  // Department { dept_id: int, teams: Team[] }
  const deptStructDecl: StructDeclaration = {
    struct_name: 'Department',
    fields: new Map<string, Typ>([
      ['dept_id', { kind: 'TInt' }],
      [
        'teams',
        { kind: 'TArray', value: { kind: 'TStruct', value: teamStructDecl } },
      ],
    ]),
  };

  // Company { company_id: int, departments: Department[] }
  const companyStructDecl: StructDeclaration = {
    struct_name: 'Company',
    fields: new Map<string, Typ>([
      ['company_id', { kind: 'TInt' }],
      [
        'departments',
        { kind: 'TArray', value: { kind: 'TStruct', value: deptStructDecl } },
      ],
    ]),
  };

  const createMember = (id: number, role: string): RuntimeValue =>
    structVal(
      memberStructDecl,
      new Map([
        ['id', intVal(id)],
        ['role', enumVal(roleEnumDecl, role)],
      ])
    );

  const createTeam = (teamId: number, members: RuntimeValue[]): RuntimeValue =>
    structVal(
      teamStructDecl,
      new Map([
        ['team_id', intVal(teamId)],
        ['members', arrayVal(members)],
      ])
    );

  const createDept = (deptId: number, teams: RuntimeValue[]): RuntimeValue =>
    structVal(
      deptStructDecl,
      new Map([
        ['dept_id', intVal(deptId)],
        ['teams', arrayVal(teams)],
      ])
    );

  const createCompany = (
    companyId: number,
    depts: RuntimeValue[]
  ): RuntimeValue =>
    structVal(
      companyStructDecl,
      new Map([
        ['company_id', intVal(companyId)],
        ['departments', arrayVal(depts)],
      ])
    );

  it('renders without throwing', () => {
    const rows = [
      createCompany(1, [
        createDept(10, [
          createTeam(100, [
            createMember(1001, 'Developer'),
            createMember(1002, 'Intern'),
          ]),
          createTeam(101, [createMember(1003, 'Manager')]),
        ]),
        createDept(11, [createTeam(110, [createMember(1004, 'Manager')])]),
      ]),
    ];

    expect(() => renderTableArrayEditor(companyStructDecl, rows)).not.toThrow();
  });

  it('renders company ID at top level', () => {
    const rows = [
      createCompany(999, [
        createDept(1, [createTeam(1, [createMember(1, 'Developer')])]),
      ]),
    ];

    renderTableArrayEditor(companyStructDecl, rows);

    expect(screen.getByDisplayValue('999')).toBeInTheDocument();
  });
});

describe('Tier 2: Mixed primitive and complex fields', () => {
  // Tag { score: int }
  const tagStructDecl: StructDeclaration = {
    struct_name: 'Tag',
    fields: new Map<string, Typ>([['score', { kind: 'TInt' }]]),
  };

  // Priority = Low | Medium | High(deadline: date)
  const priorityEnumDecl: EnumDeclaration = {
    enum_name: 'Priority',
    constructors: new Map<string, Option<Typ>>([
      ['Low', null],
      ['Medium', null],
      ['High', { value: { kind: 'TDate' } }],
    ]),
  };

  // Metadata { version: int, active: bool }
  const metadataStructDecl: StructDeclaration = {
    struct_name: 'Metadata',
    fields: new Map<string, Typ>([
      ['version', { kind: 'TInt' }],
      ['active', { kind: 'TBool' }],
    ]),
  };

  // Task { id: int, budget: money, priority: Priority, tags: Tag[], metadata: Metadata }
  const taskStructDecl: StructDeclaration = {
    struct_name: 'Task',
    fields: new Map<string, Typ>([
      ['id', { kind: 'TInt' }],
      ['budget', { kind: 'TMoney' }],
      ['priority', { kind: 'TEnum', value: priorityEnumDecl }],
      [
        'tags',
        { kind: 'TArray', value: { kind: 'TStruct', value: tagStructDecl } },
      ],
      ['metadata', { kind: 'TStruct', value: metadataStructDecl }],
    ]),
  };

  const createTag = (score: number): RuntimeValue =>
    structVal(tagStructDecl, new Map([['score', intVal(score)]]));

  const createMetadata = (version: number, active: boolean): RuntimeValue =>
    structVal(
      metadataStructDecl,
      new Map([
        ['version', intVal(version)],
        ['active', boolVal(active)],
      ])
    );

  const createTask = (
    id: number,
    budgetCents: number,
    priority: RuntimeValue,
    tagScores: number[],
    metadata: RuntimeValue
  ): RuntimeValue =>
    structVal(
      taskStructDecl,
      new Map([
        ['id', intVal(id)],
        ['budget', moneyVal(budgetCents)],
        ['priority', priority],
        ['tags', arrayVal(tagScores.map(createTag))],
        ['metadata', metadata],
      ])
    );

  it('renders without throwing', () => {
    const rows = [
      createTask(
        1,
        100000,
        enumVal(priorityEnumDecl, 'High', dateVal(2024, 12, 31)),
        [90, 85, 95],
        createMetadata(3, true)
      ),
      createTask(
        2,
        50000,
        enumVal(priorityEnumDecl, 'Low'),
        [50],
        createMetadata(1, false)
      ),
    ];

    expect(() => renderTableArrayEditor(taskStructDecl, rows)).not.toThrow();
  });

  it('renders primitive fields in main table', () => {
    const rows = [
      createTask(
        42,
        12345,
        enumVal(priorityEnumDecl, 'Medium'),
        [],
        createMetadata(1, true)
      ),
    ];

    renderTableArrayEditor(taskStructDecl, rows);

    expect(screen.getByDisplayValue('42')).toBeInTheDocument();
    // Money is displayed as dollars (12345 cents = 123.45)
    expect(screen.getByDisplayValue('123.45')).toBeInTheDocument();
  });

  it('renders nested struct fields', () => {
    const rows = [
      createTask(
        1,
        0,
        enumVal(priorityEnumDecl, 'Low'),
        [],
        createMetadata(99, true)
      ),
    ];

    renderTableArrayEditor(taskStructDecl, rows);

    // Nested metadata.version should be rendered
    expect(screen.getByDisplayValue('99')).toBeInTheDocument();
  });
});
