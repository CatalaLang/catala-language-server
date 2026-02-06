/**
 * Test for back arrow navigation in deeply nested sub-tables.
 *
 * BUG: Back arrows in nested sub-tables navigate to the top-level main table
 * instead of the immediate parent sub-table.
 *
 * Structure: Company > Department[] > Team[] > Member[]
 *
 * When clicking back arrow on a Member, it should navigate to the parent Team,
 * not to the top-level Company.
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import { TableArrayEditor } from '../../src/editors/TableArrayEditor';
import { tryCreateTableSchema } from '../../src/editors/tableArrayUtils';
import enMessages from '../../src/locales/en.json';
import type {
  RuntimeValue,
  StructDeclaration,
  EnumDeclaration,
  Typ,
  ValueDef,
  Option,
} from '../../src/generated/catala_types';

describe('TableArrayEditor - Sub-table back navigation', () => {
  let scrollIntoViewMock: ReturnType<typeof vi.fn>;
  let scrolledElements: Element[] = [];

  beforeEach(() => {
    scrolledElements = [];
    scrollIntoViewMock = vi.fn(function (this: Element) {
      scrolledElements.push(this);
    });
    Element.prototype.scrollIntoView = scrollIntoViewMock;
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it('should navigate to immediate parent, not top-level table (BUG: currently fails)', () => {
    // Role enum for Member
    const roleEnum: EnumDeclaration = {
      enum_name: 'Role',
      constructors: new Map<string, Option<Typ>>([
        ['Manager', null],
        ['Developer', null],
      ]),
    };

    // Member { id: int, role: Role }
    const memberStruct: StructDeclaration = {
      struct_name: 'Member',
      fields: new Map<string, Typ>([
        ['id', { kind: 'TInt' }],
        ['role', { kind: 'TEnum', value: roleEnum }],
      ]),
    };

    // Team { team_id: int, members: Member[] }
    const teamStruct: StructDeclaration = {
      struct_name: 'Team',
      fields: new Map<string, Typ>([
        ['team_id', { kind: 'TInt' }],
        [
          'members',
          { kind: 'TArray', value: { kind: 'TStruct', value: memberStruct } },
        ],
      ]),
    };

    // Department { dept_id: int, teams: Team[] }
    const deptStruct: StructDeclaration = {
      struct_name: 'Department',
      fields: new Map<string, Typ>([
        ['dept_id', { kind: 'TInt' }],
        [
          'teams',
          { kind: 'TArray', value: { kind: 'TStruct', value: teamStruct } },
        ],
      ]),
    };

    // Company { company_id: int, departments: Department[] }
    const companyStruct: StructDeclaration = {
      struct_name: 'Company',
      fields: new Map<string, Typ>([
        ['company_id', { kind: 'TInt' }],
        [
          'departments',
          { kind: 'TArray', value: { kind: 'TStruct', value: deptStruct } },
        ],
      ]),
    };

    const companyType: Typ = { kind: 'TStruct', value: companyStruct };

    // Helpers
    const createMember = (id: number, role: string): RuntimeValue => ({
      value: {
        kind: 'Struct',
        value: [
          memberStruct,
          new Map<string, RuntimeValue>([
            ['id', { value: { kind: 'Integer', value: id }, attrs: [] }],
            [
              'role',
              {
                value: { kind: 'Enum', value: [roleEnum, [role, null]] },
                attrs: [],
              },
            ],
          ]),
        ],
      },
      attrs: [{ kind: 'Uid', value: `member-${id}` }],
    });

    const createTeam = (
      teamId: number,
      members: RuntimeValue[]
    ): RuntimeValue => ({
      value: {
        kind: 'Struct',
        value: [
          teamStruct,
          new Map<string, RuntimeValue>([
            [
              'team_id',
              { value: { kind: 'Integer', value: teamId }, attrs: [] },
            ],
            [
              'members',
              { value: { kind: 'Array', value: members }, attrs: [] },
            ],
          ]),
        ],
      },
      attrs: [{ kind: 'Uid', value: `team-${teamId}` }],
    });

    const createDept = (
      deptId: number,
      teams: RuntimeValue[]
    ): RuntimeValue => ({
      value: {
        kind: 'Struct',
        value: [
          deptStruct,
          new Map<string, RuntimeValue>([
            [
              'dept_id',
              { value: { kind: 'Integer', value: deptId }, attrs: [] },
            ],
            ['teams', { value: { kind: 'Array', value: teams }, attrs: [] }],
          ]),
        ],
      },
      attrs: [{ kind: 'Uid', value: `dept-${deptId}` }],
    });

    const createCompany = (
      companyId: number,
      depts: RuntimeValue[]
    ): RuntimeValue => ({
      value: {
        kind: 'Struct',
        value: [
          companyStruct,
          new Map<string, RuntimeValue>([
            [
              'company_id',
              { value: { kind: 'Integer', value: companyId }, attrs: [] },
            ],
            [
              'departments',
              { value: { kind: 'Array', value: depts }, attrs: [] },
            ],
          ]),
        ],
      },
      attrs: [
        { kind: 'Uid', value: `company-${companyId}` },
        { kind: 'ArrayItemLabel', value: `Company ${companyId}` },
      ],
    });

    // Create structure:
    // Company 1
    //   └─ Department 10
    //        └─ Team 100
    //             └─ Member 1001 (Manager)
    //             └─ Member 1002 (Developer)
    const companies: RuntimeValue[] = [
      createCompany(1, [
        createDept(10, [
          createTeam(100, [
            createMember(1001, 'Manager'),
            createMember(1002, 'Developer'),
          ]),
        ]),
      ]),
    ];

    const valueDef: ValueDef = {
      value: {
        value: { kind: 'Array', value: companies },
        attrs: [],
      },
    };

    const schemaResult = tryCreateTableSchema(companyType);
    if (!schemaResult.ok) throw new Error('Expected schema to be ok');

    render(
      <IntlProvider locale="en" messages={enMessages}>
        <TableArrayEditor
          elementType={companyType}
          schema={schemaResult.schema}
          valueDef={valueDef}
          onValueChange={() => {}}
          currentPath={[]}
          diffs={[]}
          editable={true}
        />
      </IntlProvider>
    );

    // Find back arrow elements in sub-tables
    // Back arrows have title "Go to parent: {parentLabel}" and class "parent-nav-link"
    const backArrows = document.querySelectorAll('.parent-nav-link');

    // We should have back arrows in:
    // - departments sub-table (1 dept)
    // - teams sub-table (1 team)
    // - members sub-table (2 members)
    // Total: 4 back arrows
    expect(backArrows.length).toBe(4);

    // The last 2 back arrows should be in the members sub-table
    // Click the back arrow on the first member
    const memberBackArrow = backArrows[2] as HTMLElement; // First member's back arrow

    // Before clicking, verify the row structure
    const memberRow = memberBackArrow.closest('tr');
    expect(memberRow).toBeInTheDocument();
    expect(memberRow?.classList.contains('sub-table-row')).toBe(true);

    // Click the back arrow
    fireEvent.click(memberBackArrow);

    // Verify scrollIntoView was called
    expect(scrollIntoViewMock).toHaveBeenCalled();

    // Get the element that was scrolled to
    const scrolledElement = scrolledElements[0];
    expect(scrolledElement).toBeDefined();

    // BUG ASSERTION: The scrolled element should be in the "teams" sub-table
    // (the immediate parent of members), NOT in the main "companies" table.
    //
    // Currently, the bug causes it to scroll to the main table row because:
    // 1. Sub-table rows have no ID (id={isSubTable ? undefined : ...})
    // 2. onNavigateToParent uses getElementById('parent-row-0')
    // 3. This finds the main table's row, not the teams sub-table row

    // The scrolled element should be a row in the teams sub-table
    // Teams sub-table rows should NOT have id="parent-row-*" (that's main table only)
    // But they SHOULD be the parent of members

    // Check: the scrolled element should NOT be the main table Company row
    const scrolledId = scrolledElement?.id;
    console.log('Scrolled to element with id:', scrolledId);
    console.log('Scrolled element classes:', scrolledElement?.className);

    // The bug: currently scrolls to 'parent-row-0' which is the Company row
    // Expected: should scroll to the Team row (which currently has no ID)

    // This assertion should FAIL with the current buggy implementation
    // because it scrolls to the Company row (id="parent-row-0") instead of Team row
    expect(scrolledId).not.toBe('parent-row-0');
  });
});
