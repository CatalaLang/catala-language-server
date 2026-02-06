/**
 * Test for pill navigation highlighting in deeply nested sub-tables.
 *
 * BUG: Clicking a pill (count badge) to navigate to children also highlights
 * grandchildren because querySelectorAll('[data-parent-row="X"]') matches
 * all descendants, including nested sub-tables where rows happen to have
 * the same parent index.
 *
 * Structure: Company > Department[] > Team[] > Member[]
 *
 * When clicking the "departments" pill on Company row 0, only Department rows
 * with data-parent-row="0" should be highlighted, not Team rows that also
 * have data-parent-row="0" (which refers to Department 0, not Company 0).
 */

import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
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
} from '../../src/generated/catala_types';

describe('TableArrayEditor - Pill navigation highlighting', () => {
  beforeEach(() => {
    // Mock scrollIntoView
    Element.prototype.scrollIntoView = vi.fn();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it('should only highlight direct children, not grandchildren (BUG: currently fails)', async () => {
    // Role enum for Member
    const roleEnum: EnumDeclaration = {
      enum_name: 'Role',
      constructors: new Map<string, Typ | null>([
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
    //        └─ Team 101
    //             └─ Member 1003 (Developer)
    //   └─ Department 11
    //        └─ Team 110
    //             └─ Member 1101 (Manager)
    const companies: RuntimeValue[] = [
      createCompany(1, [
        createDept(10, [
          createTeam(100, [
            createMember(1001, 'Manager'),
            createMember(1002, 'Developer'),
          ]),
          createTeam(101, [createMember(1003, 'Developer')]),
        ]),
        createDept(11, [createTeam(110, [createMember(1101, 'Manager')])]),
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

    // Find the departments count badge (pill) for Company row
    // It should show "2" for the 2 departments
    const departmentsPill = screen.getByTitle('Go to departments for row #1');
    expect(departmentsPill).toBeInTheDocument();
    expect(departmentsPill.textContent).toBe('2');

    // Click the pill to navigate to departments sub-table
    fireEvent.click(departmentsPill);

    // Wait for the flash animation to be applied (it has a NAV_DELAY_MS = 100ms delay)
    await waitFor(
      () => {
        const highlighted = document.querySelectorAll('.flash-highlight');
        expect(highlighted.length).toBeGreaterThan(0);
      },
      { timeout: 1000 }
    );

    // Query all elements that received the flash-highlight class
    const flashedElements = document.querySelectorAll('.flash-highlight');
    const flashedRows = Array.from(flashedElements).filter(
      (el) => el.tagName === 'TR' && el.classList.contains('sub-table-row')
    );

    console.log('Total flashed elements:', flashedElements.length);
    console.log('Flashed sub-table rows:', flashedRows.length);
    flashedRows.forEach((row, i) => {
      const parentRow = row.getAttribute('data-parent-row');
      const rowIndex = row.getAttribute('data-row-index');
      // Check which sub-table section this row belongs to
      const subTableSection = row.closest('.sub-table-section');
      const subTableId = subTableSection?.id;
      console.log(
        `Row ${i}: data-parent-row=${parentRow}, data-row-index=${rowIndex}, subTableId=${subTableId}`
      );
    });

    // BUG ASSERTION: Only 2 department rows should be highlighted (children of Company 0)
    // Currently, this also highlights Team rows that have data-parent-row="0"
    // (which means child of Department 0, not Company 0)
    //
    // Expected: 2 rows (Department 10 and Department 11)
    // Actual (buggy): More rows because Team rows with data-parent-row="0" are also matched
    expect(flashedRows.length).toBe(2);
  });
});
