/**
 * The two-pane view shown when a test no longer fits its scope. The left pane
 * must be inert and complete: it is the only copy of what the tester wrote.
 */
import React from 'react';
import { describe, it, expect } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import type {
  CarryRecord,
  Recovery,
  StructDeclaration,
  Test,
  TestIo,
  TestList,
  Typ,
} from '../../src/generated/catala_types';
import type { TestRunResults } from '../../src/generated/catala_types';
import type { TestRunStatus } from '../../src/test-case-editor/TestFileEditor';
import BrokenTestView from '../../src/test-case-editor/BrokenTestView';
import {
  arrayVal,
  dateVal,
  intVal,
  moneyVal,
  rv,
  structVal,
} from './test-helpers';
import enMessages from '../../src/locales/en.json';

const endDateEnum: Typ = {
  kind: 'TEnum',
  value: {
    enum_name: 'B.EndDate',
    // Constructor payloads are Option<Typ>: wrapped, or null for no payload.
    constructors: new Map<string, { value: Typ } | null>([
      ['Fixed', { value: { kind: 'TDate' } }],
      ['OpenEnded', null],
    ]),
    ctor_attrs: new Map(),
  },
};

function io(typ: Typ, value?: TestIo['value']): TestIo {
  return { typ, value };
}

function authored(): Test {
  return {
    testing_scope: 'C_one',
    tested_scope: {
      module_name: 'B',
      name: 'C',
      inputs: new Map(),
      outputs: new Map(),
    },
    // Types inferred from the literals: end_date reads as a plain date.
    test_inputs: new Map<string, TestIo>([
      ['start_date', io({ kind: 'TDate' }, { value: dateVal(2025, 1, 1) })],
      ['end_date', io({ kind: 'TDate' }, { value: dateVal(2999, 12, 31) })],
    ]),
    test_outputs: new Map(),
    description: '',
    title: 'one',
  };
}

/** What the extension's prefill produces: same type carried, changed type not. */
function rebuilt(): Test {
  return {
    ...authored(),
    test_inputs: new Map<string, TestIo>([
      ['start_date', io({ kind: 'TDate' }, { value: dateVal(2025, 1, 1) })],
      ['end_date', io(endDateEnum)],
    ]),
  };
}

function view(
  overrides: {
    /** No rebuild to offer: the pair carries only the authored test. */
    blocked?: boolean;
    outcomes?: CarryRecord[];
    notes?: Recovery['notes'];
  } = {}
): Recovery {
  return {
    tests: [
      {
        authored: authored(),
        rebuilt: overrides.blocked ? undefined : rebuilt(),
        outcomes:
          overrides.outcomes ??
          (overrides.blocked
            ? []
            : [
                {
                  path: [{ kind: 'StructField', value: 'start_date' }],
                  side: { kind: 'In' },
                  outcome: { kind: 'Fits' },
                },
                {
                  path: [{ kind: 'StructField', value: 'end_date' }],
                  side: { kind: 'In' },
                  outcome: {
                    kind: 'TypeChanged',
                    value: [{ kind: 'TDate' }, endDateEnum],
                  },
                },
              ]),
      },
    ],
    notes: overrides.notes ?? [],
    working_copy: 'test_one.catala_en.repair',
  };
}

function renderView(
  v: Recovery,
  runStates?: Record<
    string,
    { status: TestRunStatus; results?: TestRunResults }
  >
): HTMLElement {
  const { container } = render(
    <IntlProvider locale="en" messages={enMessages}>
      <BrokenTestView view={v} runStates={runStates} onRun={(): void => {}} />
    </IntlProvider>
  );
  return container;
}

describe('BrokenTestView', () => {
  it('shows the authored values in both panes rather than a compiler error', () => {
    renderView(view());
    expect(screen.getByText('one')).toBeTruthy();
    expect(screen.getByText('Original')).toBeTruthy();
    expect(screen.getByText('Working copy')).toBeTruthy();
    // The sentinel that would otherwise have been overwritten.
    expect(screen.getAllByDisplayValue('2999-12-31').length).toBeGreaterThan(0);
  });

  it('leaves a field whose type changed empty, and says why', () => {
    const container = renderView(view());
    // start_date survives on both sides; end_date only on the authored one.
    expect(screen.getAllByDisplayValue('2025-01-01').length).toBe(2);
    expect(screen.getAllByDisplayValue('2999-12-31').length).toBe(1);
    const dot = container.querySelector('.broken-pane-rebuilt .fate-attention');
    expect(dot).toBeTruthy();
    // Localized names, not raw catala type syntax -- in the tooltip.
    expect(dot!.getAttribute('title')).toMatch(/date → EndDate/);
    expect(screen.queryByText(/date → EndDate/)).toBeNull();
  });

  it('renders the authored pane with the ordinary editors, inert', () => {
    const container = renderView(view());
    const pane = container.querySelector('.broken-pane-authored')!;
    // Same layout as the right pane...
    expect(pane.querySelector('.composite-editor')).toBeTruthy();
    // ...but nothing editable and nothing to click except tabs.
    expect(pane.querySelectorAll('input:not([disabled])').length).toBe(0);
    expect(pane.querySelectorAll('button:not(.tab)').length).toBe(0);
  });

  it('keeps the authored pane quiet except for what promotion deletes', () => {
    const container = renderView(view());
    const pane = container.querySelector('.broken-pane-authored')!;
    // Carried and even not-carried fields say nothing on the left: the right
    // pane owns those signals. Only Dropped has no other place to be said.
    expect(pane.querySelectorAll('.fate-mark').length).toBe(0);
  });

  it('names the new target only when the rebuild points elsewhere', () => {
    const still = renderView(view());
    expect(still.querySelector('.broken-retargeted')).toBeNull();
    const v = view();
    v.tests[0].rebuilt!.tested_scope = {
      ...v.tests[0].rebuilt!.tested_scope,
      name: 'ComputeBenefit',
    };
    const moved = renderView(v);
    const line = moved.querySelector('.broken-retargeted');
    expect(line).toBeTruthy();
    expect(line!.textContent).toContain('B.ComputeBenefit');
  });

  it('counts a blank record as the values its declaration asks for', () => {
    const v = view();
    const period: Typ = {
      kind: 'TStruct',
      value: {
        struct_name: 'B.Period',
        fields: new Map<string, Typ>([
          ['first_day', { kind: 'TDate' }],
          ['last_day', { kind: 'TDate' }],
        ]),
        field_attrs: new Map(),
      },
    };
    v.tests[0].rebuilt!.test_inputs = new Map<string, TestIo>([
      ['period', io(period, { value: rv({ kind: 'Unset' }) })],
    ]);
    const container = renderView(v);
    expect(
      container.querySelector('.readiness-unfilled')!.textContent
    ).toContain('2 values to fill');
  });

  it('counts the unfilled fields and offers to jump to the first', () => {
    const v = view();
    v.tests[0].rebuilt!.test_inputs = new Map<string, TestIo>([
      ['end_date', io(endDateEnum, { value: rv({ kind: 'Unset' }) })],
    ]);
    const container = renderView(v);
    const chip = container.querySelector('.readiness-unfilled');
    expect(chip).toBeTruthy();
    expect(chip!.textContent).toContain('1 value to fill');
  });

  it('warns on the authored side when promotion would delete a field', () => {
    const container = renderView(
      view({
        outcomes: [
          {
            path: [{ kind: 'StructField', value: 'end_date' }],
            side: { kind: 'In' },
            outcome: { kind: 'Dropped' },
          },
        ],
      })
    );
    expect(container.querySelectorAll('.fate-dropped').length).toBe(1);
    // ...and the rebuilt pane says nothing about it: not its field.
    expect(container.querySelectorAll('.carry-mark').length).toBe(0);
  });

  it('marks a conversion so it does not look like something the tester typed', () => {
    const container = renderView(
      view({
        outcomes: [
          {
            path: [{ kind: 'StructField', value: 'start_date' }],
            side: { kind: 'In' },
            outcome: { kind: 'Wrap' },
          },
        ],
      })
    );
    // Reassurance is an icon; the sentence lives in the tooltip.
    const mark = container.querySelector('.carry-mark.carry-done');
    expect(mark).toBeTruthy();
    expect(mark!.getAttribute('title')).toMatch(/now optional/);
    expect(screen.queryByText(/now optional/)).toBeNull();
  });

  it('says what it could not do, in the reader\u2019s language', () => {
    renderView(
      view({
        notes: [
          {
            kind: 'ModuleNotFound',
            value: { module_name: 'B', candidates: [] },
          },
        ],
      })
    );
    expect(screen.getByText(/declaring module/)).toBeTruthy();
  });

  it('shows what a run disagreed with, not just that it failed', () => {
    // A plain ValueEditor accepts diffs and draws nothing; the assertion
    // editor must be the one rendering them.
    const withOutputs: Recovery = view();
    const expected = io({ kind: 'TMoney' }, { value: moneyVal(100) });
    withOutputs.tests[0].authored.test_outputs = new Map([['total', expected]]);
    withOutputs.tests[0].rebuilt!.tested_scope.outputs = new Map([
      ['total', { kind: 'TMoney' }],
    ]);
    withOutputs.tests[0].rebuilt!.test_outputs = new Map([['total', expected]]);
    const container = renderView(withOutputs, {
      C_one: {
        status: 'error',
        results: {
          kind: 'Ok',
          value: {
            test_outputs: new Map(),
            assert_failures: true,
            diffs: [
              {
                path: [{ kind: 'StructField', value: 'total' }],
                expected: { value: { kind: 'Money', value: 100 }, attrs: [] },
                actual: { value: { kind: 'Money', value: 250 }, attrs: [] },
              },
            ],
          },
        },
      },
    });
    // "Expected" alone also matches the outputs heading.
    const actual = container.querySelector('.diff-actual');
    expect(actual).not.toBeNull();
    expect(actual?.textContent).toMatch(/2[.,]50/);
  });

  it('shows a context variable as using its computed default, not as blank', () => {
    // A context var the test never overrode arrives as NotOverridden; the
    // rebuilt pane must render the ordinary placeholder, not an empty editor.
    const v = view();
    v.tests[0].rebuilt!.tested_scope.inputs = new Map([
      ['rate', { typ: { kind: 'TRat' }, is_context: true }],
    ]);
    v.tests[0].rebuilt!.test_inputs = new Map([
      ['rate', io({ kind: 'TRat' }, { value: rv({ kind: 'NotOverridden' }) })],
    ]);
    const container = renderView(v);
    expect(screen.getByText(/Using computed value/)).toBeTruthy();
    expect(container.querySelector('.context-var-badge')).not.toBeNull();
    expect(
      container.querySelector('.broken-pane-rebuilt .value-editor')
    ).toBeNull();
  });

  it('lets the tester override a context variable from the rebuilt pane', () => {
    const v = view();
    v.tests[0].rebuilt!.tested_scope.inputs = new Map([
      ['rate', { typ: { kind: 'TRat' }, is_context: true }],
    ]);
    v.tests[0].rebuilt!.test_inputs = new Map([
      ['rate', io({ kind: 'TRat' }, { value: rv({ kind: 'NotOverridden' }) })],
    ]);
    const container = renderView(v);
    fireEvent.click(screen.getByText('Override'));
    expect(
      container.querySelector('.broken-pane-rebuilt .value-editor')
    ).not.toBeNull();
  });

  it("keeps a context output's two sides apart: the assertion's mark is not masked", () => {
    // z is `context output`: it appears in both records, so it produces one
    // carry record per side. The output side's TypeChanged explains the
    // emptied assertion; the input side's WasUnset (never overridden) must
    // neither mask it nor decorate the healthy input row.
    const v = view();
    const pair = v.tests[0];
    pair.rebuilt!.tested_scope.inputs = new Map([
      ['z', { typ: { kind: 'TRat' }, is_context: true }],
    ]);
    pair.rebuilt!.tested_scope.outputs = new Map([['z', { kind: 'TRat' }]]);
    pair.rebuilt!.test_inputs = new Map([
      ['z', io({ kind: 'TRat' }, { value: rv({ kind: 'NotOverridden' }) })],
    ]);
    pair.rebuilt!.test_outputs = new Map([['z', io({ kind: 'TRat' })]]);
    pair.outcomes = [
      {
        path: [{ kind: 'StructField', value: 'z' }],
        side: { kind: 'In' },
        outcome: { kind: 'WasUnset' },
      },
      {
        path: [{ kind: 'StructField', value: 'z' }],
        side: { kind: 'Out' },
        outcome: {
          kind: 'TypeChanged',
          value: [{ kind: 'TMoney' }, { kind: 'TRat' }],
        },
      },
    ];
    const container = renderView(v);
    // The explanation survives, exactly once, and on the outputs side.
    const marks = container.querySelectorAll(
      '.test-output-row .fate-attention'
    );
    expect(marks.length).toBe(1);
    expect(marks[0].getAttribute('title')).toMatch(/money → decimal/);
    expect(
      container.querySelector('.test-output-row .fate-attention')
    ).not.toBeNull();
  });

  it('names the constructor an enum lost, instead of the unchanged type name', () => {
    const code = (ctors: string[]): Typ => ({
      kind: 'TEnum',
      value: {
        enum_name: 'B.Code',
        constructors: new Map(ctors.map((c) => [c, null])),
        ctor_attrs: new Map(),
      },
    });
    const v = view();
    const pair = v.tests[0];
    pair.rebuilt!.tested_scope.outputs = new Map([['z', code(['N', 'T'])]]);
    pair.rebuilt!.test_outputs = new Map([['z', io(code(['N', 'T']))]]);
    pair.outcomes = [
      {
        path: [{ kind: 'StructField', value: 'z' }],
        side: { kind: 'Out' },
        outcome: {
          kind: 'TypeChanged',
          value: [code(['R']), code(['N', 'T'])],
        },
      },
    ];
    const container = renderView(v);
    const mark = container.querySelector('.test-output-row .fate-attention');
    expect(mark?.getAttribute('title')).toBe(
      'Code no longer has R: pick a new value'
    );
  });

  it('offers to add an assertion for an unasserted output, and adds it on request', () => {
    // The rebuild lists every scope output; one the test never asserted must
    // not render as an (empty-looking) expected value.
    const v = view();
    v.tests[0].rebuilt!.tested_scope.outputs = new Map([
      ['total', { kind: 'TMoney' }],
    ]);
    v.tests[0].rebuilt!.test_outputs = new Map([
      ['total', io({ kind: 'TMoney' })],
    ]);
    const container = renderView(v);
    expect(
      container.querySelector('.test-output-row .assertion-value-editor')
    ).toBeNull();
    fireEvent.click(screen.getByText(/Add expected value/));
    expect(
      container.querySelector('.test-output-row .assertion-value-editor')
    ).not.toBeNull();
  });

  it('keeps the carry mark on an output whose assertion could not follow', () => {
    const v = view();
    v.tests[0].rebuilt!.tested_scope.outputs = new Map([
      ['total', { kind: 'TMoney' }],
    ]);
    v.tests[0].rebuilt!.test_outputs = new Map([
      ['total', io({ kind: 'TMoney' })],
    ]);
    v.tests[0].outcomes = [
      ...v.tests[0].outcomes,
      {
        path: [{ kind: 'StructField', value: 'total' }],
        side: { kind: 'Out' },
        outcome: {
          kind: 'TypeChanged',
          value: [{ kind: 'TInt' }, { kind: 'TMoney' }],
        },
      },
    ];
    const container = renderView(v);
    const dots = container.querySelectorAll('.fate-attention');
    const titles = [...dots].map((d) => d.getAttribute('title') ?? '');
    expect(titles.some((t) => /integer → money/.test(t))).toBe(true);
  });

  it('still shows the authored values, and offers no exit, when there is nothing to rebuild against', () => {
    // No signature, no rebuild: nothing to replace the original WITH.
    render(
      <IntlProvider locale="en" messages={enMessages}>
        <BrokenTestView
          view={view({ blocked: true })}
          onReplace={(): void => {}}
          onDiscard={(): void => {}}
        />
      </IntlProvider>
    );
    expect(screen.getAllByDisplayValue('2999-12-31').length).toBe(1);
    expect(screen.getByText(/No current signature/)).toBeTruthy();
    expect(screen.queryByText(/Replace the original/)).toBeNull();
    expect(screen.queryByText(/Discard the working copy/)).toBeNull();
  });

  it('does not tell the tester to rebuild when there is nothing to rebuild on', () => {
    renderView(
      view({
        blocked: true,
        notes: [
          {
            kind: 'ModuleWontCompile',
            value: { name: 'B', error: 'unknown identifier "base"' },
          },
        ],
      })
    );
    expect(screen.queryByText(/Rebuild the test on the right/)).toBeNull();
    expect(screen.getByText(/cannot be rebuilt yet/)).toBeTruthy();
  });

  it("shows the compiler's own words when a module will not build", () => {
    renderView(
      view({
        blocked: true,
        notes: [
          {
            kind: 'ModuleWontCompile',
            value: { name: 'B', error: 'unknown identifier "base"' },
          },
        ],
      })
    );
    expect(screen.getByText(/unknown identifier "base"/)).toBeTruthy();
  });

  it('says a scope is gone rather than blaming the module that compiles fine', () => {
    renderView(
      view({
        blocked: true,
        notes: [
          {
            kind: 'ScopeNotFound',
            value: {
              module_name: 'B',
              scope_name: 'C',
              candidates: [
                { module_name: 'B', name: 'Allocation', shared: 9, out_of: 10 },
                { name: 'D', shared: 0, out_of: 10 },
              ],
            },
          },
        ],
      })
    );
    expect(screen.getByText(/no longer has a scope/)).toBeTruthy();
    expect(
      screen.getByRole('option', { name: /Allocation \(9\/10\)/ })
    ).toBeTruthy();
    expect(screen.queryByText(/does not compile/)).toBeNull();
  });

  it('lets the tester say which scope the test was meant for', () => {
    // Candidates are ranked, never chosen.
    const chosen: string[] = [];
    render(
      <IntlProvider locale="en" messages={enMessages}>
        <BrokenTestView
          view={view({
            blocked: true,
            notes: [
              {
                kind: 'ScopeNotFound',
                value: {
                  module_name: 'B',
                  scope_name: 'C',
                  candidates: [
                    {
                      module_name: 'B',
                      name: 'Allocation',
                      shared: 9,
                      out_of: 10,
                    },
                  ],
                },
              },
            ],
          })}
          onRetarget={(s): void => {
            chosen.push(s);
          }}
        />
      </IntlProvider>
    );
    // Nothing chosen yet: the button waits for the tester.
    const button = screen.getByRole('button', { name: /Rebuild against/ });
    expect((button as HTMLButtonElement).disabled).toBe(true);
    fireEvent.change(screen.getByRole('combobox'), {
      target: { value: 'B.Allocation' },
    });
    expect(screen.getByText(/9 of this test's 10/)).toBeTruthy();
    fireEvent.click(screen.getByRole('button', { name: /Rebuild against/ }));
    expect(chosen).toEqual(['B.Allocation']);
  });

  it('hands the rebuild over when the tester replaces the original', () => {
    const replaced: TestList[] = [];
    render(
      <IntlProvider locale="en" messages={enMessages}>
        <BrokenTestView
          view={view()}
          onReplace={(tests): void => {
            replaced.push(tests);
          }}
        />
      </IntlProvider>
    );
    fireEvent.click(screen.getByText(/Replace the original/));
    expect(replaced.length).toBe(1);
    expect(replaced[0][0].test_inputs.get('start_date')).toEqual(
      view().tests[0].rebuilt!.test_inputs.get('start_date')
    );
  });

  it('lets the tester throw the working copy away', () => {
    let discarded = 0;
    render(
      <IntlProvider locale="en" messages={enMessages}>
        <BrokenTestView
          view={view()}
          onDiscard={(): void => {
            discarded += 1;
          }}
        />
      </IntlProvider>
    );
    fireEvent.click(screen.getByText(/Discard the working copy/));
    expect(discarded).toBe(1);
  });

  it('offers scopes from other modules when the module itself is gone', () => {
    const chosen: string[] = [];
    render(
      <IntlProvider locale="en" messages={enMessages}>
        <BrokenTestView
          view={view({
            blocked: true,
            notes: [
              {
                kind: 'ModuleNotFound',
                value: {
                  module_name: 'B',
                  candidates: [
                    {
                      module_name: 'Liquidation',
                      name: 'Attribution',
                      shared: 9,
                      out_of: 10,
                    },
                  ],
                },
              },
            ],
          })}
          onRetarget={(s): void => {
            chosen.push(s);
          }}
        />
      </IntlProvider>
    );
    expect(screen.getByText(/Which scope should it target now/)).toBeTruthy();
    fireEvent.change(screen.getByRole('combobox'), {
      target: { value: 'Liquidation.Attribution' },
    });
    fireEvent.click(screen.getByRole('button', { name: /Rebuild against/ }));
    expect(chosen).toEqual(['Liquidation.Attribution']);
  });

  it('shows the rebuild once the tester has picked a scope', () => {
    // The view arrives anew after a retarget; the right pane must follow it.
    const { rerender } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <BrokenTestView view={view({ blocked: true })} />
      </IntlProvider>
    );
    expect(screen.getByText(/No current signature/)).toBeTruthy();
    rerender(
      <IntlProvider locale="en" messages={enMessages}>
        <BrokenTestView view={view()} />
      </IntlProvider>
    );
    expect(screen.queryByText(/No current signature/)).toBeNull();
    // start_date carried across, so it is now on both sides
    expect(screen.getAllByDisplayValue('2025-01-01').length).toBe(2);
  });
});

describe('marks below the field level', () => {
  const pairTyp: Typ = {
    kind: 'TStruct',
    value: {
      struct_name: 'B.Pair',
      fields: new Map<string, Typ>([
        ['first', { kind: 'TInt' }],
        ['amount', { kind: 'TMoney' }],
      ]),
    },
  };
  const pairVal = rv({
    kind: 'Struct',
    value: [
      pairTyp.value as never,
      new Map([
        ['first', rv({ kind: 'Integer', value: 1 })],
        ['amount', rv({ kind: 'Unset' })],
      ]),
    ],
  });
  it('shows a carried record with its blank field marked inside', () => {
    const base = authored();
    const reb: Test = {
      ...base,
      test_inputs: new Map<string, TestIo>([
        ['pair', io(pairTyp, { value: pairVal })],
      ]),
    };
    const v: Recovery = {
      tests: [
        {
          authored: {
            ...base,
            test_inputs: new Map([['pair', io(pairTyp, { value: pairVal })]]),
          },
          rebuilt: reb,
          outcomes: [
            {
              path: [{ kind: 'StructField', value: 'pair' }],
              side: { kind: 'In' },
              outcome: { kind: 'Partial' },
            },
            {
              path: [
                { kind: 'StructField', value: 'pair' },
                { kind: 'StructField', value: 'amount' },
              ],
              side: { kind: 'In' },
              outcome: { kind: 'WasUnset' },
            },
          ],
        },
      ],
      notes: [],
      working_copy: 'x.repair',
    };
    const container = renderView(v);
    const rebuiltPane = container.querySelector('.broken-pane-rebuilt')!;
    const nested = rebuiltPane.querySelectorAll('.carry-marked .fate-mark');
    expect(nested.length).toBe(1);
    expect(nested[0].getAttribute('title')).toBe(
      enMessages['broken.markWasUnset']
    );
    const authoredPane = container.querySelector('.broken-pane-authored')!;
    expect(authoredPane.querySelectorAll('.carry-marked').length).toBe(0);
  });
});

describe('destination hints', () => {
  it('appends the hint rebuild computed to the mark tooltip', () => {
    const container = renderView(
      view({
        outcomes: [
          {
            path: [{ kind: 'StructField', value: 'end_date' }],
            side: { kind: 'In' },
            outcome: { kind: 'WasUnset' },
            hint: ['start_date'],
          },
          {
            path: [{ kind: 'StructField', value: 'start_date' }],
            side: { kind: 'In' },
            outcome: { kind: 'Dropped' },
            hint: [],
          },
        ],
      })
    );
    const mark = container.querySelector('.broken-pane-rebuilt .fate-mark');
    expect(mark?.getAttribute('title')).toContain('start_date');
  });
});

// A nested carry mark must survive re-renders that do not go through TestPanes: a tab switch, or table-local state.
describe('regression: nested-mark-rerender', () => {
  const pointDecl: StructDeclaration = {
    struct_name: 'B.Point',
    fields: new Map<string, Typ>([
      ['x', { kind: 'TInt' }],
      ['y', { kind: 'TInt' }],
    ]),
  };
  const pointTyp: Typ = { kind: 'TStruct', value: pointDecl };
  const pointsTyp: Typ = { kind: 'TArray', value: pointTyp };

  function io(typ: Typ, value?: TestIo['value']): TestIo {
    return { typ, value };
  }

  function test(): Test {
    const holed = structVal(
      pointDecl,
      new Map([
        ['x', intVal(1)],
        ['y', rv({ kind: 'Unset' })],
      ])
    );
    return {
      testing_scope: 'T',
      tested_scope: {
        module_name: 'B',
        name: 'C',
        inputs: new Map([
          ['points', { typ: pointsTyp, is_context: false }],
          ['others', { typ: pointsTyp, is_context: false }],
        ]),
        outputs: new Map(),
        module_deps: [],
      },
      test_inputs: new Map<string, TestIo>([
        ['points', io(pointsTyp, { value: arrayVal([holed]) })],
        ['others', io(pointsTyp, { value: arrayVal([]) })],
      ]),
      test_outputs: new Map(),
      description: '',
      title: '',
    };
  }

  function view(): Recovery {
    return {
      tests: [
        {
          authored: test(),
          rebuilt: test(),
          outcomes: [
            {
              path: [{ kind: 'StructField', value: 'points' }],
              side: { kind: 'In' },
              outcome: { kind: 'Partial' },
            },
            {
              path: [
                { kind: 'StructField', value: 'points' },
                { kind: 'ListIndex', value: 0 },
                { kind: 'StructField', value: 'y' },
              ],
              side: { kind: 'In' },
              outcome: { kind: 'WasUnset' },
            },
          ],
        },
      ],
      notes: [],
      working_copy: 'x.repair',
    };
  }

  describe('nested carry marks under a subtree re-render', () => {
    it('keeps the mark on points[0].y after switching tabs and back', () => {
      const { container } = render(
        <IntlProvider locale="en" messages={enMessages}>
          <BrokenTestView view={view()} onRun={(): void => {}} />
        </IntlProvider>
      );
      const rebuilt = container.querySelector('.broken-pane-rebuilt')!;
      const marksBefore = rebuilt.querySelectorAll('.carry-marked .fate-mark');
      expect(marksBefore.length).toBe(1);

      // A tab switch re-renders CompositeEditor and every editor under it,
      // without going through TestPanes.
      const tabs = Array.from(rebuilt.querySelectorAll('button.tab'));
      const others = tabs.find((t) => t.textContent?.includes('others'))!;
      const points = tabs.find((t) => t.textContent?.includes('points'))!;
      fireEvent.click(others);
      fireEvent.click(points);

      const marksAfter = rebuilt.querySelectorAll('.carry-marked .fate-mark');
      expect(marksAfter.length).toBe(1);
    });

    it('keeps the mark after the table re-renders from its own state', () => {
      const { container } = render(
        <IntlProvider locale="en" messages={enMessages}>
          <BrokenTestView view={view()} onRun={(): void => {}} />
        </IntlProvider>
      );
      const rebuilt = container.querySelector('.broken-pane-rebuilt')!;
      expect(rebuilt.querySelectorAll('.carry-marked .fate-mark').length).toBe(
        1
      );

      // Right-clicking a row header opens the row context menu: table-local
      // state, so TableArrayEditor re-renders its cells without TestPanes.
      const rowControls = rebuilt.querySelector('.table-cell-controls')!;
      fireEvent.contextMenu(rowControls, { clientX: 10, clientY: 10 });

      expect(rebuilt.querySelectorAll('.carry-marked .fate-mark').length).toBe(
        1
      );
    });
  });
});

// On the authored pane, a dropped list input keeps its red mark on the tab header even when its value holds a blank: the authored pane counts nothing.
describe('regression: authored-tab-mark', () => {
  const intsTyp: Typ = { kind: 'TArray', value: { kind: 'TInt' } };

  function io(typ: Typ, value?: TestIo['value']): TestIo {
    return { typ, value };
  }

  function authored(): Test {
    return {
      testing_scope: 'T',
      tested_scope: {
        module_name: 'B',
        name: 'C',
        inputs: new Map([
          ['kept', { typ: intsTyp, is_context: false }],
          ['gone', { typ: intsTyp, is_context: false }],
        ]),
        outputs: new Map(),
        module_deps: [],
      },
      test_inputs: new Map<string, TestIo>([
        ['kept', io(intsTyp, { value: arrayVal([intVal(1)]) })],
        // The tester left one element as `impossible`.
        ['gone', io(intsTyp, { value: arrayVal([rv({ kind: 'Unset' })]) })],
      ]),
      test_outputs: new Map(),
      description: '',
      title: '',
    };
  }

  function rebuilt(): Test {
    const a = authored();
    return {
      ...a,
      tested_scope: {
        ...a.tested_scope,
        inputs: new Map([['kept', { typ: intsTyp, is_context: false }]]),
      },
      test_inputs: new Map<string, TestIo>([
        ['kept', io(intsTyp, { value: arrayVal([intVal(1)]) })],
      ]),
    };
  }

  describe('authored pane tab headers', () => {
    it('shows the dropped mark on a list input that also holds a blank', () => {
      const v: Recovery = {
        tests: [
          {
            authored: authored(),
            rebuilt: rebuilt(),
            outcomes: [
              {
                path: [{ kind: 'StructField', value: 'kept' }],
                side: { kind: 'In' },
                outcome: { kind: 'Fits' },
              },
              {
                path: [{ kind: 'StructField', value: 'gone' }],
                side: { kind: 'In' },
                outcome: { kind: 'Dropped' },
              },
            ],
          },
        ],
        notes: [],
        working_copy: 'x.repair',
      };
      const { container } = render(
        <IntlProvider locale="en" messages={enMessages}>
          <BrokenTestView view={v} onRun={(): void => {}} />
        </IntlProvider>
      );
      const authoredPane = container.querySelector('.broken-pane-authored')!;
      const tabs = Array.from(authoredPane.querySelectorAll('button.tab'));
      expect(tabs.length).toBe(2);
      const gone = tabs.find((t) => t.textContent?.includes('gone'))!;
      expect(gone.querySelector('.fate-dropped')).not.toBeNull();
    });
  });
});
