/**
 * The two-pane view shown when a test no longer fits its scope. The left pane
 * must be inert and complete: it is the only copy of what the tester wrote.
 */
import React from 'react';
import { describe, it, expect } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import type {
  Recovery,
  Test,
  TestIo,
  TestList,
  Typ,
} from '../../src/generated/catala_types';
import type { TestRunResults } from '../../src/generated/catala_types';
import type { TestRunStatus } from '../../src/test-case-editor/TestFileEditor';
import BrokenTestView from '../../src/test-case-editor/BrokenTestView';
import { dateVal, moneyVal, rv } from './test-helpers';
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

function view(overrides: Partial<Recovery> = {}): Recovery {
  return {
    original: [authored()],
    rebuilt: [rebuilt()],
    notes: [],
    working_copy: 'test_one.catala_en.updated',
    carry_outcomes: [
      {
        testing_scope: 'C_one',
        field: 'start_date',
        outcome: { kind: 'Fits' },
      },
      {
        testing_scope: 'C_one',
        field: 'end_date',
        outcome: { kind: 'TypeChanged', value: 'date -> B.EndDate' },
      },
    ],
    ...overrides,
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
  it('shows the authored values rather than a compiler error', () => {
    renderView(view());
    expect(screen.getByText('one')).toBeTruthy();
    // The sentinel that would otherwise have been overwritten.
    expect(screen.getAllByDisplayValue('2999-12-31').length).toBeGreaterThan(0);
  });

  it('renders both panes', () => {
    renderView(view());
    expect(screen.getByText('As last saved')).toBeTruthy();
    expect(screen.getByText('Rebuild against the current scope')).toBeTruthy();
  });

  it('leaves a field whose type changed empty rather than guessing', () => {
    renderView(view());
    // start_date survives on both sides; end_date only on the authored one.
    expect(screen.getAllByDisplayValue('2025-01-01').length).toBe(2);
    expect(screen.getAllByDisplayValue('2999-12-31').length).toBe(1);
  });

  it('marks a field that could not be carried, with the reason', () => {
    renderView(view());
    expect(screen.getByText(/date -> B.EndDate/)).toBeTruthy();
  });

  it('marks a conversion so it does not look like something the tester typed', () => {
    renderView(
      view({
        carry_outcomes: [
          {
            testing_scope: 'C_one',
            field: 'start_date',
            outcome: { kind: 'Wrap' },
          },
        ],
      })
    );
    expect(screen.getByText(/now optional/)).toBeTruthy();
  });

  it('says nothing about a field the test simply never set', () => {
    renderView(
      view({
        carry_outcomes: [
          {
            testing_scope: 'C_one',
            field: 'start_date',
            outcome: { kind: 'WasUnset' },
          },
        ],
      })
    );
    expect(
      screen.queryByText(/kept|now required|needs a new value/)
    ).toBeNull();
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
    withOutputs.original[0].test_outputs = new Map([['total', expected]]);
    withOutputs.rebuilt[0].tested_scope.outputs = new Map([
      ['total', { kind: 'TMoney' }],
    ]);
    withOutputs.rebuilt[0].test_outputs = new Map([['total', expected]]);
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
    v.rebuilt[0].tested_scope.inputs = new Map([
      ['rate', { typ: { kind: 'TRat' }, is_context: true }],
    ]);
    v.rebuilt[0].test_inputs = new Map([
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
    v.rebuilt[0].tested_scope.inputs = new Map([
      ['rate', { typ: { kind: 'TRat' }, is_context: true }],
    ]);
    v.rebuilt[0].test_inputs = new Map([
      ['rate', io({ kind: 'TRat' }, { value: rv({ kind: 'NotOverridden' }) })],
    ]);
    const container = renderView(v);
    fireEvent.click(screen.getByText('Override'));
    expect(
      container.querySelector('.broken-pane-rebuilt .value-editor')
    ).not.toBeNull();
  });

  it('offers to add an assertion for an unasserted output, not a blank value', () => {
    // The rebuild lists every scope output; one the test never asserted must
    // not render as an (empty-looking) expected value.
    const v = view();
    v.rebuilt[0].tested_scope.outputs = new Map([
      ['total', { kind: 'TMoney' }],
    ]);
    v.rebuilt[0].test_outputs = new Map([['total', io({ kind: 'TMoney' })]]);
    const container = renderView(v);
    expect(screen.getByText(/Add expected value/)).toBeTruthy();
    expect(
      container.querySelector('.test-output-row .assertion-value-editor')
    ).toBeNull();
  });

  it('creates the assertion when the tester asks for it', () => {
    const v = view();
    v.rebuilt[0].tested_scope.outputs = new Map([
      ['total', { kind: 'TMoney' }],
    ]);
    v.rebuilt[0].test_outputs = new Map([['total', io({ kind: 'TMoney' })]]);
    const container = renderView(v);
    fireEvent.click(screen.getByText(/Add expected value/));
    expect(
      container.querySelector('.test-output-row .assertion-value-editor')
    ).not.toBeNull();
  });

  it('keeps the carry mark on an output whose assertion could not follow', () => {
    const v = view();
    v.rebuilt[0].tested_scope.outputs = new Map([
      ['total', { kind: 'TMoney' }],
    ]);
    v.rebuilt[0].test_outputs = new Map([['total', io({ kind: 'TMoney' })]]);
    v.carry_outcomes = [
      ...v.carry_outcomes,
      {
        testing_scope: 'C_one',
        field: 'total',
        outcome: { kind: 'TypeChanged', value: 'integer -> money' },
      },
    ];
    renderView(v);
    expect(screen.getByText(/integer -> money/)).toBeTruthy();
  });

  it('lets the divider be moved from the keyboard', () => {
    const container = renderView(view());
    const handle = container.querySelector('.broken-split');
    expect(handle).not.toBeNull();
    const before = (
      container.querySelector('.broken-panes') as HTMLElement
    ).style.getPropertyValue('--broken-split');
    fireEvent.keyDown(handle as Element, { key: 'ArrowRight' });
    const after = (
      container.querySelector('.broken-panes') as HTMLElement
    ).style.getPropertyValue('--broken-split');
    expect(after).not.toBe(before);
  });

  it('still shows the authored values when there is nothing to rebuild against', () => {
    renderView(view({ rebuilt: [] }));
    expect(screen.getAllByDisplayValue('2999-12-31').length).toBe(1);
    expect(screen.getByText(/No current signature/)).toBeTruthy();
  });

  it('does not tell the tester to rebuild when there is nothing to rebuild on', () => {
    renderView(
      view({
        rebuilt: [],
        notes: [
          {
            kind: 'ModuleWontCompile',
            value: { name: 'B', error: 'unknown identifier "base"' },
          },
        ],
      })
    );
    expect(screen.queryByText(/carried over for you/)).toBeNull();
    expect(screen.getByText(/cannot be rebuilt yet/)).toBeTruthy();
  });

  it("shows the compiler's own words when a module will not build", () => {
    renderView(
      view({
        rebuilt: [],
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
        rebuilt: [],
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
            rebuilt: [],
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
      view().rebuilt[0].test_inputs.get('start_date')
    );
  });

  it('offers neither exit while the rebuild is blocked', () => {
    // No signature, no rebuild: nothing to replace the original WITH.
    render(
      <IntlProvider locale="en" messages={enMessages}>
        <BrokenTestView
          view={view({ rebuilt: [] })}
          onReplace={(): void => {}}
          onDiscard={(): void => {}}
        />
      </IntlProvider>
    );
    expect(screen.queryByText(/Replace the original/)).toBeNull();
    expect(screen.queryByText(/Discard the working copy/)).toBeNull();
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
            rebuilt: [],
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
        <BrokenTestView view={view({ rebuilt: [] })} />
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
