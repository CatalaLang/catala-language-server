/** When `read` fails: repair view or error page. One row per decision. */
import { describe, it, expect } from 'vitest';
import type {
  BrokenNote,
  CarryOutcome,
  Recovery,
  Test,
} from '../../src/generated/catala_types';
import {
  decideOnReadFailure,
  type OpenDecision,
} from '../../src/test-case-editor/openDecision';

function test(): Test {
  return {
    testing_scope: 'T',
    tested_scope: { module_name: 'B', name: 'C', inputs: [], outputs: [] },
    test_inputs: new Map(),
    test_outputs: new Map(),
    description: '',
    title: '',
  };
}

const readError = 'Required module not found: B';
const moduleError = 'Unknown type "Count"';
const notFound = { module_name: 'B', candidates: [] };
const workingCopy = { name: 'test.catala_en.repair', error: 'syntax error' };

/** One test, rebuilt unless no live signature, one field per outcome. */
function view(
  notes: BrokenNote[],
  outcomes: CarryOutcome['kind'][] | 'no rebuild'
): Recovery {
  const rebuilt = outcomes !== 'no rebuild';
  return {
    tests: [
      {
        authored: test(),
        rebuilt: rebuilt ? test() : undefined,
        outcomes: rebuilt
          ? outcomes.map((kind) => ({
              path: [{ kind: 'StructField', value: 'x' }],
              side: { kind: 'In' },
              outcome: { kind } as CarryOutcome,
              hint: [],
            }))
          : [],
      },
    ],
    notes,
    working_copy: 'test.catala_en.repair',
  };
}

const repair: OpenDecision = { kind: 'repair' };
const moduleWontBuild: OpenDecision = {
  kind: 'cannotOpen',
  cause: { kind: 'ModuleWontBuild' },
  message: moduleError,
};
const toolProblem = (message: string): OpenDecision => ({
  kind: 'cannotOpen',
  cause: { kind: 'ToolProblem' },
  message,
});

const noDraft = false;
const draft = true;

const rows: [string, Recovery, boolean, OpenDecision][] = [
  [
    'scope renamed: repair, picking a new scope',
    view(
      [
        {
          kind: 'ScopeNotFound',
          value: { module_name: 'B', scope_name: 'C', candidates: [] },
        },
      ],
      'no rebuild'
    ),
    noDraft,
    repair,
  ],
  [
    'module renamed: repair, picking a new module',
    view([{ kind: 'ModuleNotFound', value: notFound }], 'no rebuild'),
    noDraft,
    repair,
  ],
  ['a field renamed: repair', view([], ['Fits', 'Dropped']), noDraft, repair],
  ['a value wrapped in an option: repair', view([], ['Wrap']), noDraft, repair],
  [
    'module does not build (or not from where catala runs): error page',
    view(
      [{ kind: 'ModuleWontCompile', value: { name: 'B', error: moduleError } }],
      'no rebuild'
    ),
    noDraft,
    moduleWontBuild,
  ],
  [
    'module builds, has the scope, rebuild fails anyway: error page',
    view(
      [{ kind: 'Other', value: { name: 'B.C', error: 'Not_found' } }],
      'no rebuild'
    ),
    noDraft,
    toolProblem('Not_found'),
  ],
  [
    'everything fits (catala ran from above the project): read error',
    view([], ['Fits', 'Fits']),
    noDraft,
    toolProblem(readError),
  ],
  [
    'a draft that fits its target is a repair ready to apply: repair',
    view([], ['Fits', 'Fits']),
    draft,
    repair,
  ],
  [
    'a draft that cannot be read is still a draft: repair',
    view([{ kind: 'WorkingCopyUnreadable', value: workingCopy }], ['Fits']),
    draft,
    repair,
  ],
  [
    'a draft whose module does not build: error page, the draft is kept',
    view(
      [{ kind: 'ModuleWontCompile', value: { name: 'B', error: moduleError } }],
      'no rebuild'
    ),
    draft,
    moduleWontBuild,
  ],
];

describe('decideOnReadFailure', () => {
  it.each(rows)('%s', (_, recovery, hasDraft, expected) => {
    expect(decideOnReadFailure(readError, recovery, hasDraft)).toEqual(
      expected
    );
  });
});
