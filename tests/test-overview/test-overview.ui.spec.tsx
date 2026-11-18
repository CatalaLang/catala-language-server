/**
 * The test overview panel, driven through its messages: what the extension
 * host sends (the list of tests, the run results) and what the panel asks for
 * in return (runs, reloads, opening a test).
 */
import React from 'react';
import { describe, it, expect, vi } from 'vitest';
import { act, fireEvent, render, screen, within } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import type { WebviewApi } from 'vscode-webview';
import type {
  TestDebugger,
  TestsDownMessage,
} from '../../src/generated/catala_types';
import {
  readTestsUpMessage,
  writeTestsDownMessage,
} from '../../src/generated/catala_types';
import TestOverview from '../../src/test-overview/TestOverview';
import enMessages from '../../src/locales/en.json';
import { guiTest, plainTest } from './fixtures';

// A web component: jsdom cannot build its shadow DOM. A plain input in its
// place, with the same props.
vi.mock('@vscode-elements/react-elements', () => ({
  VscodeTextfield: ({
    value,
    placeholder,
    onInput,
  }: {
    value: string;
    placeholder: string;
    onInput: (e: React.FormEvent<HTMLInputElement>) => void;
  }) => (
    <input
      value={value}
      placeholder={placeholder}
      onInput={onInput}
      onChange={() => {}}
    />
  ),
}));

const allowance = guiTest({
  file: '/work/tests/test_allowance.catala_en',
  scope: 'TestAllowance',
  tested: 'Allowance',
  title: 'Two children',
});
const pension = guiTest({
  file: '/work/tests/test_pension.catala_en',
  scope: 'TestPension',
  tested: 'Pension',
  title: 'Early retirement',
});
const tax = plainTest({
  file: '/work/tests/test_tax.catala_en',
  scope: 'TestTax',
});

function panel(): ReturnType<typeof vi.fn> {
  const postMessage = vi.fn();
  render(
    <IntlProvider locale="en" messages={enMessages}>
      <TestOverview
        vscode={{ postMessage } as unknown as WebviewApi<unknown>}
      />
    </IntlProvider>
  );
  return postMessage;
}

const sent = (post: ReturnType<typeof vi.fn>): unknown[] =>
  post.mock.calls.map(([m]) => readTestsUpMessage(m));

function hostSends(message: TestsDownMessage): void {
  act(() => {
    window.dispatchEvent(
      new MessageEvent('message', { data: writeTestsDownMessage(message) })
    );
  });
}

const listed = (tests: TestDebugger[]): void =>
  hostSends({ kind: 'AllTests', value: tests });

function result(
  test: TestDebugger,
  success: boolean,
  order = false
): TestsDownMessage {
  return {
    kind: 'TestScopeResult',
    value: {
      entry: test.test,
      scope_success: { success, date: '2026-11-03T14:32:00.000Z' },
      test_id: test.test_id,
      order,
    },
  };
}

/** The titles of the cards, top to bottom. */
const cardTitles = (): string[] =>
  Array.from(document.querySelectorAll('.test-item .test-title')).map(
    (e) => e.textContent ?? ''
  );

/** The card of the test of that title. */
function card(title: string): HTMLElement {
  const item = screen
    .getByText(title, { selector: '.test-title' })
    .closest('.test-item');
  if (!(item instanceof HTMLElement)) {
    throw new Error(`No card for ${title}`);
  }
  return item;
}

describe('listing', () => {
  it('waits for the host before showing any test', () => {
    panel();
    expect(cardTitles()).toEqual([]);
    listed([allowance]);
    expect(cardTitles()).toEqual(['Two children']);
  });

  it('shows only the GUI tests until "GUI tests only" is unchecked', () => {
    panel();
    listed([allowance, tax]);
    expect(cardTitles()).toEqual(['Two children']);
    fireEvent.click(screen.getByRole('checkbox'));
    expect(cardTitles()).toEqual(['Two children', 'TestTax']);
  });

  it('puts the failed tests first', () => {
    panel();
    listed([
      {
        ...allowance,
        last_run: { success: true, date: '2026-11-03T14:32:00.000Z' },
      },
      {
        ...pension,
        last_run: { success: false, date: '2026-11-03T14:32:00.000Z' },
      },
    ]);
    expect(cardTitles()).toEqual(['Early retirement', 'Two children']);
  });

  it('a card tells a test never run from a test that ran', () => {
    panel();
    listed([
      allowance,
      {
        ...pension,
        last_run: { success: true, date: '2026-11-03T14:32:00.000Z' },
      },
    ]);
    within(card('Two children')).getByText('Never run');
    within(card('Two children')).getByTitle('Test never run');
    within(card('Early retirement')).getByText(/^Tested on /);
    within(card('Early retirement')).getByTitle('Test passed');
  });

  it('says so when there is no test', () => {
    panel();
    listed([]);
    screen.getByText('No test found');
  });
});

describe('filters', () => {
  it('the search box narrows the list', () => {
    panel();
    listed([allowance, pension]);
    fireEvent.input(screen.getByPlaceholderText('Search a test…'), {
      target: { value: 'retirement' },
    });
    expect(cardTitles()).toEqual(['Early retirement']);
  });

  it('a scope pin keeps the tests of that scope, under its name', () => {
    panel();
    listed([allowance, pension]);
    fireEvent.click(screen.getByText('Pension', { selector: '.scope-title' }));
    expect(cardTitles()).toEqual(['Early retirement']);
    screen.getByRole('heading', { name: 'Pension' });
  });

  it('reset brings every test back', () => {
    panel();
    listed([allowance, pension]);
    fireEvent.click(screen.getByText('Pension', { selector: '.scope-title' }));
    fireEvent.click(screen.getByText('Reset filters'));
    expect(cardTitles()).toEqual(['Two children', 'Early retirement']);
  });
});

describe('runs', () => {
  it('the run button of a card asks for that test, by its id', () => {
    const post = panel();
    listed([allowance, pension]);
    fireEvent.click(
      within(card('Early retirement')).getByTitle('Run the test')
    );
    expect(sent(post)).toEqual([
      { kind: 'SpecificTestRequest', value: [pension.test_id] },
    ]);
    within(card('Early retirement')).getByTitle('Test running…');
    within(card('Two children')).getByTitle('Test never run');
  });

  it('"Run tests" asks for the tests shown when a filter is on', () => {
    const post = panel();
    listed([allowance, tax]);
    fireEvent.click(screen.getByText('Run tests'));
    expect(sent(post)).toEqual([
      { kind: 'SpecificTestRequest', value: [allowance.test_id] },
    ]);
  });

  it('"Run tests" asks for every test when no filter is on', () => {
    const post = panel();
    listed([allowance, tax]);
    fireEvent.click(screen.getByRole('checkbox'));
    fireEvent.click(screen.getByText('Run tests'));
    expect(sent(post)).toEqual([{ kind: 'SpecificTestRequest', value: [] }]);
  });

  it('a result goes to the test of its id, wherever it stands', () => {
    panel();
    listed([allowance, pension]);
    fireEvent.click(
      within(card('Early retirement')).getByTitle('Run the test')
    );
    hostSends(result(pension, true));
    within(card('Early retirement')).getByTitle('Test passed');
    within(card('Two children')).getByTitle('Test never run');
  });

  it('a failure moves up when the run reorders, and only then', () => {
    panel();
    listed([allowance, pension]);
    hostSends(result(pension, false));
    expect(cardTitles()).toEqual(['Two children', 'Early retirement']);
    hostSends(result(pension, false, true));
    expect(cardTitles()).toEqual(['Early retirement', 'Two children']);
  });

  it('a result for a test no longer listed changes nothing', () => {
    panel();
    listed([allowance]);
    hostSends(result(pension, false, true));
    expect(cardTitles()).toEqual(['Two children']);
    within(card('Two children')).getByTitle('Test never run');
  });
});

describe('requests to the host', () => {
  it('opens a GUI test in the test case editor', () => {
    const post = panel();
    listed([allowance]);
    fireEvent.click(
      within(card('Two children')).getByTitle('Open the Catala editor')
    );
    expect(sent(post)).toEqual([
      { kind: 'OpenInTestEditor', value: allowance.filename },
    ]);
  });

  it('opens a plain test in the text editor', () => {
    const post = panel();
    listed([tax]);
    fireEvent.click(screen.getByRole('checkbox'));
    fireEvent.click(within(card('TestTax')).getByTitle('Open the text editor'));
    expect(sent(post)).toEqual([
      { kind: 'OpenInTextEditor', value: { value: tax.filename } },
    ]);
  });

  it('"Add test" opens the scope picker', () => {
    const post = panel();
    listed([]);
    fireEvent.click(screen.getAllByText('Add test')[0]);
    expect(sent(post)).toEqual([{ kind: 'OpenTestScopePicker' }]);
  });

  it('reload asks for the list again, and waits for it', () => {
    const post = panel();
    listed([allowance]);
    const reload = screen.getByTitle('Reload the test list');
    fireEvent.click(reload.querySelector('.refresh')!);
    expect(sent(post)).toEqual([{ kind: 'Ready' }]);
    screen.getByTitle('Reloading…');
    listed([allowance, pension]);
    screen.getByTitle('Reload the test list');
    expect(cardTitles()).toEqual(['Two children', 'Early retirement']);
  });
});
