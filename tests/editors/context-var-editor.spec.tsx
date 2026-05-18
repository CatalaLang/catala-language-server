import React from 'react';
import { describe, it, expect, vi } from 'vitest';
import { render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../../src/locales/en.json';
import TestInputsEditor from '../../src/test-case-editor/TestInputsEditor';
import type { ScopeDef, TestInputs } from '../../src/generated/catala_types';

const testedScope: ScopeDef = {
  name: 'C',
  module_name: 'Context',
  inputs: new Map([
    ['x', { typ: { kind: 'TInt' }, is_context: false }],
    ['y', { typ: { kind: 'TInt' }, is_context: true }],
  ]),
  outputs: new Map(),
  module_deps: [],
};

function makeInputs(yKind: 'NotOverridden' | number): TestInputs {
  return new Map([
    [
      'x',
      {
        typ: { kind: 'TInt' },
        value: {
          value: { value: { kind: 'Int', value: 5 } },
          pos: undefined,
        },
      },
    ],
    [
      'y',
      {
        typ: { kind: 'TInt' },
        value:
          yKind === 'NotOverridden'
            ? { value: { value: { kind: 'NotOverridden' } }, pos: undefined }
            : {
                value: { value: { kind: 'Int', value: yKind } },
                pos: undefined,
              },
      },
    ],
  ]);
}

function renderInputsEditor(inputs: TestInputs, onchange = vi.fn()) {
  return render(
    <IntlProvider locale="en" messages={enMessages}>
      <TestInputsEditor
        test_inputs={inputs}
        tested_scope={testedScope}
        onTestInputsChange={onchange}
      />
    </IntlProvider>
  );
}

describe('TestInputsEditor - context variables', () => {
  it('shows placeholder for unset context var, Override button switches to editor, × resets', () => {
    const onChange = vi.fn();
    const { container } = renderInputsEditor(
      makeInputs('NotOverridden'),
      onChange
    );

    // y is context + unset → placeholder mode
    expect(screen.getByText(/using computed value/i)).toBeInTheDocument();
    expect(container.querySelector('.context-var-editor')).toBeNull();

    // click Override → editor appears
    fireEvent.click(screen.getByRole('button', { name: /override/i }));
    expect(screen.queryByText(/using computed value/i)).toBeNull();

    // click reset (trash) → back to placeholder
    fireEvent.click(screen.getByRole('button', { name: /remove override/i }));
    expect(screen.getByText(/using computed value/i)).toBeInTheDocument();
  });

  it('typing an invalid value in an overridden context var keeps the editor visible', () => {
    // y starts with a valid override (99) → override mode
    const { container } = renderInputsEditor(makeInputs(99));

    // sanity: no placeholder visible
    expect(screen.queryByText(/using computed value/i)).toBeNull();

    // type an invalid intermediate value
    const input = container.querySelector(
      '.context-var-editor input'
    ) as HTMLInputElement;
    expect(input).not.toBeNull();
    fireEvent.change(input, { target: { value: '-' } });

    // the editor must still be showing (not reverted to placeholder)
    expect(screen.queryByText(/using computed value/i)).toBeNull();
    expect(container.querySelector('.context-var-editor')).toBeInTheDocument();
    // and the invalid indicator should be present
    expect(
      container.querySelector('.value-editor.invalid')
    ).toBeInTheDocument();
  });
});
