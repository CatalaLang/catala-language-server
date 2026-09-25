import { describe, it, expect } from 'vitest';
import { useState } from 'react';
import { act, render, screen, fireEvent } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../../src/locales/en.json';
import ValueEditor from '../../src/editors/ValueEditors';
import type { TestIo, Typ } from '../../src/generated/catala_types';
import { moneyVal, rv } from './test-helpers.tsx';

/** A parent that stores each emitted value and hands it back as the prop,
 *  through a clone, the way the test editor does. */
let flush: () => void = () => {};
let deliver: (io: TestIo) => void = () => {};
function Harness({ io: initial }: { io: TestIo }): React.ReactElement {
  const [io, setIo] = useState<TestIo>(initial);
  deliver = setIo;
  return (
    <IntlProvider locale="en" messages={enMessages}>
      <ValueEditor
        testIO={io}
        onValueChange={(next) => {
          const clone = JSON.parse(JSON.stringify(next));
          flush = () => setIo(clone);
        }}
        currentPath={[]}
        diffs={[]}
      />
    </IntlProvider>
  );
}

function type(input: HTMLInputElement, text: string): void {
  fireEvent.change(input, { target: { value: text } });
  act(flush);
}

describe.each([
  {
    typ: { kind: 'TRat' } as Typ,
    name: 'RatEditor',
    older: rv({ kind: 'Decimal', value: 153 }),
    olderShown: '153',
  },
  {
    typ: { kind: 'TMoney' } as Typ,
    name: 'MoneyEditor',
    older: moneyVal(15300),
    olderShown: '153.00',
  },
])(
  '$name with the value echoed by the parent',
  ({ typ, older, olderShown }) => {
    it('keeps the text while typing 153.01', () => {
      render(<Harness io={{ typ }} />);
      const input = screen.getByRole('textbox') as HTMLInputElement;
      for (const typed of ['1', '15', '153', '153.', '153.0', '153.01']) {
        type(input, typed);
        expect(input.value).toBe(typed);
      }
    });

    it('takes a different value from the parent', () => {
      render(<Harness io={{ typ }} />);
      const input = screen.getByRole('textbox') as HTMLInputElement;
      type(input, '153.01');
      // An older value, as an undo delivers it.
      act(() => deliver({ typ, value: { value: older } }));
      expect(input.value).toBe(olderShown);
    });
  }
);

describe('MoneyEditor canonical form', () => {
  it.each([
    ['12.', '12.00'],
    ['3', '3.00'],
    ['12.1', '12.10'],
    ['12.10', '12.10'],
    ['-3.5', '-3.50'],
  ])('formats %s as %s on blur', (typed, shown) => {
    render(<Harness io={{ typ: { kind: 'TMoney' } }} />);
    const input = screen.getByRole('textbox') as HTMLInputElement;
    type(input, typed);
    expect(input.value).toBe(typed);
    fireEvent.blur(input);
    expect(input.value).toBe(shown);
  });

  it('shows 1210 cents as 12.10', () => {
    render(
      <Harness
        io={{ typ: { kind: 'TMoney' }, value: { value: moneyVal(1210) } }}
      />
    );
    expect((screen.getByRole('textbox') as HTMLInputElement).value).toBe(
      '12.10'
    );
  });

  it('commits a trailing separator as whole units', () => {
    render(<Harness io={{ typ: { kind: 'TMoney' } }} />);
    const input = screen.getByRole('textbox') as HTMLInputElement;
    type(input, '12.');
    expect(input.closest('.value-editor')).not.toHaveClass('invalid');
    expect(input.closest('.value-editor')).not.toHaveClass('unset');
  });
});
