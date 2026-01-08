import { describe, it, vi } from 'vitest';
import { render, within } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../../src/locales/en.json';
import type {
  Diff,
  PathSegment,
  RuntimeValue,
  RuntimeValueRaw,
  StructDeclaration,
  ValueDef,
} from '../../src/generated/catala_types';
import { TableArrayEditor } from '../../src/editors/TableArrayEditor';

// Mock confirm
vi.mock('../../src/messaging/confirm', () => ({
  confirm: async () => true,
}));

function rv(value: RuntimeValueRaw): RuntimeValue {
  return { value, attrs: [] };
}

function arrayValueDef(items: RuntimeValue[]): ValueDef {
  return { value: rv({ kind: 'Array', value: items }) };
}

function intRV(n: number): RuntimeValue {
  return rv({ kind: 'Integer', value: n });
}

function emptyRV(): RuntimeValue {
  return rv({ kind: 'Empty' });
}

function structRV(
  structDecl: StructDeclaration,
  fields: Map<string, RuntimeValue>
): RuntimeValue {
  return rv({
    kind: 'Struct',
    value: [structDecl, fields],
  });
}

describe('Expected-only row debugging', () => {
  const personStruct: StructDeclaration = {
    struct_name: 'Person',
    fields: new Map([
      ['name', { kind: 'TInt' }],
      ['age', { kind: 'TInt' }],
    ]),
  };

  it('dumps buttons in expected-only row', () => {
    const currentPath: PathSegment[] = [];
    const person1 = structRV(
      personStruct,
      new Map([
        ['name', intRV(1)],
        ['age', intRV(30)],
      ])
    );

    const diffs: Diff[] = [
      {
        path: [{ kind: 'ListIndex', value: 0 }],
        expected: person1,
        actual: emptyRV(),
      },
    ];

    const { container } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <TableArrayEditor
          elementType={{ kind: 'TStruct', value: personStruct }}
          structType={personStruct}
          valueDef={arrayValueDef([person1])}
          onValueChange={() => {}}
          currentPath={currentPath}
          diffs={diffs}
          editable={true}
        />
      </IntlProvider>
    );

    const expectedOnlyRow = container.querySelector('.expected-only-row');
    console.log('Expected-only row found:', !!expectedOnlyRow);

    if (expectedOnlyRow) {
      const expectedOnlyRowElement = expectedOnlyRow as HTMLElement;
      const allButtons = within(expectedOnlyRowElement).queryAllByRole(
        'button'
      );
      console.log('Total buttons:', allButtons.length);

      allButtons.forEach((btn, idx) => {
        console.log(`Button ${idx}:`, {
          title: btn.getAttribute('title'),
          className: btn.className,
          textContent: btn.textContent,
        });
      });

      const moveButtons = within(expectedOnlyRowElement).queryAllByRole(
        'button',
        {
          name: /move/i,
        }
      );
      console.log('Move buttons found:', moveButtons.length);
      moveButtons.forEach((btn, idx) => {
        console.log(`Move button ${idx}:`, btn.getAttribute('title'));
      });
    }
  });
});
