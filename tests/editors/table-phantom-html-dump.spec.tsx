import { describe, it, vi } from 'vitest';
import { render } from '@testing-library/react';
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

describe('TableArrayEditor HTML dump for debugging', () => {
  const personStruct: StructDeclaration = {
    struct_name: 'Person',
    fields: new Map([
      ['name', { kind: 'TInt' }],
      ['age', { kind: 'TInt' }],
    ]),
  };

  it('dumps HTML for phantom row rendering', () => {
    const currentPath: PathSegment[] = [];
    const diffs: Diff[] = [
      {
        path: [{ kind: 'ListIndex', value: 0 }],
        expected: emptyRV(),
        actual: structRV(
          personStruct,
          new Map([
            ['name', intRV(100)],
            ['age', intRV(25)],
          ])
        ),
      },
    ];

    console.log('Test setup:');
    console.log('- currentPath:', JSON.stringify(currentPath));
    console.log('- diffs:', JSON.stringify(diffs, null, 2));
    console.log('- valueDef (empty array):', JSON.stringify(arrayValueDef([])));

    const { container } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <TableArrayEditor
          elementType={{ kind: 'TStruct', value: personStruct }}
          structType={personStruct}
          valueDef={arrayValueDef([])}
          onValueChange={() => {}}
          currentPath={currentPath}
          diffs={diffs}
          editable={true}
        />
      </IntlProvider>
    );

    console.log('\n=== Rendered HTML ===');
    console.log(container.innerHTML);
    console.log('=== End HTML ===\n');

    const phantomRows = container.querySelectorAll('.phantom-row');
    console.log('Number of phantom rows found:', phantomRows.length);

    const allRows = container.querySelectorAll('tbody tr');
    console.log('Total tbody rows:', allRows.length);
  });
});
