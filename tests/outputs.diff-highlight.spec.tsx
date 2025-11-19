import { render, screen, within } from '@testing-library/react';
import { describe, it, expect } from 'vitest';
import { IntlProvider } from 'react-intl';
import enMessages from '../src/locales/en.json';
import TestOutputsEditor from '../src/TestOutputsEditor';
import type { Diff, PathSegment, Typ } from '../src/generated/test_case';
import { tStruct, tRat, io, structValue } from './helpers';

function seg(kind: PathSegment['kind'], value: any): PathSegment {
  return { kind, value } as any;
}

describe('TestOutputsEditor diff highlighting (struct field)', () => {
  it('wraps the field editor with atomic diff when there is a matching diff', () => {
    const fields = new Map<string, Typ>([['surface_hectares', tRat()]]);
    const typ = tStruct('X.Struct', fields);
    const outputName = 'rotation_cultures_critère_pluriannuel';

    const structDecl = (typ as Extract<Typ, { kind: 'TStruct' }>).value;
    const expectedOuterRv = structValue(
      structDecl,
      new Map([
        [
          'surface_hectares',
          { value: { kind: 'Decimal', value: 15.12 }, attrs: [] },
        ],
      ])
    );

    const test = {
      testing_scope: 'dummy',
      tested_scope: {
        name: 'S',
        module_name: 'M',
        inputs: new Map(),
        outputs: new Map([[outputName, typ]]),
        module_deps: [],
      },
      test_inputs: new Map(),
      test_outputs: new Map([[outputName, io(typ, expectedOuterRv)]]),
      description: '',
      title: '',
    };

    const diffs: Diff[] = [
      {
        path: [
          seg('StructField', outputName),
          seg('StructField', 'surface_hectares'),
        ],
        expected: { value: { kind: 'Decimal', value: 15.12 }, attrs: [] },
        actual: { value: { kind: 'Decimal', value: 15.1 }, attrs: [] },
      },
    ];

    render(
      <IntlProvider
        locale="en"
        messages={enMessages}
        onError={(err) => {
          if ((err as any)?.code === 'MISSING_TRANSLATION') return;
          console.error(err);
        }}
      >
        <TestOutputsEditor
          test={test as any}
          onTestChange={() => {}}
          diffs={diffs}
        />
      </IntlProvider>
    );

    const row = screen
      .getByText(outputName)
      .closest('.test-output-row')! as HTMLElement;
    const fieldItem = within(row)
      .getByText('surface_hectares')
      .closest('.simple-item-vertical, .simple-item');
    expect(fieldItem).toBeTruthy();

    const highlighted = (fieldItem as Element).querySelector(
      '.diff-highlight.atomic-diff'
    );
    expect(highlighted).toBeTruthy();
  });
});
