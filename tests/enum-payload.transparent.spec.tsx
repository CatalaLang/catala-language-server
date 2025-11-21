import { render, screen, within } from '@testing-library/react';
import { describe, it, expect } from 'vitest';
import { IntlProvider } from 'react-intl';
import enMessages from '../src/locales/en.json';
import TestOutputsEditor from '../src/TestOutputsEditor';
import type { Diff, PathSegment, Typ } from '../src/generated/test_case';
import { tStruct, tEnum, tRat, io, structValue, enumValue } from './helpers';

function seg(kind: PathSegment['kind'], value: any): PathSegment {
  return { kind, value } as any;
}

describe('Enum payload highlight without EnumPayload segment in diff path', () => {
  it('highlights payload field diffs even if UI recurses into Enum', () => {
    const payloadFields = new Map<string, Typ>([['ratio', tRat()]]);
    const payloadTyp = tStruct('X.Payload', payloadFields);
    const enumTyp = tEnum(
      'X.MyEnum',
      new Map([['VariantA', { value: payloadTyp }]])
    );

    const outerFields = new Map<string, Typ>([['enum_field', enumTyp]]);
    const outerTyp = tStruct('X.Outer', outerFields);
    const outputName = 'simple_output';

    const payloadDecl = (payloadTyp as Extract<Typ, { kind: 'TStruct' }>).value;
    const payloadRv = structValue(
      payloadDecl,
      new Map([
        ['ratio', { value: { kind: 'Decimal', value: 0.26 }, attrs: [] }],
      ])
    );
    const enumDecl = (enumTyp as Extract<Typ, { kind: 'TEnum' }>).value;
    const enumRv = enumValue(enumDecl, 'VariantA', payloadRv);
    const outerDecl = (outerTyp as Extract<Typ, { kind: 'TStruct' }>).value;
    const expectedOuterRv = structValue(
      outerDecl,
      new Map([['enum_field', enumRv]])
    );

    const test = {
      testing_scope: 'dummy',
      tested_scope: {
        name: 'S',
        module_name: 'M',
        inputs: new Map(),
        outputs: new Map([[outputName, outerTyp]]),
        module_deps: [],
      },
      test_inputs: new Map(),
      test_outputs: new Map([[outputName, io(outerTyp, expectedOuterRv)]]),
      description: '',
      title: '',
    };

    const diffs: Diff[] = [
      {
        path: [
          seg('StructField', outputName),
          seg('StructField', 'enum_field'),
          seg('StructField', 'ratio'),
        ],
        expected: { value: { kind: 'Decimal', value: 0.26 }, attrs: [] },
        actual: { value: { kind: 'Decimal', value: 0.25 }, attrs: [] },
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
      .getByText('ratio')
      .closest('.simple-item-vertical, .simple-item');
    expect(fieldItem).toBeTruthy();

    const highlighted = (fieldItem as Element).querySelector(
      '.diff-highlight.atomic-diff'
    );
    expect(highlighted).toBeTruthy();
  });
});
