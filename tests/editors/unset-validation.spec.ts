/**
 * Tests for hasUnsetInTest: recursive Unset detection across RuntimeValue trees.
 */

import { describe, it, expect } from 'vitest';
import type {
  RuntimeValue,
  Test,
  Typ,
  StructDeclaration,
  EnumDeclaration,
  Option,
  TestIo,
} from '../../src/generated/test_case';
import { hasUnsetInTest } from '../../src/editors/unsetValidation';
import { rv, io } from '../helpers';

// ============================================================================
// Helpers
// ============================================================================

function testIo(value: RuntimeValue): TestIo {
  return io({ kind: 'TInt' }, value);
}

function makeTest(
  inputs: Map<string, TestIo>,
  outputs: Map<string, TestIo>
): Test {
  return {
    testing_scope: 'TestScope',
    tested_scope: {
      name: 'Scope',
      module_name: 'Module',
      inputs: new Map(),
      outputs: new Map(),
      module_deps: [],
    },
    test_inputs: inputs,
    test_outputs: outputs,
    description: '',
    title: 'test',
  };
}

// ============================================================================
// containsUnset (tested indirectly through hasUnsetInTest)
// ============================================================================

describe('hasUnsetInTest', () => {
  describe('Unset detection in value trees', () => {
    it('detects a direct Unset value in inputs', () => {
      const test = makeTest(
        new Map([['x', testIo(rv({ kind: 'Unset' }))]]),
        new Map()
      );
      expect(hasUnsetInTest(test)).toBe(true);
    });

    it('returns false when all values are set', () => {
      const test = makeTest(
        new Map([['x', testIo(rv({ kind: 'Integer', value: 42 }))]]),
        new Map([['y', testIo(rv({ kind: 'Integer', value: 1 }))]])
      );
      expect(hasUnsetInTest(test)).toBe(false);
    });

    it('detects Unset nested inside an Array', () => {
      const test = makeTest(
        new Map([
          [
            'arr',
            testIo(
              rv({
                kind: 'Array',
                value: [
                  rv({ kind: 'Integer', value: 1 }),
                  rv({ kind: 'Unset' }),
                ],
              })
            ),
          ],
        ]),
        new Map()
      );
      expect(hasUnsetInTest(test)).toBe(true);
    });

    it('detects Unset nested inside a Struct', () => {
      const structDecl: StructDeclaration = {
        struct_name: 'S',
        fields: new Map<string, Typ>([
          ['a', { kind: 'TInt' }],
          ['b', { kind: 'TInt' }],
        ]),
      };
      const test = makeTest(
        new Map([
          [
            's',
            testIo(
              rv({
                kind: 'Struct',
                value: [
                  structDecl,
                  new Map<string, RuntimeValue>([
                    ['a', rv({ kind: 'Integer', value: 1 })],
                    ['b', rv({ kind: 'Unset' })],
                  ]),
                ],
              })
            ),
          ],
        ]),
        new Map()
      );
      expect(hasUnsetInTest(test)).toBe(true);
    });

    it('detects Unset inside an Enum payload', () => {
      const enumDecl: EnumDeclaration = {
        enum_name: 'E',
        constructors: new Map<string, Option<Typ>>([
          ['Some', { value: { kind: 'TInt' } }],
        ]),
      };
      const test = makeTest(
        new Map([
          [
            'e',
            testIo(
              rv({
                kind: 'Enum',
                value: [enumDecl, ['Some', { value: rv({ kind: 'Unset' }) }]],
              })
            ),
          ],
        ]),
        new Map()
      );
      expect(hasUnsetInTest(test)).toBe(true);
    });

    it('returns false for Enum with no payload', () => {
      const enumDecl: EnumDeclaration = {
        enum_name: 'E',
        constructors: new Map<string, Option<Typ>>([['None', null]]),
      };
      const test = makeTest(
        new Map([
          [
            'e',
            testIo(
              rv({
                kind: 'Enum',
                value: [enumDecl, ['None', null]],
              })
            ),
          ],
        ]),
        new Map()
      );
      expect(hasUnsetInTest(test)).toBe(false);
    });
  });

  describe('checkInputs / checkOutputs flags', () => {
    const unsetIo = new Map([['x', testIo(rv({ kind: 'Unset' }))]]);
    const cleanIo = new Map([['y', testIo(rv({ kind: 'Integer', value: 1 }))]]);

    it('checks only inputs when checkOutputs is false', () => {
      const test = makeTest(cleanIo, unsetIo);
      expect(hasUnsetInTest(test, { checkOutputs: false })).toBe(false);
    });

    it('checks only outputs when checkInputs is false', () => {
      const test = makeTest(unsetIo, cleanIo);
      expect(hasUnsetInTest(test, { checkInputs: false })).toBe(false);
    });
  });
});
