/**
 * Regression test for nested struct initialization bug.
 *
 * Bug: When editing a nested field on a new (Unset) item, setNestedValue
 * creates an empty Map for the nested struct instead of initializing all fields.
 * This causes the OCaml backend to crash with "List.combine" error because
 * the struct has fewer fields than the declaration expects.
 */
import { describe, it, expect } from 'vitest';
import type {
  RuntimeValue,
  StructDeclaration,
  Typ,
} from '../../src/generated/catala_types';
import {
  setNestedValue,
  updateOrConstructStruct,
} from '../../src/editors/tableArrayUtils';
import { getDefaultValue } from '../../src/editors/ValueEditors';

describe('Nested struct initialization', () => {
  // Setup: Person struct with nested Address struct
  const addressStructDecl: StructDeclaration = {
    struct_name: 'Address',
    fields: new Map<string, Typ>([
      ['street', { kind: 'TLit', value: 'TString' }],
      ['city', { kind: 'TLit', value: 'TString' }],
      ['zip', { kind: 'TLit', value: 'TString' }],
    ]),
  };

  const personStructDecl: StructDeclaration = {
    struct_name: 'Person',
    fields: new Map<string, Typ>([
      ['name', { kind: 'TLit', value: 'TString' }],
      ['age', { kind: 'TLit', value: 'TInt' }],
      ['address', { kind: 'TStruct', value: addressStructDecl }],
    ]),
  };

  it('should initialize all nested struct fields when editing a nested field on Unset item', () => {
    // Start with an Unset item (what getDefaultValue returns for structs)
    const unsetItem: RuntimeValue = { value: { kind: 'Unset' }, attrs: [] };

    // Edit nested field: address.city
    const newCityValue: RuntimeValue = {
      value: { kind: 'String', value: 'Paris' },
      attrs: [],
    };

    const result = updateOrConstructStruct(
      unsetItem,
      personStructDecl,
      ['address', 'city'],
      newCityValue
    );

    // Result should be a struct
    expect(result.value.kind).toBe('Struct');
    if (result.value.kind !== 'Struct') return;

    const [, personFields] = result.value.value;

    // Person should have all 3 fields
    expect(personFields.size).toBe(3);
    expect(personFields.has('name')).toBe(true);
    expect(personFields.has('age')).toBe(true);
    expect(personFields.has('address')).toBe(true);

    // Address should be a struct with all 3 fields
    const addressValue = personFields.get('address');
    expect(addressValue?.value.kind).toBe('Struct');
    if (addressValue?.value.kind !== 'Struct') return;

    const [, addressFields] = addressValue.value.value;

    // BUG: Currently only 'city' is present, 'street' and 'zip' are missing
    expect(addressFields.size).toBe(3); // This should fail with current code
    expect(addressFields.has('street')).toBe(true);
    expect(addressFields.has('city')).toBe(true);
    expect(addressFields.has('zip')).toBe(true);

    // Verify the edited field has the correct value
    const cityValue = addressFields.get('city');
    expect(cityValue?.value.kind).toBe('String');
    if (cityValue?.value.kind === 'String') {
      expect(cityValue.value.value).toBe('Paris');
    }
  });

  it('should initialize all nested struct fields when using setNestedValue directly', () => {
    // Start with a Person struct where address is Unset
    const personFields = new Map<string, RuntimeValue>([
      ['name', { value: { kind: 'Unset' }, attrs: [] }],
      ['age', { value: { kind: 'Unset' }, attrs: [] }],
      ['address', { value: { kind: 'Unset' }, attrs: [] }],
    ]);

    const newCityValue: RuntimeValue = {
      value: { kind: 'String', value: 'Paris' },
      attrs: [],
    };

    const result = setNestedValue(
      personFields,
      ['address', 'city'],
      newCityValue,
      personStructDecl
    );

    // Address should now be a struct
    const addressValue = result.get('address');
    expect(addressValue?.value.kind).toBe('Struct');
    if (addressValue?.value.kind !== 'Struct') return;

    const [, addressFields] = addressValue.value.value;

    // BUG: Currently only 'city' is present
    expect(addressFields.size).toBe(3); // This should fail
    expect(addressFields.has('street')).toBe(true);
    expect(addressFields.has('city')).toBe(true);
    expect(addressFields.has('zip')).toBe(true);
  });
});
