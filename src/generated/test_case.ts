/*
  Generated by atdts from type definitions in 'test_case.atd'.

  Type-safe translations from/to JSON

  For each type 'Foo', there is a pair of functions:
  - 'writeFoo': convert a 'Foo' value into a JSON-compatible value.
  - 'readFoo': convert a JSON-compatible value into a TypeScript value
    of type 'Foo'.
*/

// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-nocheck
/* tslint:disable */
/* eslint-disable */

export type Typ =
| { kind: 'TBool' }
| { kind: 'TInt' }
| { kind: 'TRat' }
| { kind: 'TMoney' }
| { kind: 'TDate' }
| { kind: 'TDuration' }
| { kind: 'TTuple'; value: Typ[] }
| { kind: 'TStruct'; value: StructDeclaration }
| { kind: 'TEnum'; value: EnumDeclaration }
| { kind: 'TOption'; value: Typ }
| { kind: 'TArray'; value: Typ }

export type EnumDeclaration = {
  enum_name: string;
  constructors: Map<string, Option<Typ>>;
}

export type StructDeclaration = {
  struct_name: string;
  fields: Map<string, Typ>;
}

export type ScopeDef = {
  name: string;
  inputs: Map<string, Typ>;
  outputs: Map<string, Typ>;
}

export type SourcePosition = {
  filename: string;
  start_line: number /*int*/;
  start_column: number /*int*/;
  end_line: number /*int*/;
  end_column: number /*int*/;
  law_headings: string[];
}

export type Date = {
  year: number /*int*/;
  month: number /*int*/;
  day: number /*int*/;
}

export type Duration = {
  years: number /*int*/;
  months: number /*int*/;
  days: number /*int*/;
}

export type RuntimeValue =
| { kind: 'Bool'; value: boolean }
| { kind: 'Money'; value: number /*int*/ }
| { kind: 'Integer'; value: number /*int*/ }
| { kind: 'Decimal'; value: number }
| { kind: 'Date'; value: Date }
| { kind: 'Duration'; value: Duration }
| { kind: 'Enum'; value: [EnumDeclaration, [string, RuntimeValue]] }
| { kind: 'Struct'; value: [StructDeclaration, Map<string, RuntimeValue>] }
| { kind: 'Array'; value: RuntimeValue[] }

export type ValueDef = {
  value: RuntimeValue;
  pos?: SourcePosition;
}

export type TestIo = {
  typ: Typ;
  value?: ValueDef;
}

export type TestInputs = Map<string, TestIo>

export type TestOutputs = Map<string, TestIo>

export type Test = {
  testing_scope: string;
  tested_scope: ScopeDef;
  test_inputs: TestInputs;
  test_outputs: TestOutputs;
}

export type TestList = Test[]

export type ParseResults =
| { kind: 'Error'; value: string }
| { kind: 'Results'; value: TestList }

export type TestRunResults =
| { kind: 'Error'; value: string }
| { kind: 'Ok' }

export type TestRunRequest = {
  scope: string;
  uid: string;
}

export type UpMessage =
| { kind: 'Ready' }
| { kind: 'Edit'; value: TestList }
| { kind: 'TestRunRequest'; value: TestRunRequest }

export type DownMessage =
| { kind: 'Update'; value: ParseResults }
| { kind: 'TestRunResults'; value: [string, TestRunResults] }

export function writeTyp(x: Typ, context: any = x): any {
  switch (x.kind) {
    case 'TBool':
      return 'TBool'
    case 'TInt':
      return 'TInt'
    case 'TRat':
      return 'TRat'
    case 'TMoney':
      return 'TMoney'
    case 'TDate':
      return 'TDate'
    case 'TDuration':
      return 'TDuration'
    case 'TTuple':
      return ['TTuple', _atd_write_array(writeTyp)(x.value, x)]
    case 'TStruct':
      return ['TStruct', writeStructDeclaration(x.value, x)]
    case 'TEnum':
      return ['TEnum', writeEnumDeclaration(x.value, x)]
    case 'TOption':
      return ['TOption', writeTyp(x.value, x)]
    case 'TArray':
      return ['TArray', writeTyp(x.value, x)]
  }
}

export function readTyp(x: any, context: any = x): Typ {
  if (typeof x === 'string') {
    switch (x) {
      case 'TBool':
        return { kind: 'TBool' }
      case 'TInt':
        return { kind: 'TInt' }
      case 'TRat':
        return { kind: 'TRat' }
      case 'TMoney':
        return { kind: 'TMoney' }
      case 'TDate':
        return { kind: 'TDate' }
      case 'TDuration':
        return { kind: 'TDuration' }
      default:
        _atd_bad_json('Typ', x, context)
        throw new Error('impossible')
    }
  }
  else {
    _atd_check_json_tuple(2, x, context)
    switch (x[0]) {
      case 'TTuple':
        return { kind: 'TTuple', value: _atd_read_array(readTyp)(x[1], x) }
      case 'TStruct':
        return { kind: 'TStruct', value: readStructDeclaration(x[1], x) }
      case 'TEnum':
        return { kind: 'TEnum', value: readEnumDeclaration(x[1], x) }
      case 'TOption':
        return { kind: 'TOption', value: readTyp(x[1], x) }
      case 'TArray':
        return { kind: 'TArray', value: readTyp(x[1], x) }
      default:
        _atd_bad_json('Typ', x, context)
        throw new Error('impossible')
    }
  }
}

export function writeEnumDeclaration(x: EnumDeclaration, context: any = x): any {
  return {
    'enum_name': _atd_write_required_field('EnumDeclaration', 'enum_name', _atd_write_string, x.enum_name, x),
    'constructors': _atd_write_required_field('EnumDeclaration', 'constructors', _atd_write_assoc_map_to_object(_atd_write_option(writeTyp)), x.constructors, x),
  };
}

export function readEnumDeclaration(x: any, context: any = x): EnumDeclaration {
  return {
    enum_name: _atd_read_required_field('EnumDeclaration', 'enum_name', _atd_read_string, x['enum_name'], x),
    constructors: _atd_read_required_field('EnumDeclaration', 'constructors', _atd_read_assoc_object_into_map(_atd_read_option(readTyp)), x['constructors'], x),
  };
}

export function writeStructDeclaration(x: StructDeclaration, context: any = x): any {
  return {
    'struct_name': _atd_write_required_field('StructDeclaration', 'struct_name', _atd_write_string, x.struct_name, x),
    'fields': _atd_write_required_field('StructDeclaration', 'fields', _atd_write_assoc_map_to_object(writeTyp), x.fields, x),
  };
}

export function readStructDeclaration(x: any, context: any = x): StructDeclaration {
  return {
    struct_name: _atd_read_required_field('StructDeclaration', 'struct_name', _atd_read_string, x['struct_name'], x),
    fields: _atd_read_required_field('StructDeclaration', 'fields', _atd_read_assoc_object_into_map(readTyp), x['fields'], x),
  };
}

export function writeScopeDef(x: ScopeDef, context: any = x): any {
  return {
    'name': _atd_write_required_field('ScopeDef', 'name', _atd_write_string, x.name, x),
    'inputs': _atd_write_required_field('ScopeDef', 'inputs', _atd_write_assoc_map_to_object(writeTyp), x.inputs, x),
    'outputs': _atd_write_required_field('ScopeDef', 'outputs', _atd_write_assoc_map_to_object(writeTyp), x.outputs, x),
  };
}

export function readScopeDef(x: any, context: any = x): ScopeDef {
  return {
    name: _atd_read_required_field('ScopeDef', 'name', _atd_read_string, x['name'], x),
    inputs: _atd_read_required_field('ScopeDef', 'inputs', _atd_read_assoc_object_into_map(readTyp), x['inputs'], x),
    outputs: _atd_read_required_field('ScopeDef', 'outputs', _atd_read_assoc_object_into_map(readTyp), x['outputs'], x),
  };
}

export function writeSourcePosition(x: SourcePosition, context: any = x): any {
  return {
    'filename': _atd_write_required_field('SourcePosition', 'filename', _atd_write_string, x.filename, x),
    'start_line': _atd_write_required_field('SourcePosition', 'start_line', _atd_write_int, x.start_line, x),
    'start_column': _atd_write_required_field('SourcePosition', 'start_column', _atd_write_int, x.start_column, x),
    'end_line': _atd_write_required_field('SourcePosition', 'end_line', _atd_write_int, x.end_line, x),
    'end_column': _atd_write_required_field('SourcePosition', 'end_column', _atd_write_int, x.end_column, x),
    'law_headings': _atd_write_field_with_default(_atd_write_array(_atd_write_string), [], x.law_headings, x),
  };
}

export function readSourcePosition(x: any, context: any = x): SourcePosition {
  return {
    filename: _atd_read_required_field('SourcePosition', 'filename', _atd_read_string, x['filename'], x),
    start_line: _atd_read_required_field('SourcePosition', 'start_line', _atd_read_int, x['start_line'], x),
    start_column: _atd_read_required_field('SourcePosition', 'start_column', _atd_read_int, x['start_column'], x),
    end_line: _atd_read_required_field('SourcePosition', 'end_line', _atd_read_int, x['end_line'], x),
    end_column: _atd_read_required_field('SourcePosition', 'end_column', _atd_read_int, x['end_column'], x),
    law_headings: _atd_read_field_with_default(_atd_read_array(_atd_read_string), [], x['law_headings'], x),
  };
}

export function writeDate(x: Date, context: any = x): any {
  return {
    'year': _atd_write_required_field('Date', 'year', _atd_write_int, x.year, x),
    'month': _atd_write_required_field('Date', 'month', _atd_write_int, x.month, x),
    'day': _atd_write_required_field('Date', 'day', _atd_write_int, x.day, x),
  };
}

export function readDate(x: any, context: any = x): Date {
  return {
    year: _atd_read_required_field('Date', 'year', _atd_read_int, x['year'], x),
    month: _atd_read_required_field('Date', 'month', _atd_read_int, x['month'], x),
    day: _atd_read_required_field('Date', 'day', _atd_read_int, x['day'], x),
  };
}

export function writeDuration(x: Duration, context: any = x): any {
  return {
    'years': _atd_write_required_field('Duration', 'years', _atd_write_int, x.years, x),
    'months': _atd_write_required_field('Duration', 'months', _atd_write_int, x.months, x),
    'days': _atd_write_required_field('Duration', 'days', _atd_write_int, x.days, x),
  };
}

export function readDuration(x: any, context: any = x): Duration {
  return {
    years: _atd_read_required_field('Duration', 'years', _atd_read_int, x['years'], x),
    months: _atd_read_required_field('Duration', 'months', _atd_read_int, x['months'], x),
    days: _atd_read_required_field('Duration', 'days', _atd_read_int, x['days'], x),
  };
}

export function writeRuntimeValue(x: RuntimeValue, context: any = x): any {
  switch (x.kind) {
    case 'Bool':
      return ['Bool', _atd_write_bool(x.value, x)]
    case 'Money':
      return ['Money', _atd_write_int(x.value, x)]
    case 'Integer':
      return ['Integer', _atd_write_int(x.value, x)]
    case 'Decimal':
      return ['Decimal', _atd_write_float(x.value, x)]
    case 'Date':
      return ['Date', writeDate(x.value, x)]
    case 'Duration':
      return ['Duration', writeDuration(x.value, x)]
    case 'Enum':
      return ['Enum', ((x, context) => [writeEnumDeclaration(x[0], x), ((x, context) => [_atd_write_string(x[0], x), writeRuntimeValue(x[1], x)])(x[1], x)])(x.value, x)]
    case 'Struct':
      return ['Struct', ((x, context) => [writeStructDeclaration(x[0], x), _atd_write_assoc_map_to_object(writeRuntimeValue)(x[1], x)])(x.value, x)]
    case 'Array':
      return ['Array', _atd_write_array(writeRuntimeValue)(x.value, x)]
  }
}

export function readRuntimeValue(x: any, context: any = x): RuntimeValue {
  _atd_check_json_tuple(2, x, context)
  switch (x[0]) {
    case 'Bool':
      return { kind: 'Bool', value: _atd_read_bool(x[1], x) }
    case 'Money':
      return { kind: 'Money', value: _atd_read_int(x[1], x) }
    case 'Integer':
      return { kind: 'Integer', value: _atd_read_int(x[1], x) }
    case 'Decimal':
      return { kind: 'Decimal', value: _atd_read_float(x[1], x) }
    case 'Date':
      return { kind: 'Date', value: readDate(x[1], x) }
    case 'Duration':
      return { kind: 'Duration', value: readDuration(x[1], x) }
    case 'Enum':
      return { kind: 'Enum', value: ((x, context): [EnumDeclaration, [string, RuntimeValue]] => { _atd_check_json_tuple(2, x, context); return [readEnumDeclaration(x[0], x), ((x, context): [string, RuntimeValue] => { _atd_check_json_tuple(2, x, context); return [_atd_read_string(x[0], x), readRuntimeValue(x[1], x)] })(x[1], x)] })(x[1], x) }
    case 'Struct':
      return { kind: 'Struct', value: ((x, context): [StructDeclaration, Map<string, RuntimeValue>] => { _atd_check_json_tuple(2, x, context); return [readStructDeclaration(x[0], x), _atd_read_assoc_object_into_map(readRuntimeValue)(x[1], x)] })(x[1], x) }
    case 'Array':
      return { kind: 'Array', value: _atd_read_array(readRuntimeValue)(x[1], x) }
    default:
      _atd_bad_json('RuntimeValue', x, context)
      throw new Error('impossible')
  }
}

export function writeValueDef(x: ValueDef, context: any = x): any {
  return {
    'value': _atd_write_required_field('ValueDef', 'value', writeRuntimeValue, x.value, x),
    'pos': _atd_write_optional_field(writeSourcePosition, x.pos, x),
  };
}

export function readValueDef(x: any, context: any = x): ValueDef {
  return {
    value: _atd_read_required_field('ValueDef', 'value', readRuntimeValue, x['value'], x),
    pos: _atd_read_optional_field(readSourcePosition, x['pos'], x),
  };
}

export function writeTestIo(x: TestIo, context: any = x): any {
  return {
    'typ': _atd_write_required_field('TestIo', 'typ', writeTyp, x.typ, x),
    'value': _atd_write_optional_field(writeValueDef, x.value, x),
  };
}

export function readTestIo(x: any, context: any = x): TestIo {
  return {
    typ: _atd_read_required_field('TestIo', 'typ', readTyp, x['typ'], x),
    value: _atd_read_optional_field(readValueDef, x['value'], x),
  };
}

export function writeTestInputs(x: TestInputs, context: any = x): any {
  return _atd_write_assoc_map_to_object(writeTestIo)(x, context);
}

export function readTestInputs(x: any, context: any = x): TestInputs {
  return _atd_read_assoc_object_into_map(readTestIo)(x, context);
}

export function writeTestOutputs(x: TestOutputs, context: any = x): any {
  return _atd_write_assoc_map_to_object(writeTestIo)(x, context);
}

export function readTestOutputs(x: any, context: any = x): TestOutputs {
  return _atd_read_assoc_object_into_map(readTestIo)(x, context);
}

export function writeTest(x: Test, context: any = x): any {
  return {
    'testing_scope': _atd_write_required_field('Test', 'testing_scope', _atd_write_string, x.testing_scope, x),
    'tested_scope': _atd_write_required_field('Test', 'tested_scope', writeScopeDef, x.tested_scope, x),
    'test_inputs': _atd_write_required_field('Test', 'test_inputs', writeTestInputs, x.test_inputs, x),
    'test_outputs': _atd_write_required_field('Test', 'test_outputs', writeTestOutputs, x.test_outputs, x),
  };
}

export function readTest(x: any, context: any = x): Test {
  return {
    testing_scope: _atd_read_required_field('Test', 'testing_scope', _atd_read_string, x['testing_scope'], x),
    tested_scope: _atd_read_required_field('Test', 'tested_scope', readScopeDef, x['tested_scope'], x),
    test_inputs: _atd_read_required_field('Test', 'test_inputs', readTestInputs, x['test_inputs'], x),
    test_outputs: _atd_read_required_field('Test', 'test_outputs', readTestOutputs, x['test_outputs'], x),
  };
}

export function writeTestList(x: TestList, context: any = x): any {
  return _atd_write_array(writeTest)(x, context);
}

export function readTestList(x: any, context: any = x): TestList {
  return _atd_read_array(readTest)(x, context);
}

export function writeParseResults(x: ParseResults, context: any = x): any {
  switch (x.kind) {
    case 'Error':
      return ['Error', _atd_write_string(x.value, x)]
    case 'Results':
      return ['Results', writeTestList(x.value, x)]
  }
}

export function readParseResults(x: any, context: any = x): ParseResults {
  _atd_check_json_tuple(2, x, context)
  switch (x[0]) {
    case 'Error':
      return { kind: 'Error', value: _atd_read_string(x[1], x) }
    case 'Results':
      return { kind: 'Results', value: readTestList(x[1], x) }
    default:
      _atd_bad_json('ParseResults', x, context)
      throw new Error('impossible')
  }
}

export function writeTestRunResults(x: TestRunResults, context: any = x): any {
  switch (x.kind) {
    case 'Error':
      return ['Error', _atd_write_string(x.value, x)]
    case 'Ok':
      return 'Ok'
  }
}

export function readTestRunResults(x: any, context: any = x): TestRunResults {
  if (typeof x === 'string') {
    switch (x) {
      case 'Ok':
        return { kind: 'Ok' }
      default:
        _atd_bad_json('TestRunResults', x, context)
        throw new Error('impossible')
    }
  }
  else {
    _atd_check_json_tuple(2, x, context)
    switch (x[0]) {
      case 'Error':
        return { kind: 'Error', value: _atd_read_string(x[1], x) }
      default:
        _atd_bad_json('TestRunResults', x, context)
        throw new Error('impossible')
    }
  }
}

export function writeTestRunRequest(x: TestRunRequest, context: any = x): any {
  return {
    'scope': _atd_write_required_field('TestRunRequest', 'scope', _atd_write_string, x.scope, x),
    'uid': _atd_write_required_field('TestRunRequest', 'uid', _atd_write_string, x.uid, x),
  };
}

export function readTestRunRequest(x: any, context: any = x): TestRunRequest {
  return {
    scope: _atd_read_required_field('TestRunRequest', 'scope', _atd_read_string, x['scope'], x),
    uid: _atd_read_required_field('TestRunRequest', 'uid', _atd_read_string, x['uid'], x),
  };
}

export function writeUpMessage(x: UpMessage, context: any = x): any {
  switch (x.kind) {
    case 'Ready':
      return 'Ready'
    case 'Edit':
      return ['Edit', writeTestList(x.value, x)]
    case 'TestRunRequest':
      return ['TestRunRequest', writeTestRunRequest(x.value, x)]
  }
}

export function readUpMessage(x: any, context: any = x): UpMessage {
  if (typeof x === 'string') {
    switch (x) {
      case 'Ready':
        return { kind: 'Ready' }
      default:
        _atd_bad_json('UpMessage', x, context)
        throw new Error('impossible')
    }
  }
  else {
    _atd_check_json_tuple(2, x, context)
    switch (x[0]) {
      case 'Edit':
        return { kind: 'Edit', value: readTestList(x[1], x) }
      case 'TestRunRequest':
        return { kind: 'TestRunRequest', value: readTestRunRequest(x[1], x) }
      default:
        _atd_bad_json('UpMessage', x, context)
        throw new Error('impossible')
    }
  }
}

export function writeDownMessage(x: DownMessage, context: any = x): any {
  switch (x.kind) {
    case 'Update':
      return ['Update', writeParseResults(x.value, x)]
    case 'TestRunResults':
      return ['TestRunResults', ((x, context) => [_atd_write_string(x[0], x), writeTestRunResults(x[1], x)])(x.value, x)]
  }
}

export function readDownMessage(x: any, context: any = x): DownMessage {
  _atd_check_json_tuple(2, x, context)
  switch (x[0]) {
    case 'Update':
      return { kind: 'Update', value: readParseResults(x[1], x) }
    case 'TestRunResults':
      return { kind: 'TestRunResults', value: ((x, context): [string, TestRunResults] => { _atd_check_json_tuple(2, x, context); return [_atd_read_string(x[0], x), readTestRunResults(x[1], x)] })(x[1], x) }
    default:
      _atd_bad_json('DownMessage', x, context)
      throw new Error('impossible')
  }
}


/////////////////////////////////////////////////////////////////////
// Runtime library
/////////////////////////////////////////////////////////////////////

export type Option<T> = null | { value: T }

function _atd_missing_json_field(type_name: string, json_field_name: string) {
    throw new Error(`missing field '${json_field_name}'` +
                    ` in JSON object of type '${type_name}'`)
}

function _atd_missing_ts_field(type_name: string, ts_field_name: string) {
    throw new Error(`missing field '${ts_field_name}'` +
                    ` in TypeScript object of type '${type_name}'`)
}

function _atd_bad_json(expected_type: string, json_value: any, context: any) {
  let value_str = JSON.stringify(json_value)
  if (value_str.length > 200)
    value_str = value_str.substring(0, 200) + '…';

  throw new Error(`incompatible JSON value where` +
                  ` type '${expected_type}' was expected: '${value_str}'.` +
                  ` Occurs in '${JSON.stringify(context)}'.`)
}

function _atd_bad_ts(expected_type: string, ts_value: any, context: any) {
  let value_str = JSON.stringify(ts_value)
  if (value_str.length > 200)
    value_str = value_str.substring(0, 200) + '…';

  throw new Error(`incompatible TypeScript value where` +
                  ` type '${expected_type}' was expected: '${value_str}'.` +
                  ` Occurs in '${JSON.stringify(context)}'.`)
}

function _atd_check_json_tuple(len: number /*int*/, x: any, context: any) {
  if (! Array.isArray(x) || x.length !== len)
    _atd_bad_json('tuple of length ' + len, x, context);
}

function _atd_read_unit(x: any, context: any): null {
  if (x === null)
    return null
  else {
    _atd_bad_json('null', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_bool(x: any, context: any): boolean {
  if (typeof x === 'boolean')
    return x
  else {
    _atd_bad_json('boolean', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_int(x: any, context: any): number /*int*/ {
  if (Number.isInteger(x))
    return x
  else {
    _atd_bad_json('integer', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_float(x: any, context: any): number {
  if (isFinite(x))
    return x
  else {
    _atd_bad_json('number', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_string(x: any, context: any): string {
  if (typeof x === 'string')
    return x
  else {
    _atd_bad_json('string', x, context)
    throw new Error('impossible')
  }
}

function _atd_read_required_field<T>(type_name: string,
                                     field_name: string,
                                     read_elt: (x: any, context: any) => T,
                                     x: any,
                                     context: any): T {
  if (x === undefined) {
    _atd_missing_json_field(type_name, field_name)
    throw new Error('impossible')
  }
  else
    return read_elt(x, context)
}

function _atd_read_optional_field<T>(read_elt: (x: any, context: any) => T,
                                     x: any,
                                     context: any): T {
  if (x === undefined || x === null)
    return x
  else
    return read_elt(x, context)
}

function _atd_read_field_with_default<T>(read_elt: (x: any, context: any) => T,
                                         default_: T,
                                         x: any,
                                         context: any): T {
  if (x === undefined || x === null)
    return default_
  else
    return read_elt(x, context)
}

function _atd_read_option<T>(read_elt: (x: any, context: any) => T):
  (x: any, context: any) => Option<T> {
  function read_option(x: any, context: any): Option<T> {
    if (x === 'None')
      return null
    else {
      _atd_check_json_tuple(2, x, context);
      switch (x[0]) {
        case 'Some':
          return { value: read_elt(x[1], context) }
        default:
          _atd_bad_json('option', x, context)
          throw new Error('impossible')
      }
    }
  }
  return read_option
}

function _atd_read_nullable<T>(read_elt: (x: any, context: any) => T):
  (x: any, context: any) => T | null {
  function read_nullable(x: any, context: any): T | null {
    if (x === null)
      return null
    else
      return read_elt(x, context)
  }
  return read_nullable
}

function _atd_read_array<T>(read_elt: (x: any, context: any) => T):
  (elts: any, context: any) => T[] {
  function read_array(elts: any, context: any): T[] {
    if (Array.isArray(elts))
      return elts.map((x) => read_elt(x, elts))
    else {
      _atd_bad_json('array', elts, context)
      throw new Error('impossible')
    }
  }
  return read_array
}

function _atd_read_assoc_array_into_map<K, V>(
    read_key: (key: any, context: any) => K,
    read_value: (value: any, context: any) => V
  ): (x: any, context: any) => Map<K, V> {
  function read_assoc(elts: any, context: any): Map<K, V> {
    if (Array.isArray(elts)) {
      const res = new Map<K, V>([])
      for (const x of elts) {
        if (Array.isArray(x) && x.length === 2)
          res.set(read_key(x[0], x), read_value(x[1], x))
        else {
          _atd_bad_json('pair', x, elts)
          throw new Error('impossible')
        }
      }
      return res
    }
    else {
      _atd_bad_json('array', elts, context)
      throw new Error('impossible')
    }
  }
  return read_assoc
}

function _atd_read_assoc_object_into_map<T>(
    read_value: (value: any, context: any) => T
  ): (x: any, context: any) => Map<string, T> {
  function read_assoc(elts: any, context: any): Map<string, T> {
    if (typeof elts === 'object') {
      const res = new Map<string, T>([])
      for (const [key, value] of Object.entries(elts))
        res.set(key, read_value(value, elts))
      return res
    }
    else {
      _atd_bad_json('object', elts, context)
      throw new Error('impossible')
    }
  }
  return read_assoc
}

function _atd_read_assoc_object_into_array<T>(
    read_value: (value: any, context: any) => T
  ): (x: any, context: any) => [string, T][] {
  function read_assoc(elts: any, context: any): [string, T][] {
    if (typeof elts === 'object') {
      const res: [string, T][] = []
      for (const [key, value] of Object.entries(elts))
        res.push([key, read_value(value, elts)])
      return res
    }
    else {
      _atd_bad_json('object', elts, context)
      throw new Error('impossible')
    }
  }
  return read_assoc
}

function _atd_write_unit(x: any, context: any) {
  if (x === null)
    return x
  else {
    _atd_bad_ts('null', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_bool(x: any, context: any): boolean {
  if (typeof x === 'boolean')
    return x
  else {
    _atd_bad_ts('boolean', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_int(x: any, context: any): number /*int*/ {
  if (Number.isInteger(x))
    return x
  else {
    _atd_bad_ts('integer', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_float(x: any, context: any): number {
  if (isFinite(x))
    return x
  else {
    _atd_bad_ts('number', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_string(x: any, context: any): string {
  if (typeof x === 'string')
    return x
  else {
    _atd_bad_ts('string', x, context)
    throw new Error('impossible')
  }
}

function _atd_write_option<T>(write_elt: (x: T, context: any) => any):
   (elts: Option<T>, context: any) => any {
  function write_option(x: Option<T>, context: any): any {
    if (x === null)
      return 'None'
    else
      return ['Some', write_elt(x.value, context)]
  }
  return write_option
}

function _atd_write_nullable<T>(write_elt: (x: T, context: any) => any):
  (x: T | null, context: any) => any {
  function write_option(x: T | null, context: any): any {
    if (x === null)
      return null
    else
      return write_elt(x, context)
  }
  return write_option
}

function _atd_write_array<T>(write_elt: (elt: T, context: any) => any):
  (elts: T[], context: any) => any {
  return ((elts: T[], context: any): any =>
    elts.map((x) => write_elt(x, elts))
  )
}

function _atd_write_assoc_map_to_array<K, V>(
    write_key: (key: K, context: any) => any,
    write_value: (value: V, context: any) => any
  ): (elts: Map<K, V>, context: any) => any {
  function write_assoc(elts: Map<K, V>, context: any): any {
    const res: any = []
    elts.forEach((value: V, key: K) =>
      res.push([write_key(key, elts), write_value(value, elts)])
    )
    return res
  }
  return write_assoc
}

function _atd_write_assoc_map_to_object<T>(
    write_value: (value: T, context: any) => any
  ): (elts: Map<string, T>, context: any) => any {
  function write_assoc(elts: Map<string, T>, context: any): any {
    const res: any = {}
    elts.forEach((value: T, key: string) =>
      res[key] = write_value(value, elts)
    )
    return res
  }
  return write_assoc
}

function _atd_write_assoc_array_to_object<T>(
    write_value: (value: T, context: any) => any
  ): (elts: [string, T][], context: any) => any {
  function write_assoc(elts: [string, T][], context: any): any {
    const res: any = {}
    for (const [key, value] of elts)
      res[key] = write_value(value, elts)
    return res
  }
  return write_assoc
}

function _atd_write_required_field<T>(type_name: string,
                                      field_name: string,
                                      write_elt: (x: T, context: any) => any,
                                      x: T,
                                      context: any): any {
  if (x === undefined) {
    _atd_missing_ts_field(type_name, field_name)
    throw new Error('impossible')
  }
  else
    return write_elt(x, context)
}

function _atd_write_optional_field<T>(write_elt: (x: T, context: any) => any,
                                      x: T | undefined,
                                      context: any): any {
  if (x === undefined || x === null)
    return x
  else
    return write_elt(x, context)
}

function _atd_write_field_with_default<T>(
  write_elt: (x: T, context: any) => any,
  default_: T,
  x: T,
  context: any
): T {
  const value = (x === undefined || x === null) ? default_ : x
  return write_elt(value, context)
}

