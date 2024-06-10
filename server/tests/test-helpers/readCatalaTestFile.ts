import { readFileSync } from 'fs';
import * as path from 'path';

export const TEST_FILE_NAMES = [
  'allSyntax',
  'cli',
  'combination_1structDecl_1scopeDecl',
  'combination_1structDecl_1scopeDecl_1scope',
  'combination_1structDecl_2scopeDecl',
  'definition_fieldName',
  'definition_fieldName_variable',
  'scope_and_definition',
  'scope_decl',
  'struct_decl',
  'struct_decl_1field',
] as const;
export type FixtureFiles = typeof TEST_FILE_NAMES[number];

export function readCatalaFile(fileName: FixtureFiles) {
  let filePath = `../fixtures/${fileName}.catala_en`;
  filePath = path.resolve(__dirname, filePath);
  const result = readFileSync(filePath, 'utf-8');
  return result;
}
