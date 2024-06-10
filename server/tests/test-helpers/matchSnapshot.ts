import {
  CatalaFileParsed,
  CatalaSyntaxNode,
} from '../../src/shared/CatalaTypes';
import { printTree } from '../../src/shared/printUtils';

export function matchSnapshot(content: string, parsedFile: CatalaFileParsed) {
  printTree(parsedFile);
  const props = mapPropsForSnapshot(parsedFile);
  const result = {
    content: '\n' + content,
    props,
  };
  expect(result).toMatchSnapshot();
  expect(true).toBeFalsy();
}

export function mapPropsForSnapshot(parsedFile: CatalaFileParsed) {
  const result: any = {};
  if (parsedFile.enum_struct_name.length > 0)
    result.enum_struct_name = getPropsForSnapshot(parsedFile.enum_struct_name);
  if (parsedFile.field_name.length > 0)
    result.field_name = getPropsForSnapshot(parsedFile.field_name);
  if (parsedFile.scope_name.length > 0)
    result.scope = getPropsForSnapshot(parsedFile.scope_name);
  if (parsedFile.variable.length > 0)
    result.variable = getPropsForSnapshot(parsedFile.variable);
  return result;
}

export function getPropsForSnapshot(nodes: CatalaSyntaxNode[]) {
  const props = nodes.map((node) => {
    const { text, startPosition, endPosition } = node;
    const result = { text, startPosition, endPosition };
    return result;
  });
  return props;
}
