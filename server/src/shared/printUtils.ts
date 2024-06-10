import { CatalaFileParsed } from './CatalaTypes';

export function printTree(treeInput: CatalaFileParsed): void {
  const { tree, ...rest } = treeInput;
  // console.log(JSON.stringify(rest, null, 2));
  rest; /*?*/
}
