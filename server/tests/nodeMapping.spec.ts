import { parse } from '../src/catala/parser';
import {
  CatalaGrammarTypes,
  CatalaSyntaxNode,
} from '../src/shared/CatalaTypes';
import { arrayAlreadyIncludesArray } from '../src/shared/arrayUtils';
import { readCatalaFile } from './test-helpers/readCatalaTestFile';

describe('Node Mapping', () => {
  it('Node Mapping', async () => {
    const content = readCatalaFile('allSyntax');

    const result = await parse(content).catch((err) => console.error(err));
    const { tree, ...parsed } = result as any;

    const map: Record<string, CatalaGrammarTypes[][]> = {};
    Object.entries(parsed).forEach(([key, value]) => {
      map[key] = [];
      parsed[key].forEach((node: CatalaSyntaxNode) => {
        if (arrayAlreadyIncludesArray(map[key], node.ascendantTypesPath))
          return;

        map[key].push(node.ascendantTypesPath);
      });
    });

    expect(map).toMatchSnapshot();
  });
});
