import * as fs from 'fs';
import * as path from 'path';
import * as Parser from 'tree-sitter';
// import * as Catala from '/home/user/dev/projects/tree-sitter/tree-sitter-catala/bindings/node/index.js';
import * as Catala from '../catala/tree-sitter/bindings/node';
import {
  CatalaFileParsed,
  CatalaGrammarTypes,
  CatalaSyntaxNode,
} from '../shared/CatalaTypes';
import {
  getAscendantNodeTypesPath,
  getAscendantTypesUntilSourceFile,
  getClosestAscendantOfTypes,
  getTypeFromClosestAscendantOfTypes,
} from './tree-sitter-focus/tree-sitter-modules';
import {
  areArraysEqual,
  arrayAlreadyIncludesArray,
} from '../shared/arrayUtils';

export async function parse(text: string): Promise<CatalaFileParsed> {
  const parser = new Parser();
  parser.setLanguage(Catala);
  const tree = parser.parse(text);
  const catalaFile: CatalaFileParsed = {
    error: [],
    constructor_name: [],
    enum_struct_name: [],
    field_name: [],
    scope_name: [],
    variable: [],
    verb_block: [],
    tree,
  };
  walkRecursive(tree.rootNode as CatalaSyntaxNode, catalaFile);
  return catalaFile;
}

function walkRecursive(
  node: CatalaSyntaxNode | undefined,
  catalaFile: CatalaFileParsed
) {
  if (!node) return;
  const catalaSyntaxNode: CatalaSyntaxNode = Object.assign(node, {
    enclosingBlockType: 'ALL',
  });

  // ['enum_struct_name', 'scope_decl_item', 'scope_decl']
  if (node.type === 'ERROR') {
    catalaFile.error.push(catalaSyntaxNode);
  }

  if (node.type === 'constructor_name') {
    const collectedTypes = [];
    getAscendantTypesUntilSourceFile(node, collectedTypes);
    if (collectedTypes.length > 2) {
      /* prettier-ignore */ console.log('%c------------------------------------------------------------------------------------------', `background: ${'darkblue'}`);
      node.type; /*?*/
      collectedTypes; /*?*/
    }

    const parentTypes: CatalaGrammarTypes[] = ['enum_decl_item'];
    const grandParentTypes: CatalaGrammarTypes[] = ['enum_decl'];
    const ascendantTypesPath = getAscendantNodeTypesPath(
      node,
      parentTypes,
      grandParentTypes
    );
    catalaSyntaxNode.ascendantTypesPath = ascendantTypesPath;
    catalaFile.constructor_name.push(catalaSyntaxNode);
  } else if (node.type === 'enum_struct_name') {
    const collectedTypes = [];
    getAscendantTypesUntilSourceFile(node, collectedTypes);
    if (collectedTypes.length > 2) {
      /* prettier-ignore */ console.log('%c------------------------------------------------------------------------------------------', `background: ${'darkblue'}`);
      node.type; /*?*/
      collectedTypes; /*?*/
    }

    const parentTypes: CatalaGrammarTypes[] = [
      'definition',
      'enum_decl',
      'struct_decl',
      'scope_decl_item',
    ];
    const grandParentTypes: CatalaGrammarTypes[] = ['scope', 'scope_decl'];
    const ascendantTypesPath = getAscendantNodeTypesPath(
      node,
      parentTypes,
      grandParentTypes
    );
    catalaSyntaxNode.ascendantTypesPath = ascendantTypesPath;
    catalaFile.enum_struct_name.push(catalaSyntaxNode);
  } else if (node.type === 'field_name') {
    const collectedTypes = [];
    getAscendantTypesUntilSourceFile(node, collectedTypes);
    if (collectedTypes.length > 2) {
      /* prettier-ignore */ console.log('%c------------------------------------------------------------------------------------------', `background: ${'darkblue'}`);
      node.type; /*?*/
      collectedTypes; /*?*/
    }

    const parentTypes: CatalaGrammarTypes[] = [
      'definition',
      'struct_decl',
      'struct_decl_item',
    ];
    const grandParentTypes: CatalaGrammarTypes[] = [
      'scope',
      'scope_decl',
      'struct_decl',
    ];
    const ascendantTypesPath = getAscendantNodeTypesPath(
      node,
      parentTypes,
      grandParentTypes
    );
    catalaSyntaxNode.ascendantTypesPath = ascendantTypesPath;
    catalaFile.field_name.push(catalaSyntaxNode);
  } else if (node.type === 'scope_name') {
    const collectedTypes = [];
    getAscendantTypesUntilSourceFile(node, collectedTypes);
    if (collectedTypes.length > 2) {
      /* prettier-ignore */ console.log('%c------------------------------------------------------------------------------------------', `background: ${'darkblue'}`);
      node.type; /*?*/
      collectedTypes; /*?*/
    }

    const parentTypes: CatalaGrammarTypes[] = [
      'scope_decl_item',
      'scope',
      'scope_decl',
    ];
    const grandParentTypes: CatalaGrammarTypes[] = ['scope', 'scope_decl'];
    const ascendantTypesPath = getAscendantNodeTypesPath(
      node,
      parentTypes,
      grandParentTypes
    );
    catalaSyntaxNode.ascendantTypesPath = ascendantTypesPath;
    catalaFile.scope_name.push(catalaSyntaxNode);
  } else if (node.type === 'variable') {
    const collectedTypes = [];
    getAscendantTypesUntilSourceFile(node, collectedTypes);
    /* prettier-ignore */ console.log('%c------------------------------------------------------------------------------------------', `background: ${'darkblue'}`);
    node.type; /*?*/
    collectedTypes; /*?*/

    const parentTypes: CatalaGrammarTypes[] = [
      'assertion',
      'definition',
      'rule',
      'scope_decl_item',
    ];
    const grandParentTypes: CatalaGrammarTypes[] = ['scope', 'scope_decl'];
    const ascendantTypesPath = getAscendantNodeTypesPath(
      node,
      parentTypes,
      grandParentTypes
    );

    catalaSyntaxNode.ascendantTypesPath = ascendantTypesPath;
    catalaFile.variable.push(catalaSyntaxNode);
  } else if (node.type === 'verb_block') {
    const collectedTypes = [];
    getAscendantTypesUntilSourceFile(node, collectedTypes);
    if (collectedTypes.length > 2) {
      /* prettier-ignore */ console.log('%c------------------------------------------------------------------------------------------', `background: ${'darkblue'}`);
      node.type; /*?*/
      collectedTypes; /*?*/
    }

    const parentTypes: CatalaGrammarTypes[] = ['definition', 'scope_decl_item'];
    const grandParentTypes: CatalaGrammarTypes[] = ['scope', 'scope_decl'];
    const ascendantTypesPath = getAscendantNodeTypesPath(
      node,
      parentTypes,
      grandParentTypes
    );

    catalaSyntaxNode.ascendantTypesPath = ascendantTypesPath;
    catalaFile.variable.push(catalaSyntaxNode);
  }

  if (node.childCount === 0) return;

  node.children.forEach((child) => {
    walkRecursive(child, catalaFile);
  });
}

const content = fs.readFileSync(
  path.resolve(__dirname, '../..', 'tests/fixtures/cli.catala_en'),
  'utf-8'
);

async function main() {
  const result = await parse(content).catch((err) => console.error(err));
  const { tree, ...parsed } = result as any;

  const map: Record<string, CatalaGrammarTypes[][]> = {};
  Object.entries(parsed).forEach(([key, value]) => {
    map[key] = [];
    parsed[key].forEach((node: CatalaSyntaxNode) => {
      if (arrayAlreadyIncludesArray(map[key], node.ascendantTypesPath)) return;

      if (node.ascendantTypesPath.length === 0) {
        node.ascendantTypesPath; /*?*/
        node.parent.parent.text; /*?*/
        node.text; /*?*/
      }

      map[key].push(node.ascendantTypesPath);
    });
  });

  map; /*?*/
  // parsed.enum_struct_name[0].ascendantTypesPath; /*?*/
}
main();
