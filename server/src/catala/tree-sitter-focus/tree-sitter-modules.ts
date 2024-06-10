import { SyntaxNode } from 'tree-sitter';
import { CatalaSyntaxNode, CatalaGrammarTypes } from '../../shared/CatalaTypes';

export function closestAscendantOfType(
  node: SyntaxNode | undefined,
  type: CatalaGrammarTypes
): SyntaxNode | undefined {
  if (!node) return;
  if (node.parent === null) return;

  const target = node.parent.type === type;
  if (target) {
    return node.parent;
  }

  return closestAscendantOfType(node.parent, type);
}

export function getClosestAscendantOfTypes(
  node: SyntaxNode | undefined,
  types: CatalaGrammarTypes[]
): SyntaxNode | undefined {
  if (!node) return;
  if (node.parent === null) return;

  const targetType = types.find((type) => type === node?.parent?.type);
  if (targetType) {
    return node.parent;
  }

  return getClosestAscendantOfTypes(node.parent, types);
}

const maybe = ['param_decl', 'params_decl', 'scope_var'];
const skip = [
  'qfield',
  'qscope',
  'struct_content_field',
  'struct_content_fields',
  'fun_argument',
  'fun_arguments',
  'e_variable',
  'e_struct',
  'e_scope_apply',
  'e_apply',
  'e_fieldaccess',
  'e_ifthenelse',
  'e_binop',
  'tuple_contents',
  'e_tuple',
  'expression',
  'rule_parameters',
  'qenum_struct',
  'primitive_typ',
  'typ',
];
const skips = skip.concat(maybe);
export function getAscendantTypesUntilSourceFile(
  node: SyntaxNode | undefined,
  ascendantTypes: CatalaGrammarTypes[]
): CatalaGrammarTypes[] {
  if (!node) return ascendantTypes;
  if (node.parent === null) return ascendantTypes;
  // if (node.parent.type === 'source_file') return ascendantTypes;
  if (node.parent.type === 'code_block') return ascendantTypes;

  const targetType = node.parent.type as CatalaGrammarTypes;
  if (skips.includes(targetType)) {
    return getAscendantTypesUntilSourceFile(node.parent, ascendantTypes);
  }

  ascendantTypes.push(node.parent.type as CatalaGrammarTypes);
  return getAscendantTypesUntilSourceFile(node.parent, ascendantTypes);
}

export function getTypeFromClosestAscendantOfTypes(
  node: SyntaxNode | undefined,
  types: CatalaGrammarTypes[]
): CatalaGrammarTypes | undefined {
  if (!node) return;
  if (node.parent === null) return;

  const targetType = types.find((type) => type === node?.parent?.type);
  if (targetType) {
    return targetType;
  }

  return getTypeFromClosestAscendantOfTypes(node.parent, types);
}

export function getAscendantNodeTypesPath(
  node: CatalaSyntaxNode | undefined,
  parentTypes: CatalaGrammarTypes[],
  grandParentTypes: CatalaGrammarTypes[]
): CatalaSyntaxNode['ascendantTypesPath'] {
  const result: CatalaSyntaxNode['ascendantTypesPath'] = [];
  const parent = getClosestAscendantOfTypes(node, parentTypes);
  if (parent?.type) result.push(parent.type as CatalaGrammarTypes);
  const grandParent = getClosestAscendantOfTypes(parent, grandParentTypes);
  if (grandParent?.type) result.push(grandParent.type as CatalaGrammarTypes);
  return result as CatalaSyntaxNode['ascendantTypesPath'];
}

function findManyChildrenByType(
  node: CatalaSyntaxNode | undefined,
  type: CatalaGrammarTypes
) {
  if (!node) return [];
  return node.children.filter((child) => child.type === type);
}

function findFirstChildByType(
  node: CatalaSyntaxNode | undefined,
  type: CatalaGrammarTypes
): CatalaSyntaxNode | undefined {
  if (!node) return;
  return node.children.find((child) => child.type === type);
}

function findFirstChildUntilType(
  node: CatalaSyntaxNode | undefined,
  type: CatalaGrammarTypes
): CatalaSyntaxNode | undefined {
  const target = findFirstChildByType(node, type);
  if (!target) return;

  if (target.type === type) {
    return target;
  }

  findFirstChildUntilType(target, type);
}

function findNextSiblingByType(
  node: CatalaSyntaxNode | undefined,
  type: CatalaGrammarTypes
): CatalaSyntaxNode | undefined {
  if (!node) return undefined;

  let sibling = node.nextSibling;
  while (sibling) {
    if (sibling.type === type) {
      return sibling;
    }
    sibling = sibling.nextSibling;
  }

  return undefined; // Return null if no sibling of the given type is found
}
