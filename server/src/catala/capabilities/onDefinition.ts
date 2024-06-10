import { LocationLink, Position, Range } from 'vscode-languageserver';
import {
  CatalaFile,
  CatalaGrammarTypes,
  CatalaSyntaxNode,
} from '../../shared/CatalaTypes';
import { SyntaxNode } from 'tree-sitter';
import { pathToFileURL } from 'url';
import { intersectArrays, mergeArrays } from '../../shared/arrayUtils';
import { convertTreeSitterNodePositionToLocationLink } from '../../shared/vscodeUtils';

type CompoundMap = `${CatalaGrammarTypes}+${CatalaGrammarTypes}`;

export function onDefinition(targetFile: CatalaFile, position: Position) {
  const nodeUnderCursor = targetFile.tree.rootNode.descendantForPosition({
    row: position.line,
    column: position.character,
  });

  console.log('[INFO] Node: ', nodeUnderCursor);

  const newResults = filterByPositionInTree(targetFile, nodeUnderCursor);
  const result = newResults.map((node) =>
    convertTreeSitterNodePositionToLocationLink(node, targetFile)
  );
  return result;
}

/**
 * declaration scope ABC
 *     input xyz content money
 */
function filterByPositionInTree(
  targetFile: CatalaFile,
  node: SyntaxNode
): SyntaxNode[] {
  const type = node.type as CatalaGrammarTypes;
  const parsedNodesByType = (targetFile as any)[type] as CatalaSyntaxNode[];
  if (!parsedNodesByType) return [];

  const currentNodeFromParser = parsedNodesByType.find((n) => n.id === node.id);
  if (!currentNodeFromParser) return [];
  /* prettier-ignore */ console.log('>>>> _ >>>> ~ file: onDefinition.ts ~ line 43 ~ currentNodeFromParser.ascendantTypesPath', currentNodeFromParser.ascendantTypesPath);

  // same text
  const onlySameText = parsedNodesByType.filter((n) => n.text === node.text);
  // without self
  const withoutSelf = onlySameText.filter((n) => n.id !== node.id);
  const log = withoutSelf.map((n) => n.ascendantTypesPath);
  /* prettier-ignore */ console.log('>>>> _ >>>> ~ file: onDefinition.ts ~ line 48 ~ log\n', log);

  /**
   * To know which node type should go where
   */
  // const test: CompoundMap = 'scope+definition'
  const nodeDefinitionMapping: Record<string, CatalaGrammarTypes[]> = {
    enum_struct_name: ['struct_decl'],
    field_name: ['struct_decl'],
    scope: ['scope_decl', 'struct_decl'],
    scope_decl: ['scope', 'struct_decl'],
    scope_name: ['scope', 'scope_decl'],
    struct_decl: ['enum_struct_name', 'struct_decl_item'],
    struct_decl_item: ['enum_struct_name'],
    variable: ['scope', 'scope_decl'],
    verb_block: ['struct_decl'],
    '': [] as CatalaGrammarTypes[],
  };

  // opposite type, eg. scope will give definitions for declaration structure, and not other scope
  const mappedByFarthest = withoutSelf.filter((maybeNode) => {
    const current = currentNodeFromParser.ascendantTypesPath;
    /* prettier-ignore */ console.log('>>>> _ >>>> ~ file: onDefinition.ts ~ line 69 ~ current', current);
    if (!current) return false;
    if (current.length === 0) return false;
    const farthest = [...current].pop();
    const mapToCurrent = nodeDefinitionMapping[node.type ?? ''];
    /* prettier-ignore */ console.log('mapToCurrent', mapToCurrent);
    const mapToFarthest = nodeDefinitionMapping[farthest ?? ''];
    /* prettier-ignore */ console.log('mapToFarthest', mapToFarthest);
    const mapTo = intersectArrays(mapToFarthest, mapToCurrent);
    /* prettier-ignore */ console.log('mapTo', mapTo);

    const okay = mapTo?.find((mappingElement) =>
      maybeNode.ascendantTypesPath.includes(mappingElement)
    );
    return okay;
    // return n.enclosingBlockType !== currentNodeFromParser.enclosingBlockType;
  });

  return mappedByFarthest;

  // nodesByType = nodesByType.filter((n) => {
  //   if (!n.enclosingBlockType) return;
  //   const mapTo: CatalaGrammarTypes[] = (nodeDefinitionMapping as any)[
  //     currentNodeFromParser?.enclosingBlockType ?? ''
  //   ];
  //   const isAll = mapTo.includes('ALL');
  //   if (isAll) return true;

  //   const okay = mapTo.includes(n.enclosingBlockType);
  //   return okay;
  // });

  // switch (type) {
  //   case 'enum_struct_name': {
  //     break;
  //   }
  //   default: {
  //     console.log('unknown type', node.type);
  //   }
  // }

  // const result = nodesByType;
  // return result;
}
