import { SyntaxNode } from 'tree-sitter';
import { pathToFileURL } from 'url';
import { LocationLink, Range, Position } from 'vscode-languageserver';
import { CatalaFile } from './CatalaTypes';

export function convertTreeSitterNodePositionToLocationLink(
  resultNode: SyntaxNode | undefined,
  targetFile: CatalaFile
): LocationLink {
  const range = convertNodeToRange(resultNode);
  const result = LocationLink.create(
    pathToFileURL(targetFile?.uri ?? '').toString(),
    range,
    range
  );
  return result;
}

export function convertNodeToRange(resultNode: SyntaxNode | undefined) {
  const result = Range.create(
    Position.create(
      resultNode?.startPosition.row ?? 0,
      resultNode?.startPosition.column ?? 0
    ),
    Position.create(
      resultNode?.endPosition.row ?? 0,
      resultNode?.endPosition.column ?? 0
    )
  );
  return result;
}
