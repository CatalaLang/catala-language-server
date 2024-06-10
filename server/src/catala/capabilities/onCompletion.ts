import { CompletionItem, CompletionItemKind } from 'vscode-languageserver';
import { CatalaFile } from '../../shared/CatalaTypes';
import { getCatalaKeywords } from '../modules/getKeywords';

export function onCompletion(catalaFiles: CatalaFile[]): CompletionItem[] {
  const keywords = getCatalaKeywords();
  const result = keywords.map((keyword) => ({
    label: keyword,
    kind: CompletionItemKind.Keyword,
  }));

  const declarations = catalaFiles.flatMap((file) => {
    const declarationAggregates = file.enum_struct_name.concat(file.scope_name);
    let declarationNames = declarationAggregates.map(
      (declaration) => declaration.text
    );
    declarationNames = [...new Set(declarationNames)];

    return declarationNames.map((declarationName) => {
      return {
        label: declarationName,
        kind: CompletionItemKind.Class,
      };
    });
  });

  const fields = catalaFiles.flatMap((file) => {
    const fieldAggregates = file.field_name.concat(file.variable);
    let fieldNames = fieldAggregates.map((field) => field.text);
    fieldNames = [...new Set(fieldNames)];
    return fieldNames.map((fieldName) => {
      return {
        label: fieldName,
        kind: CompletionItemKind.Variable,
      };
    });
  });

  const aggregate = [...result, ...declarations, ...fields];
  return aggregate;
}
