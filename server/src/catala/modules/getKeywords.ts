import { CatalaLanguages } from '../../shared/CatalaTypes';
import { tokens_local } from '../tree-sitter-focus/tree-sitter-focus';

export function getCatalaKeywords(language: CatalaLanguages = 'en') {
  // TODO ISSUE-Nexpb4xv
  const result = Object.values(tokens_local[language]).map((keyword) => {
    // Because the values, are either strings or regexes
    if (typeof keyword === 'string') {
      return keyword;
    }

    let asString = keyword.source;
    asString = asString.replace(/\\s\+/g, ' ');
    return asString;
  });
  return result;
}
