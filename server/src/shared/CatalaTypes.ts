import * as Parser from 'tree-sitter';
import * as GrammarJson from '../catala/tree-sitter/src/grammar.json';

export type Id = number;
export type CatalaLanguages = 'en' | 'fr' | 'pl';

export type CatalaGrammarTypes = keyof typeof GrammarJson['rules'] | 'ERROR';
export interface CatalaSyntaxNode extends Parser.SyntaxNode {
  type: CatalaGrammarTypes;
  children: CatalaSyntaxNode[];
  nextSibling: CatalaSyntaxNode;
  parent: CatalaSyntaxNode;
  ascendantTypesPath: CatalaGrammarTypes[];
}

export enum DeclarationType {
  Scope = 'Scope',
  /** TODO ISSUE-6FErb8ou Can I assume, that structs are unique? */
  Struct = 'Struct',
  StructField = 'StructField',
}

/**
 * declaration scope IncomeTaxComputation:
 *   input individual content Individual   <---
 */
export interface CatalaStructureField {
  id: Id;
  text: string;
  type: DeclarationType | string;
  scopeId?: Id;
  structId?: Id;
}

export interface CatalaDeclaration {
  id: Id;
  text: string;
  type: DeclarationType;
  startPosition: Parser.SyntaxNode['startPosition'];
  endPosition: Parser.SyntaxNode['endPosition'];
}

export interface CatalaFileParsed {
  error: CatalaSyntaxNode[];
  constructor_name: CatalaSyntaxNode[];
  enum_struct_name: CatalaSyntaxNode[];
  field_name: CatalaSyntaxNode[];
  scope_name: CatalaSyntaxNode[];
  variable: CatalaSyntaxNode[];
  verb_block: CatalaSyntaxNode[];
  tree: Parser.Tree;
}

export interface CatalaFile extends CatalaFileParsed {
  /** after going through UriUtils.toSysPath */
  uri: string;
}
