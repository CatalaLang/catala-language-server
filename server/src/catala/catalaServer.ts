import {
  Connection,
  Diagnostic,
  DiagnosticSeverity,
  Position,
  TextDocuments,
} from 'vscode-languageserver';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { parse } from './parser';
import { CatalaFile } from '../shared/CatalaTypes';
import { UriUtils } from '../shared/uri-utils';
import { onCompletion } from './capabilities/onCompletion';
import { onDefinition } from './capabilities/onDefinition';
import { convertNodeToRange } from '../shared/vscodeUtils';
import { closestAscendantOfType } from './tree-sitter-focus/tree-sitter-modules';
import { getCatalaCliOutput } from './modules/getCatalaCliOutput';

export class CatalaServer {
  private catalaFiles: CatalaFile[] = [];

  constructor(
    private connection: Connection,
    private documents: TextDocuments<TextDocument>
  ) {}

  public async validateTextDocument(textDocument: TextDocument) {
    console.log('[INFO] Validating text document...');
    if (this.catalaFiles.length === 0) return Promise.resolve([]);

    const targetFile = this.catalaFiles.find(
      (f) => f.uri === UriUtils.toSysPath(textDocument.uri)
    );
    if (!targetFile) return Promise.resolve([]);
    if (!targetFile.error.length) return Promise.resolve([]);

    const diagnostics: Diagnostic[] = [];

    const cliOutput = await getCatalaCliOutput(targetFile.tree.rootNode.text);
    if (!cliOutput?.range) return Promise.resolve([]);
    const message = cliOutput.text;
    console.error(message, ' at\n', cliOutput.range);
    const diagnostic: Diagnostic = {
      severity: DiagnosticSeverity.Error,
      range: cliOutput.range,
      source: 'catala-language-server',
      message,
    };
    diagnostics.push(diagnostic);

    return Promise.resolve(diagnostics);
  }

  public async onInitialized() {
    this.catalaFiles = [];
    // TODO ISSUE-JcuPKubs
    const extensionSettings = await this.connection.workspace.getConfiguration({
      section: '',
    });

    // parse current document
    for await (const document of this.documents.all()) {
      const content = document.getText();
      try {
        const parsed = await parse(content);
        const uri = UriUtils.toSysPath(document.uri);
        const file: CatalaFile = {
          ...parsed,
          uri,
        };
        this.catalaFiles.push(file);
      } catch (error) {
        console.error(error);
      }
    }

    // const logMe = JSON.stringify(this.catalaFiles[0], null, 2);
  }

  public onCompletion() {
    return onCompletion(this.catalaFiles);
  }

  public async onDefinition(document: TextDocument, position: Position) {
    const normalizedUri = UriUtils.toSysPath(document.uri);
    const targetFile = this.catalaFiles.find(
      (file) => file.uri === normalizedUri
    );
    if (!targetFile) return [];

    const definitions = onDefinition(targetFile, position);
    return definitions;
  }
}
