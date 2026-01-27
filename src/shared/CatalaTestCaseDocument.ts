import * as vscode from 'vscode';
import type {
  ParseResults,
  Test,
  TestList,
  TestOutputs,
} from '../generated/catala_types';
import { atdToCatala } from '../test-case-editor/testCaseCompilerInterop';
import {
  parseContents,
  getLanguageFromUri,
} from '../extension/testCaseEditorProvider';
import { logger } from '../extension/logger';
import type { integer } from 'vscode-languageclient';

/**
 * Custom document.
 * The editor UI works with the AST (ATD structure, a TestList)
 * but the extension will serialize it to / parse it from a Catala text file.
 */
export class CatalaTestCaseDocument
  extends vscode.Disposable
  implements vscode.CustomDocument
{
  private readonly _uri: vscode.Uri;
  private readonly _language: string;
  //At some point we could think of a better type for the doc contents?
  private _parseResults: ParseResults;
  private _editManager: EditManager;

  private readonly _onDidDispose = new vscode.EventEmitter<void>();
  public readonly onDidDispose = this._onDidDispose.event;

  static async create(
    uri: vscode.Uri,
    backupId: string | undefined
  ): Promise<CatalaTestCaseDocument> {
    const dataFile =
      typeof backupId === 'string' ? vscode.Uri.parse(backupId) : uri;
    const fileData = await CatalaTestCaseDocument.readFile(dataFile);
    return new CatalaTestCaseDocument(uri, fileData);
  }

  private static async readFile(uri: vscode.Uri): Promise<Uint8Array> {
    if (uri.scheme === 'untitled') {
      return new Uint8Array();
    }
    return new Uint8Array(await vscode.workspace.fs.readFile(uri));
  }

  // Fired when an edit is made, notify vs code
  // (which in turn manages the undo stack and dirty indicator...)
  // This does **not** require an explicit subscription in
  // our code (although we re-emit it from the editor,
  // which is the only thing that VS code knows about -- the custom
  // document model is unknown to VS code).
  //
  // Triggered from `setContents()`
  private readonly _onDidChange = new vscode.EventEmitter<
    vscode.CustomDocumentEditEvent<CatalaTestCaseDocument>
  >();
  public readonly onDidChange = this._onDidChange.event;

  // This event is used to trigger UI refreshes.
  // It is fired on GUI edits, undo and redo operations.
  // We subscribe to this event from `resolveCustomEditor`
  private readonly _onDidChangeDocument = new vscode.EventEmitter<
    vscode.CustomDocumentContentChangeEvent<CatalaTestCaseDocument>
  >();
  public readonly onDidChangeContent = this._onDidChangeDocument.event;

  public get uri(): vscode.Uri {
    return this._uri;
  }

  public get language(): string {
    return this._language;
  }

  public get parseResults(): ParseResults {
    return this._parseResults;
  }

  async save(cancellation: vscode.CancellationToken): Promise<void> {
    this._editManager.sync();
    await this.saveAs(this.uri, cancellation);
  }

  async saveAs(
    targetResource: vscode.Uri,
    cancellation: vscode.CancellationToken
  ): Promise<void> {
    if (this._parseResults.kind !== 'Results') {
      throw new Error('Invalid testcase file, cannot save');
    }
    const catalaSource = atdToCatala(this._parseResults.value, this.language);
    const writeData = Buffer.from(catalaSource, 'utf-8');
    if (cancellation.isCancellationRequested) {
      return;
    }
    await vscode.workspace.fs.writeFile(targetResource, writeData);
  }

  async revert(_cancellation: vscode.CancellationToken): Promise<void> {
    const diskContent = await CatalaTestCaseDocument.readFile(this.uri);
    this._parseResults = parseContents(diskContent, this._uri, this._language);

    this._onDidChangeDocument.fire({
      document: this,
    });
  }

  async backup(
    destination: vscode.Uri,
    cancellation: vscode.CancellationToken
  ): Promise<vscode.CustomDocumentBackup> {
    await this.saveAs(destination, cancellation);

    return {
      id: destination.toString(),
      delete: async (): Promise<void> => {
        try {
          await vscode.workspace.fs.delete(destination);
        } catch {
          // noop
        }
      },
    };
  }

  /**
   * Called by VS Code when there are no more references to the document.
   * This happens when all editors for it have been closed.
   */
  dispose(): void {
    this._onDidDispose.fire();
    super.dispose();
  }

  public scheduleChange(tests: TestList, mayBeBatched: boolean): void {
    this._editManager.scheduleChange(tests, mayBeBatched);
  }

  public resetTestOutputs(testingScope: string, outputs: TestOutputs): void {
    this._editManager.resetTestOutputs(testingScope, outputs);

    this._onDidChangeDocument.fire({ document: this });
  }

  // 'makeEdit' in sample
  _setContents(tests: TestList): void {
    const lastRev = this._parseResults;
    const thisRev = (this._parseResults = { kind: 'Results', value: tests });

    this._onDidChange.fire({
      document: this,
      label: 'edit',
      undo: (): void => {
        if (lastRev !== undefined) {
          this._parseResults = lastRev;
          this._onDidChangeDocument.fire({ document: this });
        }
      },
      redo: (): void => {
        this._parseResults = thisRev;
        this._onDidChangeDocument.fire({ document: this });
      },
    });
  }

  private constructor(uri: vscode.Uri, initialContent: Uint8Array) {
    super(() => {}); //XXX -- the sample just seems to be able to call super()
    this._uri = uri;
    this._language = getLanguageFromUri(this._uri);

    this._parseResults = parseContents(
      initialContent,
      this._uri,
      this._language
    );

    this._editManager = new EditManager(this);
  }
}

class EditManager {
  private _doc: CatalaTestCaseDocument;
  private _currentChange: TestList | undefined;
  private _timeout: NodeJS.Timeout | undefined;

  constructor(doc: CatalaTestCaseDocument) {
    this._doc = doc;
    this._currentChange = undefined;
    this._timeout = undefined;
  }

  private applyCurrentChange(): void {
    if (this._currentChange !== undefined) {
      this._doc._setContents(this._currentChange);

      this._currentChange = undefined;
    }
  }

  public scheduleChange(testList: TestList, mayBeBatched: boolean): void {
    clearTimeout(this._timeout);

    // if we reach a non-batchable change
    // we should apply two changes (previous collection + current)
    if (!mayBeBatched) {
      if (this._currentChange !== undefined) {
        this.applyCurrentChange();
      }
      this._currentChange = testList;
      this.applyCurrentChange();
    } else {
      this._currentChange = testList;
      this._timeout = setTimeout(this.applyCurrentChange.bind(this), 350);
    }
  }

  public resetTestOutputs(testingScope: string, outputs: TestOutputs): void {
    this.sync();

    const parseResults = this._doc.parseResults;
    // Problem? We need to forbid UI changes until this
    // state has been propagated to the UI through an Update message?
    if (parseResults.kind !== 'Results') {
      logger.log('Unexpected invalid test file while resetting assertions');
      return;
    }
    const testList = parseResults.value;
    // find affected test
    const idx: integer = testList.findIndex(
      (test) => test.testing_scope === testingScope
    );
    if (idx === -1) {
      logger.log(
        `While resetting assertions: could not find testing scope ${testingScope}`
      );
      return;
    }
    // replace outputs
    const updatedTest: Test = {
      ...testList[idx],
      test_outputs: outputs,
    };

    const newValue = testList.toSpliced(idx, 1, updatedTest);

    this._doc._setContents(newValue);
  }

  // force immediate applying of the latest version, e.g. when saving
  public sync(): void {
    clearTimeout(this._timeout);
    this.applyCurrentChange();
  }
}
