import * as vscode from 'vscode';
import type {
  ParseResults,
  Test,
  TestIo,
  TestList,
  TestOutputs,
} from '../generated/catala_types';
import { ensureArrayUids } from '../editors/tableArrayUtils';
import { atdToCatala } from '../test-case-editor/testCaseCompilerInterop';
import {
  parseContents,
  getLanguageFromUri,
} from '../extension/testCaseEditorProvider';
import { rebuiltFrom } from '../test-case-editor/testCaseUtils';
import { logger } from '../extension/logger';
import type { integer } from 'vscode-languageclient';

function stampIoUids(io: TestIo): TestIo {
  if (!io.value) return io;
  return {
    ...io,
    value: { ...io.value, value: ensureArrayUids(io.value.value) },
  };
}

function stampParseResultsUids(results: ParseResults): ParseResults {
  if (results.kind !== 'Results') return results;
  return {
    kind: 'Results',
    value: results.value.map((test) => ({
      ...test,
      test_inputs: new Map(
        Array.from(test.test_inputs, ([k, v]) => [k, stampIoUids(v)])
      ),
      test_outputs: new Map(
        Array.from(test.test_outputs, ([k, v]) => [k, stampIoUids(v)])
      ),
    })),
  };
}

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
  /* Kept out of _parseResults, or a save would write it over the original. */
  private _rebuilt: TestList | undefined;
  private _editManager: EditManager;
  private _rebuildManager: EditManager;

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
    this._rebuildManager.sync();
    await this.saveAs(this.uri, cancellation);
  }

  async saveAs(
    targetResource: vscode.Uri,
    cancellation: vscode.CancellationToken
  ): Promise<void> {
    /* Never over the original, which stays authoritative until replaced. */
    if (this._parseResults.kind === 'BrokenTest') {
      if (this._rebuilt === undefined || this._rebuilt.length === 0) {
        // Nothing to write, but a plain save must still succeed or a dirty
        // flag (a restored session, say) can never be cleared.
        if (targetResource.toString() === this.uri.toString()) return;
        throw new Error(
          'Nothing to save yet: this test could not be rebuilt against its scope.'
        );
      }
      const source = atdToCatala(this._rebuilt, this.language);
      if (cancellation.isCancellationRequested) return;
      await vscode.workspace.fs.writeFile(
        vscode.Uri.file(targetResource.fsPath + '.updated'),
        Buffer.from(source, 'utf-8')
      );
      return;
    }
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
    this._parseResults = stampParseResultsUids(
      parseContents(diskContent, this._uri, this._language)
    );
    this._rebuilt = rebuiltFrom(this._parseResults);

    this._onDidChangeDocument.fire({
      document: this,
    });
  }

  async backup(
    destination: vscode.Uri,
    cancellation: vscode.CancellationToken
  ): Promise<vscode.CustomDocumentBackup> {
    /* The working copy is the backup; restoring reopens the original, whose
       rebuild finds the copy. (Writing under the backup path put it at
       `<backup>.updated` while VS Code restored from `<backup>`: ENOENT.) */
    if (this._parseResults.kind === 'BrokenTest') {
      await this.saveAs(this.uri, cancellation);
      return { id: this.uri.toString(), delete: async (): Promise<void> => {} };
    }
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

  /** An edit to the rebuild of a broken test, coalesced like an ordinary one. */
  public setRebuilt(tests: TestList, mayBeBatched: boolean): void {
    this._rebuildManager.scheduleChange(tests, mayBeBatched);
  }

  /** Dirties the document; leaves the parse results alone. */
  _commitRebuilt(tests: TestList): void {
    const previous = this._rebuilt;
    this._rebuilt = tests;
    this._onDidChange.fire({
      document: this,
      label: 'rebuild',
      undo: (): void => {
        this._rebuilt = previous;
        this._onDidChangeDocument.fire({ document: this });
      },
      redo: (): void => {
        this._rebuilt = tests;
        this._onDidChangeDocument.fire({ document: this });
      },
    });
  }

  public get rebuilt(): TestList | undefined {
    return this._rebuilt;
  }

  /** The tester chose which scope to rebuild against: one undoable step. */
  public retarget(results: ParseResults): void {
    const previous = { results: this._parseResults, rebuilt: this._rebuilt };
    const next = { results, rebuilt: rebuiltFrom(results) };
    const apply = (s: typeof next): void => {
      this._parseResults = s.results;
      this._rebuilt = s.rebuilt;
      this._onDidChangeDocument.fire({ document: this });
    };
    apply(next);
    this._onDidChange.fire({
      document: this,
      label: 'retarget',
      undo: (): void => apply(previous),
      redo: (): void => apply(next),
    });
  }

  private get workingCopyUri(): vscode.Uri {
    return vscode.Uri.file(this.uri.fsPath + '.updated');
  }

  private async deleteWorkingCopy(): Promise<void> {
    try {
      await vscode.workspace.fs.delete(this.workingCopyUri);
    } catch {
      /* there was none */
    }
  }

  /** The rebuild becomes the test. The working copy goes, or reopening would
   *  prefer it. Not undoable: version control is the way back. */
  async replaceOriginal(): Promise<void> {
    if (this._parseResults.kind !== 'BrokenTest') {
      throw new Error('Only a broken test has a working copy to promote.');
    }
    this._rebuildManager.sync();
    const rebuilt = this._rebuilt;
    if (rebuilt === undefined || rebuilt.length === 0) {
      throw new Error(
        'Nothing to replace the original with: this test could not be rebuilt against its scope.'
      );
    }
    const source = atdToCatala(rebuilt, this.language);
    await vscode.workspace.fs.writeFile(this.uri, Buffer.from(source, 'utf-8'));
    await this.deleteWorkingCopy();
    this._parseResults = stampParseResultsUids({
      kind: 'Results',
      value: rebuilt,
    });
    this._rebuilt = undefined;
    this._onDidChangeDocument.fire({ document: this });
  }

  /** Delete the working copy, retarget included, and reparse. */
  async discardWorkingCopy(): Promise<void> {
    if (this._parseResults.kind !== 'BrokenTest') {
      throw new Error('Only a broken test has a working copy to discard.');
    }
    await this.deleteWorkingCopy();
    await this.revert(new vscode.CancellationTokenSource().token);
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
    // Disposable wants a dispose callback; ours has nothing extra to release.
    super(() => {});
    this._uri = uri;
    this._language = getLanguageFromUri(this._uri);

    this._parseResults = stampParseResultsUids(
      parseContents(initialContent, this._uri, this._language)
    );
    this._rebuilt = rebuiltFrom(this._parseResults);

    this._editManager = new EditManager(this, (t) => this._setContents(t));
    this._rebuildManager = new EditManager(this, (t) => this._commitRebuilt(t));
  }
}

class EditManager {
  private _doc: CatalaTestCaseDocument;
  private _apply: (tests: TestList) => void;
  private _currentChange: TestList | undefined;
  private _timeout: NodeJS.Timeout | undefined;

  constructor(doc: CatalaTestCaseDocument, apply: (tests: TestList) => void) {
    this._doc = doc;
    this._apply = apply;
    this._currentChange = undefined;
    this._timeout = undefined;
  }

  private applyCurrentChange(): void {
    if (this._currentChange !== undefined) {
      this._apply(this._currentChange);

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
    const stampedOutputs: TestOutputs = new Map(
      Array.from(outputs, ([k, v]) => [k, stampIoUids(v)])
    );
    const updatedTest: Test = {
      ...testList[idx],
      test_outputs: stampedOutputs,
    };

    const newValue = testList.toSpliced(idx, 1, updatedTest);

    this._apply(newValue);
  }

  // force immediate applying of the latest version, e.g. when saving
  public sync(): void {
    clearTimeout(this._timeout);
    this.applyCurrentChange();
  }
}
