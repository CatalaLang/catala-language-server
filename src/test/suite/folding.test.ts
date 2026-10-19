import * as assert from 'assert';
import * as vscode from 'vscode';

const content = [
  '# Article 1',
  'Some prose.',
  '```catala',
  'declaration scope S:',
  '  input x content integer',
  '```',
  'More prose.',
  '',
].join('\n');

function lineVisible(editor: vscode.TextEditor, line: number): boolean {
  return editor.visibleRanges.some(
    (r) => r.start.line <= line && line <= r.end.line
  );
}

// The folding model is computed asynchronously after the document opens.
async function eventually(check: () => Promise<boolean>): Promise<boolean> {
  for (let i = 0; i < 50; i++) {
    if (await check()) return true;
    await new Promise((r) => setTimeout(r, 100));
  }
  return false;
}

suite('Folding Catala code blocks', function () {
  this.timeout(60_000);

  test('the fold and unfold commands act on every block', async () => {
    await vscode.extensions.getExtension('catalalang.catala')?.activate();
    const doc = await vscode.workspace.openTextDocument({
      language: 'catala_en',
      content,
    });
    const editor = await vscode.window.showTextDocument(doc);

    assert.ok(
      await eventually(async () => {
        await vscode.commands.executeCommand('catala.foldCodeBlocks');
        return !lineVisible(editor, 3);
      }),
      'block not folded'
    );
    assert.ok(lineVisible(editor, 2), 'opening fence hidden');
    assert.ok(lineVisible(editor, 6), 'prose after the block hidden');

    await vscode.commands.executeCommand('catala.unfoldCodeBlocks');
    assert.ok(
      await eventually(async () => lineVisible(editor, 3)),
      'block not unfolded'
    );
  });
});
