/**
 * The first start on a project that was never built: the server builds the
 * stdlib itself, and the files must be analysed without a restart.
 */
import * as assert from 'assert';
import * as path from 'path';
import * as vscode from 'vscode';

async function hoverText(
  uri: vscode.Uri,
  position: vscode.Position
): Promise<string> {
  const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
    'vscode.executeHoverProvider',
    uri,
    position
  );
  return hovers
    .flatMap((h) => h.contents)
    .map((c) => (typeof c === 'string' ? c : c.value))
    .join('\n');
}

suite('First start', function () {
  this.timeout(180_000);

  test('a file of a project never built gets its hover', async () => {
    const ws = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
    assert.ok(ws, 'no workspace folder');
    const uri = vscode.Uri.file(path.join(ws, 'test_optionals.catala_en'));
    await vscode.window.showTextDocument(uri);

    // `calc.base` in `definition calc.base equals $1000.00`.
    const position = new vscode.Position(13, 16);
    let text = '';
    const deadline = Date.now() + 120_000;
    while (!text.includes('money') && Date.now() < deadline) {
      await new Promise((resolve) => setTimeout(resolve, 2_000));
      text = await hoverText(uri, position);
    }
    assert.ok(text.includes('money'), `no type on hover, got: ${text}`);
  });
});
