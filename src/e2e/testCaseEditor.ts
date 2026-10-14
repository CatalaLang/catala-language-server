/**
 * Drives a real VS Code window (vscode-extension-tester): opens a round-trip
 * test in the test case editor and reads the page itself.
 */
import * as assert from 'assert';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { execSync } from 'child_process';
import {
  By,
  EditorView,
  VSBrowser,
  WebView,
  Workbench,
} from 'vscode-extension-tester';

const fixtures = path.resolve(__dirname, '../../tests/round_trip');

describe('Test case editor, as displayed', function () {
  this.timeout(240_000);

  let dir: string;
  before(() => {
    dir = fs.mkdtempSync(path.join(os.tmpdir(), 'catala-ui-'));
    for (const f of ['clerk.toml', 'items.catala_en', 'test_items.catala_en']) {
      fs.copyFileSync(path.join(fixtures, f), path.join(dir, f));
    }
    execSync('clerk start', { cwd: dir, stdio: 'ignore' });
  });

  it('shows the test of the opened file', async () => {
    const file = 'test_items.catala_en';
    await VSBrowser.instance.openResources(dir);
    const input = await new Workbench().openCommandPrompt();
    await input.setText(path.join(dir, file));
    await input.confirm();
    await new EditorView().openEditor(file);
    await new Workbench().executeCommand(
      'Catala: Open with Catala Test Editor'
    );
    const page = new WebView();
    await page.switchToFrame(120_000);
    try {
      // Test titles are editable fields: read their values
      const titles = async (): Promise<(string | null)[]> =>
        Promise.all(
          (
            await VSBrowser.instance.driver.findElements(
              By.css('input[aria-label="Title"]')
            )
          ).map((e) => e.getAttribute('value'))
        );
      await VSBrowser.instance.driver.wait(
        async () => (await titles()).includes('Two lines'),
        120_000,
        'the page never showed the test "Two lines"'
      );
      assert.deepStrictEqual(await titles(), ['Two lines']);
    } finally {
      await page.switchBack();
    }
  });
});
