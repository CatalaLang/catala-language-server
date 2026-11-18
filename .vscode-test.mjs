import { defineConfig } from '@vscode/test-cli';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';

// A project never built: no `_build`, so no stdlib until the server runs
// `clerk start`.
function neverBuilt() {
  const ws = fs.mkdtempSync(path.join(os.tmpdir(), 'catala-first-'));
  for (const f of [
    'clerk.toml',
    'optionals.catala_en',
    'test_optionals.catala_en',
  ]) {
    fs.copyFileSync(path.join('tests/round_trip', f), path.join(ws, f));
  }
  return ws;
}

export default defineConfig([
  { label: 'no workspace', files: 'dist/test/suite/**/*.test.js' },
  {
    label: 'first start',
    files: 'dist/test/firststart/**/*.test.js',
    workspaceFolder: neverBuilt(),
  },
]);
