import * as vscode from 'vscode';
import type { Executable } from 'vscode-languageclient/node';
import { LanguageClient } from 'vscode-languageclient/node';
import * as path from 'path';

import { TestCaseEditorProvider } from './testCaseEditor';
import { logger } from './logger';
import * as fs from 'fs';
import * as cmd_exists from 'command-exists';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext): void {
  const local_path = context.asAbsolutePath(
    path.join('_build', 'default', 'server', 'src', 'main.exe')
  );
  const lsp_binary_prefix = 'catala-lsp';
  let lsp_binary = lsp_binary_prefix;

  const is_local_binary_present = fs.existsSync(local_path);
  let is_binary_in_path = false;
  if (cmd_exists.sync(lsp_binary_prefix)) {
    is_binary_in_path = true;
  } else {
    lsp_binary = lsp_binary_prefix + '.exe';
    is_binary_in_path = cmd_exists.sync(lsp_binary);
  }

  if (!is_binary_in_path && !is_local_binary_present) {
    const err_msg =
      'Catala LSP not found on the system: please refer to the installation procedure https://github.com/CatalaLang/catala-language-server/?tab=readme-ov-file#installation';
    vscode.window.showErrorMessage(err_msg);
  } else {
    let cmd;
    if (is_local_binary_present) {
      cmd = local_path;
    } else {
      cmd = lsp_binary;
    }
    const run: Executable = { command: cmd };
    client = new LanguageClient(
      cmd,
      'Catala Language Server Protocol',
      { run, debug: run },
      {
        documentSelector: [
          { scheme: 'file', language: 'catala_en', pattern: '**/*.catala_en' },
          { scheme: 'file', language: 'catala_fr', pattern: '**/*.catala_fr' },
        ],
      }
    );

    client.start();
  }

  // Always register the custom editor provider
  context.subscriptions.push(TestCaseEditorProvider.register(context));

  // Ensure the logger is disposed when the extension is deactivated
  context.subscriptions.push({ dispose: () => logger.dispose() });
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
