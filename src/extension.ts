import type { ExtensionContext } from 'vscode';
import type { Executable } from 'vscode-languageclient/node';
import { LanguageClient } from 'vscode-languageclient/node';
import * as path from 'path';

import { TestCaseEditorProvider } from './testCaseEditor';
import { logger } from './logger';

let client: LanguageClient;

export function activate(context: ExtensionContext): void {
  logger.show();

  const cmd = context.asAbsolutePath(
    path.join('server', '_build', 'default', 'src', 'main.exe')
  );

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
