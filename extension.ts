import { ExtensionContext, workspace } from 'vscode';
import {
  LanguageClient,
  Executable,
  DidChangeConfigurationNotification,
} from 'vscode-languageclient/node';
import * as path from 'path';

import { CatalaTestCaseEditorProvider } from './catalaTestCaseEditor';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
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

  console.log('activating test case editor');
  context.subscriptions.push(CatalaTestCaseEditorProvider.register(context));
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
