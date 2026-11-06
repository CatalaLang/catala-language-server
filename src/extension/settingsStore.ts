import * as vscode from 'vscode';
import type { CatalaSettings, SettingsMessage } from '../shared/settings';
import { readSettings, resolveLanguage } from '../shared/settings';

const SECTION = 'catala';

export type SettingsSource = {
  get(): CatalaSettings;
  follow(webview: vscode.Webview, file?: string): vscode.Disposable;
};

export class SettingsStore implements SettingsSource {
  private readonly webviews = new Map<vscode.Webview, string | undefined>();

  constructor(context: vscode.ExtensionContext) {
    context.subscriptions.push(
      vscode.workspace.onDidChangeConfiguration((event) => {
        if (!event.affectsConfiguration(SECTION)) {
          return;
        }
        for (const [webview, file] of this.webviews) {
          void this.post(webview, file);
        }
      })
    );
  }

  public get(): CatalaSettings {
    const configuration = vscode.workspace.getConfiguration(SECTION);
    return readSettings({
      language: configuration.get('language'),
    });
  }

  public follow(webview: vscode.Webview, file?: string): vscode.Disposable {
    this.webviews.set(webview, file);
    void this.post(webview, file);
    return new vscode.Disposable(() => this.webviews.delete(webview));
  }

  private async post(
    webview: vscode.Webview,
    file: string | undefined
  ): Promise<void> {
    const settings = this.get();
    const message: SettingsMessage = {
      kind: 'catalaSettings',
      value: settings,
      locale: resolveLanguage(settings, file, vscode.env.language),
    };
    await webview.postMessage(message);
  }
}
