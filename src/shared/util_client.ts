import * as vscode from 'vscode';
import * as fs from 'fs';

export type RunArgs = {
  uri: string;
  scope: string;
  inputs?: JSON;
};

function pathFromConfig(confId: string, defaultCmd: string): string {
  const confPath = vscode.workspace
    .getConfiguration('catala')
    .get<string>(confId);
  if (confPath === undefined || confPath === null || confPath.trim() === '')
    return defaultCmd;
  if (!fs.existsSync(confPath)) {
    vscode.window.showWarningMessage(
      `Could not find executable for ${confId} at ${confPath}, falling back to default`
    );
    return defaultCmd;
  }
  return confPath;
}

export const clerkPath: string = pathFromConfig('clerkPath', 'clerk');

export function getCwd(bufferPath: string): string | undefined {
  return vscode.workspace.getWorkspaceFolder(vscode.Uri.parse(bufferPath))?.uri
    ?.fsPath;
}

export function hasResourceUri(x: unknown): x is { resourceUri: vscode.Uri } {
  if (!x || typeof x !== 'object') return false;
  const ru = (x as { resourceUri?: unknown }).resourceUri;
  return !!ru && ru instanceof vscode.Uri;
}
