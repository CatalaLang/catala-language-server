import * as vscode from 'vscode';
import * as fs from 'fs';

// For exhaustiveness checks
export function assertUnreachable(x: never): never {
  throw new Error(`Unexpected value: ${x}`);
}

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

export type ClerkPosition = {
  fname: string;
  start_lnum: number;
  start_cnum: number;
  end_lnum: number;
  end_cnum: number;
};

export function positionToLocation(pos: ClerkPosition): vscode.Location {
  return new vscode.Location(vscode.Uri.file(pos.fname), positionToRange(pos));
}

export function positionToRange(
  pos: Omit<ClerkPosition, 'fname'>
): vscode.Range {
  return new vscode.Range(
    new vscode.Position(pos.start_lnum - 1, pos.start_cnum - 1),
    new vscode.Position(pos.end_lnum - 1, pos.end_cnum - 1)
  );
}
