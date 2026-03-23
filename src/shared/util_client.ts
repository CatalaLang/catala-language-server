import * as vscode from 'vscode';
import * as fs from 'fs';
import cmd_exists from 'command-exists';
import { execFileSync } from 'child_process';
import path from 'path';
import { logger } from '../extension/logger';

export type RunArgs = {
  uri: string;
  scope: string;
  inputs?: JSON;
};

var warned = false;

export function resolveBinaryPath(
  public_bin_name: string,
  context?: vscode.ExtensionContext,
  local_bin_name?: string,
  configured_path?: string | undefined
): string | undefined {
  if (configured_path) {
    if (fs.existsSync(configured_path)) {
      // Configured path is highest priority
      return configured_path;
    } else {
      vscode.window.showWarningMessage(
        `${public_bin_name} configured path '${configured_path}' does not exist.`
      );
    }
  }
  const local_path = local_bin_name
    ? context
      ? context.asAbsolutePath(
          path.join('_build', 'default', 'server', 'src', local_bin_name)
        )
      : undefined
    : undefined;
  if (local_path && fs.existsSync(local_path)) {
    // Then, dev path is higher priority
    return local_path;
  } else if (cmd_exists.sync(public_bin_name)) {
    // Then, the one in the path
    return public_bin_name;
  } else {
    const public_bin_exe = public_bin_name + '.exe';
    if (cmd_exists.sync(public_bin_exe)) {
      // Also try to look it up with a .exe
      return public_bin_exe;
    } else {
      try {
        // Final try: lookup in opam bin dirs
        const opam_bin = execFileSync('opam', ['var', 'bin']).toString().trim();
        if (opam_bin != '' && fs.existsSync(opam_bin)) {
          const in_opam_bin = path.join(opam_bin, public_bin_name);
          if (fs.existsSync(in_opam_bin)) {
            if (!warned) {
              vscode.window.showErrorMessage(
                `Run 'opam init --shell-setup' in a terminal and *fully* restart vscode: Catala is installed but not available in the shell environment`
              );
              warned = true;
            }
          }
        }
      } catch {
        /* tslint:disable */
      }
    }
  }
  if (!warned) {
    vscode.window.showErrorMessage(
      `${public_bin_name} not found on the system. Please refer to the installation procedure https://github.com/CatalaLang/catala-language-server/?tab=readme-ov-file#installation`
    );
    warned = true;
  }
}

export function getConfig(confId: string): string | undefined {
  const confPath = vscode.workspace
    .getConfiguration('catala')
    .get<string>(confId);
  return confPath?.trim();
}

export const catalaPath: string =
  resolveBinaryPath('catala', undefined, undefined, getConfig('catalaPath')) ??
  'catala';

export const clerkPath: string =
  resolveBinaryPath('clerk', undefined, undefined, getConfig('clerkPath')) ??
  'clerk';

logger.log(`catala command: ${catalaPath}`);
logger.log(`clerk command: ${clerkPath}`);

export function getCwd(bufferPath: string): string | undefined {
  return vscode.workspace.getWorkspaceFolder(vscode.Uri.parse(bufferPath))?.uri
    ?.fsPath;
}

export function hasResourceUri(x: unknown): x is { resourceUri: vscode.Uri } {
  if (!x || typeof x !== 'object') return false;
  const ru = (x as { resourceUri?: unknown }).resourceUri;
  return !!ru && ru instanceof vscode.Uri;
}
