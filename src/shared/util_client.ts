import * as vscode from 'vscode';
import * as fs from 'fs';
import cmd_exists from 'command-exists';
import { execFileSync, spawn } from 'child_process';
import path from 'path';
import { logger } from '../extension/logger';

export type JsonValue =
  | string
  | number
  | boolean
  | null
  | JsonValue[]
  | { [key: string]: JsonValue };

export type RunArgs = {
  uri: string;
  scope: string;
  inputs?: JSON;
  withTrace?: boolean;
  traceOutputFile?: string;
  headless?: boolean;
  buildDir?: string;
  ninjaOutput?: string;
};

var warned = false;

export type Binary = { path: string; version?: string };

/**
 * Spawns `command` and resolves with its trimmed stdout, or `undefined` if the
 * process could not be started or exited non-zero. Never rejects: a missing
 * executable resolves to `undefined` instead of raising an unhandled 'error'
 * event. Note that a successful command with no output resolves to '', which is
 * distinct from the `undefined` returned on failure.
 */
function spawnStdout(
  command: string,
  args: string[]
): Promise<string | undefined> {
  return new Promise((resolve) => {
    const proc = spawn(command, args);
    // Without an encoding, 'data' hands us Buffers: they interpolate fine in a
    // template literal but are not strings, which silently breaks any consumer
    // expecting one (e.g. TreeItem.description).
    let out = '';
    proc.stdout.setEncoding('utf8');
    proc.stdout.on('data', (data: string) => {
      // Concatenate: output may arrive in several chunks.
      out += data;
    });
    proc.on('error', (err) => {
      logger.log(`Failed to spawn '${command}': ${err.message}`);
      resolve(undefined);
    });
    proc.on('close', (code: number | null) => {
      resolve(code === 0 ? out.trim() : undefined);
    });
  });
}

export async function tryBinaryPath(
  binaryName: string,
  pathEnv: string,
  server?: boolean
): Promise<Binary | undefined> {
  let binary = path.join(pathEnv, binaryName);
  if (!fs.existsSync(binary)) {
    return undefined;
  }
  if (server) {
    // Server doesn't have version command and never hands so just returning the binary name
    return { path: binary };
  }
  const version = await spawnStdout(binary, ['--version']);
  if (version === undefined) {
    // A non-zero exit means the binary is not usable, not merely version-less.
    return undefined;
  }
  return { path: binary, version: version || undefined };
}

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
      const msg = `${public_bin_name} configured path '${configured_path}' does not exist.`;
      logger.log(msg);
      vscode.window.showWarningMessage(msg);
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

export function getCwd(bufferPath: string): string | undefined {
  // Uri.file, not Uri.parse: bufferPath is an OS path.
  return vscode.workspace.getWorkspaceFolder(vscode.Uri.file(bufferPath))?.uri
    ?.fsPath;
}

// The CLI helpers spawn with shell:true on Windows so a bare 'clerk'/'catala'
// resolves via PATHEXT; cmd.exe then word-splits and Node does not quote argv,
// so double-quote any argument containing whitespace (e.g. a spaced path).
export function shellArg(a: string): string {
  return process.platform === 'win32' && /\s/.test(a) ? `"${a}"` : a;
}

export function hasResourceUri(x: unknown): x is { resourceUri: vscode.Uri } {
  if (!x || typeof x !== 'object') return false;
  const ru = (x as { resourceUri?: unknown }).resourceUri;
  return !!ru && ru instanceof vscode.Uri;
}
