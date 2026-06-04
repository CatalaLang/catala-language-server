import type * as vscode from 'vscode';
import type { RunArgs } from '../shared/util_client';

export async function resolveDebugArgs(
  config: vscode.DebugConfiguration,
  selectScopeFn: () => Promise<RunArgs | undefined>
): Promise<vscode.DebugConfiguration | undefined> {
  if (!config.args?.scope) {
    const selected = await selectScopeFn();
    if (!selected) return undefined;
    config.args = selected;
  }
  return config;
}
