import * as vscode from 'vscode';
import type { LanguageClient } from 'vscode-languageclient/node';
import type {
  Entrypoint,
  EntrypointParamKind,
  EntrypointsParams,
} from '../generated/catala_types';
import {
  writeEntrypointsParams,
  readEntrypoints,
} from '../generated/catala_types';

// Atd prevents us to obtain direct vscode's ranges, we convert them here.
export type CatalaEntrypoint = Omit<Entrypoint, 'range'> & {
  range: vscode.Range;
};

export async function listEntrypoints(
  client: LanguageClient,
  only?: EntrypointParamKind[],
  path?: string,
  no_lambdas?: boolean,
  no_variables?: boolean
): Promise<Array<CatalaEntrypoint>> {
  const params: EntrypointsParams = {
    only: (only ??= []),
    path,
    no_lambdas,
    no_variables,
  };
  let ret: JSON = await client.sendRequest(
    'catala.listEntrypoints',
    writeEntrypointsParams(params)
  );
  const raw_entrypoints = readEntrypoints(ret);
  const entrypoints: Array<CatalaEntrypoint> = raw_entrypoints.map((e) => {
    const cep: CatalaEntrypoint = {
      path: e.path,
      entrypoint: e.entrypoint,
      range: new vscode.Range(
        e.range.start.line,
        e.range.start.character,
        e.range.end_.line,
        e.range.end_.character
      ),
    };
    return cep;
  });
  return entrypoints;
}
