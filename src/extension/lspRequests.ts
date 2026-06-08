import * as vscode from 'vscode';
import type { LanguageClient } from 'vscode-languageclient/node';

import type {
  Entrypoint,
  EntrypointParamKind,
  EntrypointsParams,
  ReadTestOutput,
  ReadTestParams,
  TestList,
  WriteTestOutput,
  WriteTestParams,
} from '../generated/catala_types';
import {
  writeReadTestParams,
  writeEntrypointsParams,
  readEntrypoints,
  readReadTestOutput,
  writeWriteTestParams,
} from '../generated/catala_types';
import { getClient } from '../extension';

// Atd prevents us to obtain direct vscode's ranges, we convert them here.
export type CatalaEntrypoint = Omit<Entrypoint, 'range'> & {
  range: vscode.Range;
};

export async function listEntrypoints(
  only?: EntrypointParamKind[],
  path?: string,
  no_lambdas?: boolean,
  no_variables?: boolean
): Promise<Array<CatalaEntrypoint>> {
  const client = await getClient()
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

export type ExceptionsArgs = {
  uri: string;
  scope: string;
  variable: string;
  declFile: string;
  declLine: number;
  declCol: number;
  declEndLine: number;
  declEndCol: number;
};

export async function exceptionsAt(
  uri: vscode.Uri,
  position: vscode.Position
): Promise<ExceptionsArgs | null> {
  const client = await getClient()
  const result = await client.sendRequest<ExceptionsArgs | null>(
    'catala.exceptionsAt',
    {
      uri: uri.toString(),
      position: { line: position.line, character: position.character },
    }
  );
  return result;
}

export async function readTest(
  lang: string,
  contents: string,
  bufferPath?: string
): Promise<ReadTestOutput | null> {
  let params: ReadTestParams = { lang, contents, buffer_path: bufferPath }
  const client = await getClient()
  const result = await client.sendRequest(
    'catala.readTest',
    writeReadTestParams(params)
  );
  return result ? readReadTestOutput(result) : null;
}

export async function writeTest(
  client: LanguageClient,
  lang: string,
  tests: TestList
): Promise<WriteTestOutput | null> {
  let params: WriteTestParams = { lang, tests }
  return await client.sendRequest(
    'catala.writeTest',
    writeWriteTestParams(params)
  );
}
