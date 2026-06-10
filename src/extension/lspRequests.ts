import * as vscode from 'vscode';
import type { LanguageClient } from 'vscode-languageclient/node';

import type {
  Entrypoint,
  EntrypointParamKind,
  EntrypointsParams,
  GenerateTestOutput,
  GenerateTestParams,
  ListScopesOutput,
  ListScopesParams,
  ReadTestOutput,
  ReadTestParams,
  RunTestOutput,
  RunTestParams,
  SerializeInputsOutput,
  SerializeInputsParams,
  TestInputs,
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
  writeRunTestOutput,
  writeRunTestParams,
  readRunTestOutput,
  writeListScopesParams,
  readListScopesOutput,
  writeGenerateTestParams,
  readGenerateTestOutput,
  writeSerializeInputsParams,
  readSerializeInputsOutput,
} from '../generated/catala_types';
import { getClient } from '../extension';
import { logger } from './logger';

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
  lang: string,
  tests: TestList
): Promise<WriteTestOutput | null> {
  let params: WriteTestParams = { lang, tests }
  const client = await getClient()
  return await client.sendRequest(
    'catala.writeTest',
    writeWriteTestParams(params)
  );
}

export async function runTest(
  file: string,
  scope: string,
  input?: TestInputs
): Promise<RunTestOutput | null> {
  let params: RunTestParams = { scope, file, input }
  const client = await getClient()
  logger.log("before request")
  const result = await client.sendRequest(
    'catala.runTest',
    writeRunTestParams(params)
  );
  logger.log("after request")
  return result ? readRunTestOutput(result) : null;
}

export async function listScopes(file: string): Promise<ListScopesOutput | null> {
  const client = await getClient()
  let params: ListScopesParams = { file }
  const result = await client.sendRequest(
    'catala.runTest',
    writeListScopesParams(params)
  );
  return result ? readListScopesOutput(result) : null;
}

export async function generateTest(
  file: string,
  scope: string,
  defaultValues?: boolean,
  forceModule?: boolean
): Promise<GenerateTestOutput | null> {
  const client = await getClient()
  let params: GenerateTestParams = { file, scope, default_values: defaultValues, force_module: forceModule }
  const result = await client.sendRequest(
    'catala.runTest',
    writeGenerateTestParams(params)
  );
  return result ? readGenerateTestOutput(result) : null;
}

export async function serializeInputs(
  inputs: TestInputs
): Promise<SerializeInputsOutput | null> {
  const client = await getClient()
  let params: SerializeInputsParams = { inputs }
  const result = await client.sendRequest(
    'catala.runTest',
    writeSerializeInputsParams(params)
  );
  return result ? readSerializeInputsOutput(result) : null;
}