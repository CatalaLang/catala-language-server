export type IoInput = 'NoInput' | 'OnlyInput' | 'Reentrant';

export type IoLog = {
  io_input: IoInput;
  io_output: boolean;
};

export type SourcePos = {
  filename: string;
  start_line: number;
  law_headings: string[];
};

export type VarComputation = {
  kind: 'VarComputation';
  name: string[];
  io: IoLog;
  value: unknown;
  pos?: SourcePos;
};

export type SubScopeCall = {
  kind: 'SubScopeCall';
  name: string[];
  inputs: VarComputation[];
  body: TraceEvent[];
};

export type TraceEvent = VarComputation | SubScopeCall;
