import type { JsonValue } from '../shared/util_client';
import type { TraceElement } from './traceUtils';

export type TracePos = { line: number; character: number };

export type TraceUpMessage =
  | { kind: 'ready' }
  | { kind: 'run'; scope: string }
  | { kind: 'loadFile'; path: string }
  | { kind: 'openLocation'; file: string; start: TracePos; end: TracePos }
  | {
      kind: 'requestExtract';
      id: number;
      file: string;
      line: number;
    };

export type TraceDownMessage =
  | {
      kind: 'init';
      file: string;
      cwd: string;
      scopes: [string, JsonValue][];
      scope?: string;
      trace?: TraceElement[];
      run?: boolean;
    }
  | { kind: 'result'; ok: true; trace: TraceElement[] }
  | { kind: 'result'; ok: false; error: string }
  | { kind: 'extract'; id: number; text: string | null };

export type TraceResult =
  | { ok: true; trace: TraceElement[] }
  | { ok: false; error: string };
