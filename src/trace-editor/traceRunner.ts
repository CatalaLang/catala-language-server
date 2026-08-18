import * as vscode from 'vscode';
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from 'fs';
import { execFileSync } from 'child_process';
import { createHash } from 'crypto';
import { tmpdir } from 'os';
import { dirname, join } from 'path';
import type { TraceResult } from './messages';
import { type TraceElement, traceFromJson } from './traceUtils';
import { getCwd } from '../shared/util_client';
import { logger } from '../extension/logger';

export function readTraceFile(path: string): TraceResult {
  try {
    const parsed = JSON.parse(readFileSync(path, 'utf8'));
    const trace = traceFromJson(parsed);
    if (trace === null) {
      return { ok: false, error: 'The file does not contain a Catala trace.' };
    }
    return { ok: true, trace };
  } catch (e) {
    return { ok: false, error: String(e) };
  }
}

type CacheResult<T> = { scope: string; uri: string; result: T };
type CacheBucket<T> = { date: number; results: CacheResult<T>[] };

const CACHE_MAX_AGE_MS = 7 * 24 * 60 * 60 * 1000;

class PersistentCache<T, S> {
  private readonly map = new Map<string, CacheBucket<T>>();

  constructor(
    private readonly file: string,
    private readonly toJson: (v: T) => S,
    private readonly fromJson: (v: S) => T
  ) {
    this.load();
  }

  private load(): void {
    let data: Record<string, CacheBucket<S>> | undefined;
    try {
      data = JSON.parse(readFileSync(this.file, 'utf8'));
    } catch {
      return;
    }
    const now = Date.now();
    for (const [hash, bucket] of Object.entries(data ?? {})) {
      if (now - bucket.date >= CACHE_MAX_AGE_MS) continue;
      this.map.set(hash, {
        date: bucket.date,
        results: bucket.results.map((r) => ({
          scope: r.scope,
          uri: r.uri,
          result: this.fromJson(r.result),
        })),
      });
    }
    this.persist();
  }

  private persist(): void {
    const data: Record<string, CacheBucket<S>> = {};
    for (const [hash, bucket] of this.map) {
      data[hash] = {
        date: bucket.date,
        results: bucket.results.map((r) => ({
          scope: r.scope,
          uri: r.uri,
          result: this.toJson(r.result),
        })),
      };
    }
    try {
      mkdirSync(dirname(this.file), { recursive: true });
      writeFileSync(this.file, JSON.stringify(data), 'utf8');
    } catch {
      /* ignore write errors */
    }
  }

  find(hash: string, uri: string, scope: string): T | undefined {
    return this.map
      .get(hash)
      ?.results.find((r) => r.uri === uri && r.scope === scope)?.result;
  }

  set(hash: string, uri: string, scope: string, result: T): void {
    const results = (this.map.get(hash)?.results ?? []).filter(
      (r) => !(r.uri === uri && r.scope === scope)
    );
    results.push({ scope, uri, result });
    this.map.set(hash, { date: Date.now(), results });
    this.persist();
  }

  delete(hash: string): void {
    if (this.map.delete(hash)) this.persist();
  }
}

let traceCache: PersistentCache<TraceElement[], TraceElement[]> | undefined;

export function initTraceCache(storageDir: string): void {
  traceCache = new PersistentCache<TraceElement[], TraceElement[]>(
    join(storageDir, 'trace-cache.json'),
    (v) => v,
    (v) => v
  );
}

const TRACE_NINJA_FILE = join('_build', '_trace', 'clerk.ninja');

function hashNinjaFile(file: string): string {
  return createHash('sha1').update(readFileSync(file)).digest('hex');
}

function ninjaUpToDate(cwd: string): boolean {
  if (!existsSync(join(cwd, TRACE_NINJA_FILE))) return false;
  try {
    const out = execFileSync('ninja', ['-f', TRACE_NINJA_FILE, '-n'], {
      cwd,
      encoding: 'utf8',
      stdio: ['ignore', 'pipe', 'pipe'],
    });
    return out.includes('no work to do');
  } catch {
    return false;
  }
}

export async function runTrace(
  uri: string,
  scope: string,
  inputs?: JSON
): Promise<TraceResult> {
  const cwd = getCwd(uri) ?? dirname(uri);
  const ninjaFile = join(cwd, TRACE_NINJA_FILE);
  if (traceCache && existsSync(ninjaFile)) {
    const key = hashNinjaFile(ninjaFile);
    if (ninjaUpToDate(cwd)) {
      const cached = traceCache.find(key, uri, scope);
      if (cached !== undefined) {
        logger.log(`Trace for scope "${scope}" (${uri}) served from cache.`);
        return { ok: true, trace: cached };
      }
    } else {
      traceCache.delete(key);
    }
  }
  let tmpDir: string;
  try {
    tmpDir = mkdtempSync(join(tmpdir(), 'catala-trace-'));
  } catch (e) {
    return { ok: false, error: `Cannot create temporary file: ${String(e)}` };
  }
  const traceOutputFile = join(tmpDir, 'trace.json');
  try {
    logger.log(`Running trace for scope "${scope}" (${uri}).`);
    await vscode.commands.executeCommand('catala.runScope', {
      uri,
      scope,
      inputs,
      withTrace: true,
      headless: true,
      traceOutputFile,
      buildDir: '_build/_trace',
      ninjaOutput: '_build/_trace/clerk.ninja',
    });
    const result = readTraceFile(traceOutputFile);
    if (result.ok && existsSync(ninjaFile)) {
      traceCache?.set(hashNinjaFile(ninjaFile), uri, scope, result.trace);
    }
    return result;
  } catch (e) {
    return { ok: false, error: String(e) };
  } finally {
    try {
      rmSync(tmpDir, { recursive: true, force: true });
    } catch {
      // ignore cleanup errors
    }
  }
}
