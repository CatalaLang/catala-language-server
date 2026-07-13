import * as vscode from 'vscode';
import {
  mkdirSync,
  mkdtempSync,
  readFileSync,
  readdirSync,
  rmSync,
  statSync,
  writeFileSync,
} from 'fs';
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

export function hashDir(dir: string): string {
  const h = createHash('sha1');
  const walk = (d: string): void => {
    let entries;
    try {
      entries = readdirSync(d, { withFileTypes: true });
    } catch {
      return;
    }
    entries.sort((a, b) => (a.name < b.name ? -1 : 1));
    for (const e of entries) {
      const p = join(d, e.name);
      if (e.isDirectory()) {
        walk(p);
      } else {
        try {
          const st = statSync(p);
          h.update(p).update(String(st.size)).update(String(st.mtimeMs));
        } catch {
          /* ignore unreadable entries */
        }
      }
    }
  };
  walk(dir);
  return h.digest('hex');
}

type CacheResult<T> = { scope: string; uri: string; result: T };
type CacheBucket<T> = { date: number; results: CacheResult<T>[] };

const CACHE_MAX_AGE_MS = 7 * 24 * 60 * 60 * 1000;

export class PersistentCache<T, S> {
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
    this.persist(); // rewrite the file without the pruned buckets
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
}

let traceCache: PersistentCache<TraceElement[], TraceElement[]> | undefined;

export function initTraceCache(storageDir: string): void {
  traceCache = new PersistentCache<TraceElement[], TraceElement[]>(
    join(storageDir, 'trace-cache.json'),
    (v) => v,
    (v) => v
  );
}

export async function runTrace(
  uri: string,
  scope: string
): Promise<TraceResult> {
  const traceBuildDir = join(getCwd(uri) ?? dirname(uri), '_build', '_trace');
  const cached = traceCache?.find(hashDir(traceBuildDir), uri, scope);
  if (cached !== undefined) {
    logger.log(`Trace for scope "${scope}" (${uri}) served from cache.`);
    return { ok: true, trace: cached };
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
      withTrace: true,
      headless: true,
      traceOutputFile,
      buildDir: '_build/_trace',
    });
    const result = readTraceFile(traceOutputFile);
    if (result.ok) {
      traceCache?.set(hashDir(traceBuildDir), uri, scope, result.trace);
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
