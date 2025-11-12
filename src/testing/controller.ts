import * as vscode from 'vscode';
import type {
  ParseResults,
  TestRunResults,
  Diff,
  TestOutputs,
  SourcePosition,
} from '../generated/test_case';
import { getLanguageFromUri } from '../testCaseEditor';
import { parseTestFile, runTestScope } from '../testCaseCompilerInterop';

type Meta = {
  file: vscode.Uri;
  testingScope: string;
  testedModule: string;
  testedScope: string;
};

const meta = new WeakMap<vscode.TestItem, Meta>();

export function registerCatalaTests(context: vscode.ExtensionContext): void {
  const ctrl = vscode.tests.createTestController('catalaTests', 'Catala Tests');
  context.subscriptions.push(ctrl);

  const EXCLUDE_GLOB = '{**/_build/**}';
  const shouldIgnore = (uri: vscode.Uri): boolean => {
    const p = uri.fsPath.replace(/\\/g, '/');
    return p.includes('/_build/');
  };

  ctrl.resolveHandler = async (item?: vscode.TestItem) => {
    if (!item) {
      const filesEn = await vscode.workspace.findFiles(
        '**/*test*.catala_en',
        EXCLUDE_GLOB
      );
      const filesFr = await vscode.workspace.findFiles(
        '**/*test*.catala_fr',
        EXCLUDE_GLOB
      );
      for (const f of [...filesEn, ...filesFr]) {
        await discover(f);
      }
    } else if (item.uri && !shouldIgnore(item.uri)) {
      await discover(item.uri);
    }
  };

  ctrl.createRunProfile(
    'Run',
    vscode.TestRunProfileKind.Run,
    (req, token) => run(ctrl, req, token),
    false
  );

  // Watch files that look like tests in both languages
  const wEn = vscode.workspace.createFileSystemWatcher('**/*test*.catala_en');
  const wFr = vscode.workspace.createFileSystemWatcher('**/*test*.catala_fr');

  const onCreate = async (uri: vscode.Uri) => {
    if (!shouldIgnore(uri)) await discover(uri);
  };
  const onChange = async (uri: vscode.Uri) => {
    if (!shouldIgnore(uri)) await discover(uri);
  };
  const onDelete = (uri: vscode.Uri) => {
    if (!shouldIgnore(uri)) ctrl.items.delete(uri.toString());
  };

  context.subscriptions.push(
    wEn.onDidCreate(onCreate),
    wEn.onDidChange(onChange),
    wEn.onDidDelete(onDelete),
    wFr.onDidCreate(onCreate),
    wFr.onDidChange(onChange),
    wFr.onDidDelete(onDelete)
  );

  // Initial scan
  vscode.workspace
    .findFiles('**/*test*.catala_en', EXCLUDE_GLOB)
    .then((fs) => fs.forEach(discover));
  vscode.workspace
    .findFiles('**/*test*.catala_fr', EXCLUDE_GLOB)
    .then((fs) => fs.forEach(discover));

  async function discover(uri: vscode.Uri): Promise<void> {
    if (shouldIgnore(uri)) return;
    const fileId = uri.toString();
    const root =
      ctrl.items.get(fileId) ??
      ctrl.createTestItem(fileId, uri.path.split('/').pop() ?? fileId, uri);
    ctrl.items.add(root);

    try {
      const doc = await vscode.workspace.openTextDocument(uri);
      const language = getLanguageFromUri(uri); // 'en' or 'fr'
      const parsed: ParseResults = parseTestFile(
        doc.getText(),
        language,
        uri.fsPath
      );

      if (parsed.kind === 'ParseError') {
        root.children.replace([]);
        root.error = parsed.value;
        return;
      }
      if (parsed.kind === 'EmptyTestListMismatch') {
        // omit root item when there are no tests
        ctrl.items.delete(fileId);
        return;
      }

      const tests = parsed.value;
      if (tests.length === 0) {
        // omit root item for empty files with no tests
        ctrl.items.delete(fileId);
        return;
      }
      root.error = undefined;
      root.children.replace([]);

      for (const t of tests) {
        const id = `${fileId}#${t.testing_scope}`;
        const label =
          t.title && t.title.trim() !== '' ? t.title : t.testing_scope;
        const item = ctrl.createTestItem(id, label, uri);
        const tested = `${t.tested_scope.module_name}.${t.tested_scope.name}`;
        item.description = tested;
        item.tags = [
          new vscode.TestTag(`tested:${tested}`),
          new vscode.TestTag(`testing:${t.testing_scope}`),
        ];
        meta.set(item, {
          file: uri,
          testingScope: t.testing_scope,
          testedModule: t.tested_scope.module_name,
          testedScope: t.tested_scope.name,
        });
        root.children.add(item);
      }
    } catch (e) {
      root.children.replace([]);
      root.error = String(e);
    }
  }

  async function run(
    ctrl: vscode.TestController,
    request: vscode.TestRunRequest,
    token: vscode.CancellationToken
  ) {
    const tr = ctrl.createTestRun(request);
    const queue: vscode.TestItem[] = [];
    const enqueueAll = (items: Iterable<vscode.TestItem>) => {
      for (const i of items) queue.push(i);
    };
    const enqueueFromCollection = (col: vscode.TestItemCollection) => {
      col.forEach((test) => queue.push(test));
    };

    if (request.include) enqueueAll(request.include);
    else enqueueFromCollection(ctrl.items);

    while (queue.length && !token.isCancellationRequested) {
      const item = queue.pop()!;
      if (item.children.size) {
        enqueueFromCollection(item.children);
        continue;
      }

      let m = meta.get(item);
      if (!m) {
        // Attempt to resolve this item (load its children or metadata)
        if (item.uri) {
          await discover(item.uri);
        } else if (ctrl.resolveHandler) {
          await ctrl.resolveHandler(item);
        }

        // After resolving, if it now has children, traverse them
        if (item.children.size) {
          enqueueFromCollection(item.children);
          continue;
        }

        // Try to get meta again
        m = meta.get(item);
        if (!m) {
          // Nothing to run for this item, record as skipped to avoid "no output"
          tr.skipped(item);
          continue;
        }
      }

      tr.enqueued(item);
      tr.started(item);

      try {
        const res: TestRunResults = await runTestScope(
          m.file.fsPath,
          m.testingScope
        );
        if (res.kind === 'Ok') {
          const out = res.value;
          const diffs = out.diffs ?? [];
          const hasFailures = out.assert_failures || diffs.length > 0;
          if (hasFailures) {
            const msg = new vscode.TestMessage(formatDiffs(diffs));
            const loc = firstDiffLocation(diffs, out.test_outputs, m.file);
            if (loc) msg.location = loc;
            tr.failed(item, msg);
          } else {
            tr.passed(item);
          }
        } else if (res.kind === 'Cancelled') {
          tr.skipped(item);
        } else {
          tr.errored(item, new vscode.TestMessage(res.value));
        }
      } catch (e) {
        tr.errored(item, new vscode.TestMessage(String(e)));
      }
    }

    tr.end();
  }
}

function firstDiffLocation(
  diffs: Diff[],
  outputs: TestOutputs,
  file: vscode.Uri
): vscode.Location | undefined {
  if (!diffs.length) return;
  const first = diffs[0];
  const seg0 = first.path[0];
  if (!seg0 || seg0.kind !== 'StructField') return;

  const field = seg0.value;
  const out = outputs.get(field);
  const pos: SourcePosition | undefined = out?.value?.pos;
  if (!pos) return;

  // SourcePosition is 1-based; VS Code expects 0-based
  const range = new vscode.Range(
    new vscode.Position(pos.start_line - 1, pos.start_column - 1),
    new vscode.Position(pos.end_line - 1, pos.end_column - 1)
  );
  return new vscode.Location(file, range);
}

function formatDiffs(diffs: Diff[]): string {
  const seg = (s: Diff['path'][number]) => {
    switch (s.kind) {
      case 'StructField':
        return `.${s.value}`;
      case 'ListIndex':
        return `[${s.value}]`;
      case 'TupleIndex':
        return `(${s.value})`;
      case 'EnumPayload':
        return `<${s.value}>`;
    }
  };
  const pp = (rv: any): string => {
    switch (rv.value.kind) {
      case 'Bool':
      case 'Integer':
      case 'Decimal':
        return String(rv.value.value);
      case 'Money':
        return `$${(rv.value.value / 100).toFixed(2)}`;
      case 'Date': {
        const d = rv.value.value;
        return `|${d.year}-${String(d.month).padStart(2, '0')}-${String(d.day).padStart(2, '0')}|`;
      }
      case 'Duration':
        return JSON.stringify(rv.value.value);
      case 'Enum':
        return `${rv.value.value[0].enum_name}.${rv.value.value[1][0]}`;
      case 'Struct':
        return rv.value.value[0].struct_name;
      case 'Array':
        return `[${rv.value.value.map(pp).join(', ')}]`;
      default:
        return '<value>';
    }
  };
  return diffs
    .map(
      (d) =>
        `Diff at ${d.path.map(seg).join('')}:\n  expected: ${pp(
          d.expected
        )}\n  actual:   ${pp(d.actual)}`
    )
    .join('\n\n');
}
