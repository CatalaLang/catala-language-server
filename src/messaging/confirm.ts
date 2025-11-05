import { writeUpMessage, type ConfirmAction } from '../generated/test_case';
import { getVsCodeApi } from '../webviewApi';

type ConfirmActionKind = ConfirmAction['kind'];

let nextId = 1;
const pending = new Map<number, (confirmed: boolean) => void>();

export function confirm(actionKind: ConfirmActionKind): Promise<boolean> {
  return new Promise<boolean>((resolve) => {
    const id = nextId++;
    pending.set(id, resolve);
    const action: ConfirmAction = { kind: actionKind };
    getVsCodeApi().postMessage(
      writeUpMessage({
        kind: 'ConfirmRequest',
        value: { id, action },
      })
    );
  });
}

export function resolveConfirmResult(id: number, confirmed: boolean): void {
  const resolver = pending.get(id);
  if (resolver) {
    pending.delete(id);
    resolver(confirmed);
  }
}
