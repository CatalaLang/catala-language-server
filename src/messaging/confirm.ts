import { writeUpMessage, type ConfirmAction } from '../generated/catala_types';
import { getVsCodeApi } from '../shared/webviewApi';

/** The actions that carry nothing but their kind. */
type PlainConfirmKind = Extract<
  ConfirmAction,
  { kind: string } & { value?: never }
>['kind'];

let nextId = 1;
const pending = new Map<number, (confirmed: boolean) => void>();

export function confirm(
  request: PlainConfirmKind | ConfirmAction
): Promise<boolean> {
  return new Promise<boolean>((resolve) => {
    const id = nextId++;
    pending.set(id, resolve);
    const action: ConfirmAction =
      typeof request === 'string'
        ? ({ kind: request } as ConfirmAction)
        : request;
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
