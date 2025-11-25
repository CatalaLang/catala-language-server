import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../src/locales/en.json';
import type {
  Diff,
  PathSegment,
  RuntimeValue,
  RuntimeValueRaw,
  Typ,
  ValueDef,
} from '../src/generated/test_case';
import { ArrayEditor } from '../src/editors/ArrayEditor';

// Mock confirm to auto-approve destructive actions
vi.mock('../src/messaging/confirm', () => ({
  confirm: async () => true,
}));

function rv(value: RuntimeValueRaw): RuntimeValue {
  return { value, attrs: [] };
}
function arrayValueDef(items: RuntimeValue[]): ValueDef {
  return { value: rv({ kind: 'Array', value: items }) };
}
function intRV(n: number): RuntimeValue {
  return rv({ kind: 'Integer', value: n });
}
function emptyRV(): RuntimeValue {
  return rv({ kind: 'Empty' } as any);
}

describe('ArrayEditor - presence actions and invalidation', () => {
  const elementType: Typ = { kind: 'TInt' };

  beforeEach(() => {
    vi.clearAllMocks();
  });

  it('shows Add to expected only for appendable actual-only indices and resolves on click', () => {
    const currentPath: PathSegment[] = [];
    const diffs: Diff[] = [
      {
        path: [{ kind: 'ListIndex', value: 0 }],
        expected: emptyRV(),
        actual: intRV(10),
      } as unknown as Diff,
      {
        path: [{ kind: 'ListIndex', value: 1 }],
        expected: emptyRV(),
        actual: intRV(20),
      } as unknown as Diff,
    ];

    const onDiffResolved = vi.fn();

    render(
      <IntlProvider locale="en" messages={enMessages}>
        <ArrayEditor
          elementType={elementType}
          valueDef={arrayValueDef([])}
          onValueChange={() => {}}
          currentPath={currentPath}
          diffs={diffs}
          onDiffResolved={onDiffResolved}
        />
      </IntlProvider>
    );

    const addButtons = screen.getAllByRole('button', {
      name: /Add to expected/i,
    });
    expect(addButtons).toHaveLength(1);

    fireEvent.click(addButtons[0]);
    expect(onDiffResolved).toHaveBeenCalledTimes(1);
    expect(onDiffResolved).toHaveBeenCalledWith([
      { kind: 'ListIndex', value: 0 },
    ]);
  });

  it('shows Remove from expected only for last index when expected-only and resolves on click', async () => {
    const currentPath: PathSegment[] = [];
    const diffs: Diff[] = [
      {
        path: [{ kind: 'ListIndex', value: 0 }],
        expected: intRV(1),
        actual: emptyRV(),
      } as unknown as Diff,
      {
        path: [{ kind: 'ListIndex', value: 1 }],
        expected: intRV(2),
        actual: emptyRV(),
      } as unknown as Diff,
    ];

    const onDiffResolved = vi.fn();

    render(
      <IntlProvider locale="en" messages={enMessages}>
        <ArrayEditor
          elementType={elementType}
          valueDef={arrayValueDef([intRV(1), intRV(2)])}
          onValueChange={() => {}}
          currentPath={currentPath}
          diffs={diffs}
          onDiffResolved={onDiffResolved}
        />
      </IntlProvider>
    );

    const removeButtons = screen.getAllByRole('button', {
      name: /Remove from expected/i,
    });
    expect(removeButtons).toHaveLength(1);

    fireEvent.click(removeButtons[0]);
    await waitFor(() => {
      expect(onDiffResolved).toHaveBeenCalledTimes(1);
      expect(onDiffResolved).toHaveBeenCalledWith([
        { kind: 'ListIndex', value: 1 },
      ]);
    });
  });

  it('invalidates diffs on structural edits: add, move, delete', () => {
    const currentPath: PathSegment[] = [{ kind: 'StructField', value: 'out' }];

    const onInvalidateDiffs = vi.fn();

    const { container } = render(
      <IntlProvider locale="en" messages={enMessages}>
        <ArrayEditor
          elementType={elementType}
          valueDef={arrayValueDef([intRV(1), intRV(2)])}
          onValueChange={() => {}}
          currentPath={currentPath}
          diffs={[]}
          onInvalidateDiffs={onInvalidateDiffs}
        />
      </IntlProvider>
    );

    const addBtn = container.querySelector(
      'button.button-action-dvp.body-b3'
    ) as HTMLButtonElement;
    fireEvent.click(addBtn);

    const moveNext = container.querySelector(
      '.array-item .array-item-controls .array-move-next'
    ) as HTMLButtonElement;
    fireEvent.click(moveNext);

    const deleteBtn = container.querySelector(
      '.array-item .array-item-controls .array-delete'
    ) as HTMLButtonElement;
    fireEvent.click(deleteBtn);

    expect(onInvalidateDiffs).toHaveBeenCalled();
    expect(onInvalidateDiffs).toHaveBeenCalledWith(currentPath);
  });
});
