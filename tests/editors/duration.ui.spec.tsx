import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import enMessages from '../../src/locales/en.json';
import ValueEditor from '../../src/editors/ValueEditors';
import type { TestIo, Typ } from '../../src/generated/catala_types';
import { useState } from 'react';

function renderEditor(
  typ: Typ,
  onValueChange = vi.fn(),
  value?: TestIo['value']
) {
  const utils = render(
    <IntlProvider locale="en" messages={enMessages}>
      <ValueEditor
        testIO={{ typ, value }}
        onValueChange={onValueChange}
        currentPath={[]}
        diffs={[]}
      />
    </IntlProvider>
  );
  return { ...utils, onValueChange };
}

function ControlledEditor({
  typ,
  onValueChange,
}: {
  typ: Typ;
  onValueChange: (ti: TestIo) => void;
}) {
  const [value, setValue] = useState<TestIo['value']>();
  return (
    <IntlProvider locale="en" messages={enMessages}>
      <ValueEditor
        testIO={{ typ, value }}
        onValueChange={(ti) => {
          setValue(ti.value);
          onValueChange(ti);
        }}
        currentPath={[]}
        diffs={[]}
      />
    </IntlProvider>
  );
}

describe('DurationEditor with negative values', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it('emits Duration on blur when only one field is set (others default to 0) and preserves input, including negatives', async () => {
    const { onValueChange } = renderEditor({ kind: 'TDuration' });

    const years = screen.getByLabelText(/Years:/i) as HTMLInputElement;
    const months = screen.getByLabelText(/Months:/i) as HTMLInputElement;
    const days = screen.getByLabelText(/Days:/i) as HTMLInputElement;

    // Start with partial input: only years set (negative)
    fireEvent.change(years, { target: { value: '-2' } });

    // Sanity check: local state reflects typed value
    expect(years.value).toBe('-2');
    expect(months.value).toBe('');
    expect(days.value).toBe('');

    // Blur a field -> should emit normalized Duration (-2,0,0)
    fireEvent.blur(years);

    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({
              kind: 'Duration',
              value: { years: -2, months: 0, days: 0 },
            }),
          }),
        }),
      })
    );

    // UI should keep user input
    await waitFor(() => {
      expect(years.value).toBe('-2');
      expect(months.value).toBe('');
      expect(days.value).toBe('');
    });
  });

  it('emits Duration when only years is set (months/days default to 0), with negative support', () => {
    const { onValueChange } = renderEditor({ kind: 'TDuration' });

    const years = screen.getByLabelText(/Years:/i) as HTMLInputElement;
    fireEvent.change(years, { target: { value: '-3' } });
    fireEvent.blur(years);

    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({
              kind: 'Duration',
              value: { years: -3, months: 0, days: 0 },
            }),
          }),
        }),
      })
    );
  });

  it('emits Duration when only months is set (years/days default to 0), with negative support', () => {
    const { onValueChange } = renderEditor({ kind: 'TDuration' });

    const months = screen.getByLabelText(/Months:/i) as HTMLInputElement;
    fireEvent.change(months, { target: { value: '-5' } });
    fireEvent.blur(months);

    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({
              kind: 'Duration',
              value: { years: 0, months: -5, days: 0 },
            }),
          }),
        }),
      })
    );
  });

  it('emits Duration when only days is set (years/months default to 0), with negative support', () => {
    const { onValueChange } = renderEditor({ kind: 'TDuration' });

    const days = screen.getByLabelText(/Days:/i) as HTMLInputElement;
    fireEvent.change(days, { target: { value: '-12' } });
    fireEvent.blur(days);

    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({
              kind: 'Duration',
              value: { years: 0, months: 0, days: -12 },
            }),
          }),
        }),
      })
    );
  });

  it('does not auto-fill 0 into years when only months is set (negative months)', async () => {
    const onValueChange = vi.fn();
    render(
      <ControlledEditor
        typ={{ kind: 'TDuration' }}
        onValueChange={onValueChange}
      />
    );

    const years = screen.getByLabelText(/Years:/i) as HTMLInputElement;
    const months = screen.getByLabelText(/Months:/i) as HTMLInputElement;
    const days = screen.getByLabelText(/Days:/i) as HTMLInputElement;

    fireEvent.change(months, { target: { value: '-5' } });

    // We do emit a normalized Duration (0,-5,0)…
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({
              kind: 'Duration',
              value: { years: 0, months: -5, days: 0 },
            }),
          }),
        }),
      })
    );

    // …but the other fields should remain visually blank
    await waitFor(() => {
      expect(years.value).toBe('');
      expect(months.value).toBe('-5');
      expect(days.value).toBe('');
    });
  });

  it('does not auto-fill 0 into years or months when only days is set (negative days)', async () => {
    const onValueChange = vi.fn();
    render(
      <ControlledEditor
        typ={{ kind: 'TDuration' }}
        onValueChange={onValueChange}
      />
    );

    const years = screen.getByLabelText(/Years:/i) as HTMLInputElement;
    const months = screen.getByLabelText(/Months:/i) as HTMLInputElement;
    const days = screen.getByLabelText(/Days:/i) as HTMLInputElement;

    fireEvent.change(days, { target: { value: '-7' } });

    // We emit a normalized Duration (0,0,-7)…
    expect(onValueChange).toHaveBeenLastCalledWith(
      expect.objectContaining({
        value: expect.objectContaining({
          value: expect.objectContaining({
            value: expect.objectContaining({
              kind: 'Duration',
              value: { years: 0, months: 0, days: -7 },
            }),
          }),
        }),
      })
    );

    // …but years and months should remain visually blank
    await waitFor(() => {
      expect(years.value).toBe('');
      expect(months.value).toBe('');
      expect(days.value).toBe('-7');
    });
  });
});
