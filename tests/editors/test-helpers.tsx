import React from 'react';
import { render } from '@testing-library/react';
import { IntlProvider } from 'react-intl';
import { vi } from 'vitest';
import enMessages from '../../src/locales/en.json';
import ValueEditor from '../../src/editors/ValueEditors';
import type { TestIo, Typ } from '../../src/generated/catala_types';

export function renderEditor(
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

// Helper to extract the runtime value from the deeply nested mock call
function getLastCallValue(onValueChange: any) {
  if (!onValueChange || !onValueChange.mock) return null;
  const calls = onValueChange.mock.calls;
  if (!calls || calls.length === 0) return null;
  const lastCall = calls[calls.length - 1];
  if (!lastCall || lastCall.length === 0) return null;
  // The structure is: TestIo -> value (optional) -> value (RuntimeValue) -> value (RuntimeValueRaw)
  const testIO = lastCall[0];
  if (!testIO) return null;
  // testIO.value is the optional value field
  // testIO.value.value is the RuntimeValue
  // testIO.value.value.value is the RuntimeValueRaw
  return testIO.value?.value?.value;
}

// Helper to check the kind and optionally the value
export function expectValueKind(onValueChange: any, kind: string, value?: any) {
  const runtimeValue = getLastCallValue(onValueChange);
  expect(runtimeValue).toBeDefined();
  expect(runtimeValue?.kind).toBe(kind);
  if (value !== undefined) {
    expect(runtimeValue?.value).toEqual(value);
  }
}
