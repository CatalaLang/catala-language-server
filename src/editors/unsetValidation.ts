/**
 * Utilities for detecting and navigating to Unset/Invalid values in RuntimeValue trees.
 * Note: NotOverridden (context var using computed default) is intentionally
 * not treated as Unset — it is valid and does not block test runs.
 */

import type { RuntimeValue, Test, Typ } from '../generated/catala_types';

/** Recursively checks whether a RuntimeValue contains any Unset values. */
function containsUnset(rv: RuntimeValue): boolean {
  switch (rv.value.kind) {
    case 'Unset':
      return true;
    case 'Array':
      return rv.value.value.some(containsUnset);
    case 'Struct': {
      const map = rv.value.value[1];
      return Array.from(map.values()).some(containsUnset);
    }
    case 'Enum': {
      const payload = rv.value.value[1][1];
      return payload?.value ? containsUnset(payload.value) : false;
    }
    default:
      return false;
  }
}

/**
 * Scrolls to and focuses the first `.value-editor.invalid` or
 * `.value-editor.unset` element within a container.
 *
 * @param container - The container to search within (defaults to document)
 * @param delay - Optional delay in ms before scrolling (defaults to 0)
 */
export function scrollToFirstInvalidOrUnset(
  container: HTMLElement | Document = document,
  delay: number = 0
): void {
  setTimeout(() => {
    const el = container.querySelector(
      '.value-editor.invalid, .value-editor.unset'
    ) as HTMLElement | null;
    if (el) {
      el.scrollIntoView({ behavior: 'smooth', block: 'center' });
      const focusable = el.querySelector('input, select') as HTMLElement | null;
      focusable?.focus?.();
    }
  }, delay);
}

/** An Unset weighed by its type: a blank record counts its declared
 *  fields, recursively; anything else counts once. */
function holesOfType(typ: Typ): number {
  if (typ.kind === 'TStruct') {
    return [...typ.value.fields.values()].reduce(
      (n, t) => n + holesOfType(t),
      0
    );
  }
  return 1;
}

function countUnset(rv: RuntimeValue, typ: Typ | undefined): number {
  switch (rv.value.kind) {
    case 'Unset':
      return typ === undefined ? 1 : holesOfType(typ);
    case 'Array': {
      const elt = typ?.kind === 'TArray' ? typ.value : undefined;
      return rv.value.value.reduce((n, v) => n + countUnset(v, elt), 0);
    }
    case 'Struct': {
      const fields = typ?.kind === 'TStruct' ? typ.value.fields : undefined;
      const map = rv.value.value[1];
      return [...map.entries()].reduce(
        (n, [name, v]) => n + countUnset(v, fields?.get(name)),
        0
      );
    }
    case 'Enum': {
      const [, [ctor, payload]] = rv.value.value;
      if (!payload?.value) return 0;
      const pTyp =
        typ?.kind === 'TEnum'
          ? (typ.value.constructors.get(ctor)?.value ?? undefined)
          : undefined;
      return countUnset(payload.value, pTyp);
    }
    default:
      return 0;
  }
}

/** How many values inside [rv] are still to fill, weighed by [typ]. */
export function countUnsetIn(rv: RuntimeValue | undefined, typ: Typ): number {
  return rv === undefined ? 0 : countUnset(rv, typ);
}

/** How many values (inputs and outputs together) are still to fill. */
export function countUnsetValues(test: Test): number {
  const holes = (io: { typ: Typ; value?: { value: RuntimeValue } }): number =>
    io.value === undefined ? 0 : countUnset(io.value.value, io.typ);
  return (
    [...test.test_inputs.values()].reduce((n, io) => n + holes(io), 0) +
    [...test.test_outputs.values()].reduce((n, io) => n + holes(io), 0)
  );
}

export function hasUnsetInTest(
  test: Test,
  options: {
    checkInputs?: boolean;
    checkOutputs?: boolean;
  } = {}
): boolean {
  const { checkInputs = true, checkOutputs = true } = options;

  const inputsHas = checkInputs
    ? Array.from(test.test_inputs.values()).some(
        (io) => io.value && containsUnset(io.value.value)
      )
    : false;

  const outputsHas = checkOutputs
    ? Array.from(test.test_outputs.values()).some(
        (io) => io.value && containsUnset(io.value.value)
      )
    : false;

  return inputsHas || outputsHas;
}
