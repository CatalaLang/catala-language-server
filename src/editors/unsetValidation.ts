/**
 * Utilities for detecting and navigating to Unset/Invalid values in RuntimeValue trees.
 * Note: NotOverridden (context var using computed default) is intentionally
 * not treated as Unset — it is valid and does not block test runs.
 */

import type {
  PathSegment,
  RuntimeValue,
  Test,
} from '../generated/catala_types';

/** Recursively checks whether a RuntimeValue contains any Unset values. */
export function containsUnset(rv: RuntimeValue): boolean {
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
    const els = Array.from(
      container.querySelectorAll('.value-editor.invalid, .value-editor.unset')
    ) as HTMLElement[];
    const el = els.find((e) => e.offsetParent !== null) ?? els[0];
    if (el) {
      el.scrollIntoView({ behavior: 'smooth', block: 'center' });
      const focusable = el.querySelector('input, select') as HTMLElement | null;
      focusable?.focus?.();
    }
  }, delay);
}

function unsetPathIn(
  rv: RuntimeValue,
  prefix: PathSegment[]
): PathSegment[] | undefined {
  const raw = rv.value;
  switch (raw.kind) {
    case 'Unset':
      return prefix;
    case 'Array': {
      for (const [i, item] of raw.value.entries()) {
        const found = unsetPathIn(item, [
          ...prefix,
          { kind: 'ListIndex', value: i },
        ]);
        if (found) return found;
      }
      return undefined;
    }
    case 'Struct': {
      for (const [name, v] of raw.value[1]) {
        const found = unsetPathIn(v, [
          ...prefix,
          { kind: 'StructField', value: name },
        ]);
        if (found) return found;
      }
      return undefined;
    }
    case 'Enum': {
      // A payload is its own node, named by its constructor as diff paths do.
      const [ctor, payload] = raw.value[1];
      return payload?.value
        ? unsetPathIn(payload.value, [
            ...prefix,
            { kind: 'EnumPayload', value: ctor },
          ])
        : undefined;
    }
    default:
      return undefined;
  }
}

/** The path of the first value still to fill, inputs before outputs. */
export function firstUnsetPath(test: Test): PathSegment[] | undefined {
  for (const [name, io] of [...test.test_inputs, ...test.test_outputs]) {
    if (io.value === undefined) continue;
    const found = unsetPathIn(io.value.value, [
      { kind: 'StructField', value: name },
    ]);
    if (found) return found;
  }
  return undefined;
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
