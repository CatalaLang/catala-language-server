/**
 * Utilities for detecting and navigating to Unset/Invalid values in RuntimeValue trees.
 */

import type { RuntimeValue, Test } from '../generated/catala_types';

/**
 * Recursively checks if a RuntimeValue contains any Unset values.
 */
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
 * Scrolls to and focuses the first .invalid-badge or .unset-badge element
 * within a container.
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
      '.invalid-badge, .unset-badge'
    ) as HTMLElement | null;
    if (el) {
      el.scrollIntoView({ behavior: 'smooth', block: 'center' });
      el?.focus?.();
    }
  }, delay);
}

/**
 * Checks if a Test has any Unset values in inputs and/or outputs.
 */
export function hasUnsetInTest(
  test: Test,
  options: { checkInputs?: boolean; checkOutputs?: boolean } = {}
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
