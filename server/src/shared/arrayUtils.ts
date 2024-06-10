/* eslint-disable @typescript-eslint/no-unnecessary-type-constraint */

export function mergeArrays<T extends any>(arr1: T[], arr2: T[]): T[] {
  return Array.from(new Set([...(arr1 ?? []), ...(arr2 ?? [])]));
}

export function intersectArrays<T extends any>(arr1: T[], arr2: T[]): T[] {
  return arr1.filter((value) => arr2.includes(value));
}

/**
 * [ [ 'scope_decl_item', 'scope_decl' ],
 *   [ 'scope_decl_item', 'scope_decl' ] ],
 */
export function arrayAlreadyIncludesArray<T extends any>(
  arr: T[][],
  element: T[]
): boolean {
  const result = arr.find((item, index, self) => {
    const isSame = JSON.stringify(item) === JSON.stringify(element);
    return isSame;
    const stringifiedValue = JSON.stringify(item);
    return (
      index === self.findIndex((v) => JSON.stringify(v) === stringifiedValue)
    );
  });
  return !!result;
}

export function areArraysEqual<T extends any>(arr1: T[], arr2: T[]): boolean {
  return (
    arr1.length === arr2.length && arr1.every((value) => arr2.includes(value))
  );
}
