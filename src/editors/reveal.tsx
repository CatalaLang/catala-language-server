import { createContext, useContext } from 'react';
import type { PathSegment } from '../generated/catala_types';

/** A request to bring the editor at [path] into view: tabs on the way
 *  activate themselves. [nonce] distinguishes repeated requests. */
export type Reveal = { path: PathSegment[]; nonce: number };

export const RevealContext = createContext<Reveal | undefined>(undefined);

export function useReveal(): Reveal | undefined {
  return useContext(RevealContext);
}

function sameSegment(a: PathSegment, b: PathSegment): boolean {
  return a.kind === b.kind && a.value === b.value;
}

export function pathStartsWith(
  path: PathSegment[],
  prefix: PathSegment[]
): boolean {
  return (
    prefix.length <= path.length &&
    prefix.every((seg, i) => sameSegment(seg, path[i]))
  );
}
