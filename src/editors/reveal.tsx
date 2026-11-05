import {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useRef,
  useState,
} from 'react';
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

type ViewChoice =
  | { kind: 'tab'; tab: string }
  | { kind: 'cards'; cards: boolean };

type LinkedChoice = ViewChoice & { path: PathSegment[]; nonce: number };

type LinkedViews = {
  last: LinkedChoice | undefined;
  choose(path: PathSegment[], choice: ViewChoice): void;
};

export const LinkedViewsContext = createContext<LinkedViews | undefined>(
  undefined
);

export function useLinkedViewsState(): LinkedViews {
  const [last, setLast] = useState<LinkedChoice | undefined>(undefined);
  const choose = useCallback(
    (path: PathSegment[], choice: ViewChoice): void =>
      setLast((l) => ({ ...choice, path, nonce: (l?.nonce ?? 0) + 1 })),
    []
  );
  return useMemo(() => ({ last, choose }), [last, choose]);
}

export function useLinkedView(
  path: PathSegment[] | undefined,
  follow: (choice: ViewChoice) => void
): (choice: ViewChoice) => void {
  const linked = useContext(LinkedViewsContext);
  const followed = useRef<number | undefined>(undefined);
  const last = linked?.last;
  useEffect(() => {
    if (
      last === undefined ||
      path === undefined ||
      last.nonce === followed.current ||
      last.path.length !== path.length ||
      !pathStartsWith(last.path, path)
    )
      return;
    followed.current = last.nonce;
    follow(last);
  });
  return (choice) => {
    if (path !== undefined) linked?.choose(path, choice);
  };
}
