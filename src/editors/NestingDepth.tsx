import {
  createContext,
  useContext,
  type ReactElement,
  type ReactNode,
} from 'react';

const NestingDepthContext = createContext(0);

export function useNestingDepth(): number {
  return useContext(NestingDepthContext);
}

export function NestingDepthIncrementer({
  children,
}: {
  children: ReactNode;
}): ReactElement {
  const depth = useContext(NestingDepthContext);
  return (
    <NestingDepthContext.Provider value={depth + 1}>
      {children}
    </NestingDepthContext.Provider>
  );
}
