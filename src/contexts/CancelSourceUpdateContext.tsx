import React, { createContext } from 'react';

export type CancelSourceUpdateCallback = () => void;

export interface ICancelSourceUpdateProviderProps {
  children: React.ReactNode;
  onCancelSourceUpdate: CancelSourceUpdateCallback;
}

// Create context with explicit type, null as initial value
const CancelSourceUpdateContext =
  createContext<CancelSourceUpdateCallback | null>(null);

// Create provider component with typed props
export function CancelSourceUpdateProvider({
  children,
  onCancelSourceUpdate,
}: ICancelSourceUpdateProviderProps): JSX.Element {
  return (
    <CancelSourceUpdateContext.Provider value={onCancelSourceUpdate}>
      {children}
    </CancelSourceUpdateContext.Provider>
  );
}

// Export the context as a named export
export { CancelSourceUpdateContext };

// Also export as default for convenience
export default CancelSourceUpdateContext;
