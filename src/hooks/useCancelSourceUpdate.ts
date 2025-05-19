import { useContext } from 'react';
import {
  CancelSourceUpdateContext,
  type CancelSourceUpdateCallback,
} from '../contexts/CancelSourceUpdateContext';

/**
 * Custom hook for consuming the cancel source update context
 * @returns A function to cancel source updates
 * @throws Error when used outside of CancelSourceUpdateProvider
 */
export function useCancelSourceUpdate(): CancelSourceUpdateCallback {
  const context = useContext(CancelSourceUpdateContext);

  if (context === null) {
    throw new Error(
      'useCancelSourceUpdate must be used within a CancelSourceUpdateProvider'
    );
  }

  return context;
}

// Export as default for convenience
export default useCancelSourceUpdate;
