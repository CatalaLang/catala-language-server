import { useEffect, useState, type ReactElement, type ReactNode } from 'react';

/** Shows its content for a moment after it mounts. To show it again, remount
 *  it by changing its `key`, e.g. to a counter. */
export function Hint({
  children,
}: {
  children: ReactNode;
}): ReactElement | null {
  const [shown, setShown] = useState(true);
  useEffect(() => {
    const timer = setTimeout(() => setShown(false), 1200);
    return (): void => clearTimeout(timer);
  }, []);
  return shown ? (
    <span className="hint" role="status">
      {children}
    </span>
  ) : null;
}
