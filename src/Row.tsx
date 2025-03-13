import type { ReactElement, ReactNode } from 'react';

type Props = {
  label: string;
  children: ReactNode;
  className?: string;
};

export default function Row({
  label,
  children,
  className = '',
}: Props): ReactElement {
  return (
    <tr className={className}>
      <td>
        <strong className="identifier">{label}</strong>
      </td>
      <td>{children}</td>
    </tr>
  );
}
