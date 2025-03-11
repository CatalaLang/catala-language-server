import { type ReactElement } from 'react';
import type { ValidationError } from '../validation';

/**
 * Component to display validation errors
 */
export function ValidationErrorDisplay({
  errors,
}: {
  errors: ValidationError[];
}): ReactElement | null {
  if (errors.length === 0) return null;

  return (
    <div className="validation-errors">
      {errors.map((error, index) => (
        <div key={index} className="validation-error">
          {error.message}
        </div>
      ))}
    </div>
  );
}
