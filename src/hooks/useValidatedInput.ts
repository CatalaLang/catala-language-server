import { useState, useEffect, useRef, useCallback } from 'react';
import type { ValidationError } from '../validation';
import { registerValidationErrors, clearValidationErrors } from '../validation';

// Debounce function
function debounce<T extends (...args: any[]) => any>(
  func: T,
  wait: number
): (...args: Parameters<T>) => void {
  let timeout: ReturnType<typeof setTimeout> | null = null;

  return function (...args: Parameters<T>): void {
    const later = () => {
      timeout = null;
      func(...args);
    };

    if (timeout !== null) {
      clearTimeout(timeout);
    }
    timeout = setTimeout(later, wait);
  };
}

/**
 * Custom hook to handle validated input fields
 */
export function useValidatedInput<T>({
  initialValue,
  validator,
  onValidChange,
  validationId,
  parseValue,
  debounceMs = 500, // Default debounce time of 500ms
  validateOnChange = false, // Whether to validate on change (in addition to debounce)
}: {
  initialValue: T | undefined;
  validator: (value: string) => ValidationError[];
  onValidChange: (value: T) => void;
  validationId: string;
  parseValue: (value: string) => T;
  debounceMs?: number;
  validateOnChange?: boolean;
}): {
  inputValue: string;
  errors: ValidationError[];
  handleChange: (evt: React.ChangeEvent<HTMLInputElement>) => void;
  handleBlur: () => void;
  handleKeyDown: (evt: React.KeyboardEvent<HTMLInputElement>) => void;
  inputRef: React.RefObject<HTMLInputElement>;
  validate: () => void;
} {
  const [inputValue, setInputValue] = useState(
    initialValue !== null && initialValue !== undefined
      ? initialValue.toString()
      : ''
  );
  const [errors, setErrors] = useState<ValidationError[]>([]);
  const inputRef = useRef<HTMLInputElement>(null);

  // Update internal state when props change
  useEffect(() => {
    if (initialValue !== undefined && initialValue !== null) {
      setInputValue(initialValue.toString());
    }
  }, [initialValue]);

  // Validation function
  const validate = useCallback((): void => {
    const validationErrors = validator(inputValue);
    setErrors(validationErrors);
    registerValidationErrors(validationId, validationErrors);

    // If valid, update the value
    if (validationErrors.length === 0 && inputValue.trim() !== '') {
      onValidChange(parseValue(inputValue));
    }
  }, [inputValue, validator, validationId, onValidChange, parseValue]);

  // Create debounced validation function
  const debouncedValidate = useCallback(
    debounce(() => validate(), debounceMs),
    [validate, debounceMs]
  );

  const handleChange = (evt: React.ChangeEvent<HTMLInputElement>): void => {
    const newValue = evt.target.value;
    setInputValue(newValue);

    // Clear errors during typing for better UX
    if (errors.length > 0) {
      setErrors([]);
      clearValidationErrors(validationId);
    }

    // Trigger debounced validation
    debouncedValidate();

    // If validateOnChange is true, also validate immediately
    if (validateOnChange) {
      validate();
    }
  };

  const handleBlur = (): void => {
    // Validate immediately on blur
    validate();
  };

  const handleKeyDown = (evt: React.KeyboardEvent<HTMLInputElement>): void => {
    if (evt.key === 'Enter') {
      inputRef.current?.blur();
    }
  };

  // Clean up validation on unmount
  useEffect(() => {
    return () => {
      clearValidationErrors(validationId);
    };
  }, [validationId]);

  return {
    inputValue,
    errors,
    handleChange,
    handleBlur,
    handleKeyDown,
    inputRef,
    validate,
  };
}
