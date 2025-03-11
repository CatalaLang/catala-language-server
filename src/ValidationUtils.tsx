import { type ReactElement, useState, useEffect, useRef } from 'react';

// Simple error message component
export function ValidationError({
  message,
}: {
  message: string;
}): ReactElement | null {
  if (!message) return null;

  return (
    <div className="validation-error" role="alert">
      {message}
    </div>
  );
}

// Type for validation result
export type ValidationResult = {
  isValid: boolean;
  message: string;
};

// Custom hook for debounced validation
export function useDebounceValidation<T>(
  value: string,
  validator: (value: string) => ValidationResult,
  onValidValue: (value: T) => void,
  parser: (value: string) => T,
  debounceTime: number = 300
): [string, ValidationResult, (newValue: string) => void] {
  const [inputValue, setInputValue] = useState<string>(value);
  const [validationResult, setValidationResult] = useState<ValidationResult>({
    isValid: true,
    message: '',
  });
  const debounceTimerRef = useRef<number | null>(null);

  // Update validation with debounce
  const validateWithDebounce = (newValue: string) => {
    // Clear any existing timer
    if (debounceTimerRef.current !== null) {
      window.clearTimeout(debounceTimerRef.current);
    }

    // Set a new timer
    debounceTimerRef.current = window.setTimeout(() => {
      const result = validator(newValue);
      setValidationResult(result);

      // If valid, call the onValidValue callback with parsed value
      if (result.isValid) {
        onValidValue(parser(newValue));
      }
    }, debounceTime);
  };

  // Handle input changes
  const handleChange = (newValue: string) => {
    setInputValue(newValue);
    validateWithDebounce(newValue);
  };

  // Clean up timer on unmount
  useEffect(() => {
    return () => {
      if (debounceTimerRef.current !== null) {
        window.clearTimeout(debounceTimerRef.current);
      }
    };
  }, []);

  // Update internal state when external value changes
  useEffect(() => {
    setInputValue(value);
    setValidationResult({ isValid: true, message: '' });
  }, [value]);

  return [inputValue, validationResult, handleChange];
}

// Common validators
export const validators = {
  integer: (value: string): ValidationResult => {
    if (value === '') {
      return { isValid: false, message: 'Value cannot be empty' };
    }

    // Allow partial input like "-" that could become valid
    if (value === '-') {
      return { isValid: false, message: 'Please complete the number' };
    }

    // Check if it's a valid integer
    if (!/^-?\d+$/.test(value)) {
      return { isValid: false, message: 'Must be a whole number' };
    }

    return { isValid: true, message: '' };
  },
};
