import { useState, useEffect, useRef } from 'react';
import type {
  ValidationError} from '../validation';
import {
  registerValidationErrors,
  clearValidationErrors,
} from '../validation';

/**
 * Custom hook to handle validated input fields
 */
export function useValidatedInput<T>({
  initialValue,
  validator,
  onValidChange,
  validationId,
  parseValue,
}: {
  initialValue: T | undefined;
  validator: (value: string) => ValidationError[];
  onValidChange: (value: T) => void;
  validationId: string;
  parseValue: (value: string) => T;
}): {
  inputValue: string;
  errors: ValidationError[];
  handleChange: (evt: React.ChangeEvent<HTMLInputElement>) => void;
  handleBlur: () => void;
  handleKeyDown: (evt: React.KeyboardEvent<HTMLInputElement>) => void;
  inputRef: React.RefObject<HTMLInputElement>;
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

  const handleChange = (evt: React.ChangeEvent<HTMLInputElement>): void => {
    const newValue = evt.target.value;
    setInputValue(newValue);

    // Clear errors during typing for better UX
    if (errors.length > 0) {
      setErrors([]);
      clearValidationErrors(validationId);
    }
  };

  const handleBlur = (): void => {
    const validationErrors = validator(inputValue);
    setErrors(validationErrors);
    registerValidationErrors(validationId, validationErrors);

    // If valid, update the value
    if (validationErrors.length === 0 && inputValue.trim() !== '') {
      onValidChange(parseValue(inputValue));
    }
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
  };
}
