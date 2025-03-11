import { useState, useEffect, useRef } from 'react';

// Custom hook for debounced validation
export function useDebounceValidation<T>(
  value: string,
  onValidValue: (value: T) => void,
  parser: (value: string) => T,
  debounceTime: number = 300
): [string, (newValue: string, isValid: boolean) => void] {
  const [inputValue, setInputValue] = useState<string>(value);
  const debounceTimerRef = useRef<number | null>(null);

  // Update validation with debounce
  const handleChange = (newValue: string, isValid: boolean) => {
    setInputValue(newValue);

    // Clear any existing timer
    if (debounceTimerRef.current !== null) {
      window.clearTimeout(debounceTimerRef.current);
    }

    // Set a new timer
    debounceTimerRef.current = window.setTimeout(() => {
      // Only call onValidValue if the input is valid
      if (isValid) {
        onValidValue(parser(newValue));
      }
    }, debounceTime);
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
  }, [value]);

  return [inputValue, handleChange];
}
