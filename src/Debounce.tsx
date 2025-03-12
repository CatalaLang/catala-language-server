import { useState, useEffect, useRef } from 'react';

// Custom hook for debouncing input changes
export function useDebounce(
  initialValue: string,
  onChange: (value: string) => void,
  delay: number = 300
): [string, (newValue: string) => void] {
  const [value, setValue] = useState<string>(initialValue);
  const debounceTimerRef = useRef<number | null>(null);

  // Handle input changes with debounce
  const handleChange = (newValue: string) => {
    setValue(newValue);

    // Clear any existing timer
    if (debounceTimerRef.current !== null) {
      window.clearTimeout(debounceTimerRef.current);
    }

    // Set a new timer
    debounceTimerRef.current = window.setTimeout(() => {
      onChange(newValue);
    }, delay);
  };

  // Clean up timer on unmount
  useEffect(() => {
    return (): void => {
      if (debounceTimerRef.current !== null) {
        window.clearTimeout(debounceTimerRef.current);
      }
    };
  }, []);

  // Update internal state when external value changes
  useEffect(() => {
    setValue(initialValue);
  }, [initialValue]);

  return [value, handleChange];
}
