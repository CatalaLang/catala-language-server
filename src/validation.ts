// Types for validation
export type ValidationError = {
  message: string;
  severity: 'error' | 'warning';
};

export type ValidationResult = {
  isValid: boolean;
  errors: ValidationError[];
};

// Map to store validation errors by component ID
export type ValidationMap = Map<string, ValidationError[]>;

// Global validation state
export const validationState: {
  errors: ValidationMap;
  hasErrors: boolean;
} = {
  errors: new Map(),
  hasErrors: false,
};

// Generate a unique ID for validation tracking
export function generateValidationId(prefix: string): string {
  return `${prefix}_${Math.random().toString(36).substring(2, 9)}`;
}

// Register validation errors
export function registerValidationErrors(
  id: string,
  errors: ValidationError[]
): void {
  if (errors.length > 0) {
    validationState.errors.set(id, errors);
    validationState.hasErrors = true;
  } else {
    validationState.errors.delete(id);
    validationState.hasErrors = Array.from(
      validationState.errors.values()
    ).some((errs) => errs.length > 0);
  }
}

// Clear validation errors for a component
export function clearValidationErrors(id: string): void {
  validationState.errors.delete(id);
  validationState.hasErrors = Array.from(validationState.errors.values()).some(
    (errs) => errs.length > 0
  );
}

// Validate a numeric input
export function validateNumeric(
  value: string,
  options: { isInteger?: boolean; min?: number; max?: number } = {}
): ValidationResult {
  const errors: ValidationError[] = [];
  const numValue = parseFloat(value);

  if (value.trim() === '') {
    errors.push({
      message: 'Value cannot be empty',
      severity: 'error',
    });
    return { isValid: false, errors };
  }

  if (isNaN(numValue)) {
    errors.push({
      message: 'Value must be a number',
      severity: 'error',
    });
    return { isValid: false, errors };
  }

  if (options.isInteger && !Number.isInteger(numValue)) {
    errors.push({
      message: 'Value must be an integer',
      severity: 'error',
    });
  }

  if (options.min !== undefined && numValue < options.min) {
    errors.push({
      message: `Value must be at least ${options.min}`,
      severity: 'error',
    });
  }

  if (options.max !== undefined && numValue > options.max) {
    errors.push({
      message: `Value must be at most ${options.max}`,
      severity: 'error',
    });
  }

  return {
    isValid: errors.length === 0,
    errors,
  };
}

// Validate a date input
export function validateDate(value: string): ValidationResult {
  const errors: ValidationError[] = [];

  if (value.trim() === '') {
    errors.push({
      message: 'Date cannot be empty',
      severity: 'error',
    });
    return { isValid: false, errors };
  }

  const date = new Date(value);
  if (isNaN(date.getTime())) {
    errors.push({
      message: 'Invalid date format',
      severity: 'error',
    });
  }

  return {
    isValid: errors.length === 0,
    errors,
  };
}

// Validate a money input
export function validateMoney(value: string): ValidationResult {
  const errors: ValidationError[] = [];

  if (value.trim() === '') {
    errors.push({
      message: 'Value cannot be empty',
      severity: 'error',
    });
    return { isValid: false, errors };
  }

  const regex = /^-?\d+(\.\d{1,2})?$/;
  if (!regex.test(value)) {
    errors.push({
      message: 'Invalid money format (use format: 123.45)',
      severity: 'error',
    });
  }

  return {
    isValid: errors.length === 0,
    errors,
  };
}

// Trigger validation for all components
export function validateAll(): boolean {
  // This function would be called before saving
  return !validationState.hasErrors;
}

// Focus on the first error element (to be implemented with refs)
export function focusFirstError(): void {
  // This would require DOM refs to be implemented
  console.log('Would focus on first error');
}

// Helper to extract validation errors for display
export function getValidationErrorsForDisplay(id: string): ValidationError[] {
  return validationState.errors.get(id) ?? [];
}
