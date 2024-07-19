module.exports = {
  parser: '@typescript-eslint/parser',
  extends: ['eslint:recommended', 'plugin:@typescript-eslint/recommended'],
  plugins: ['@typescript-eslint'],
  parserOptions: {
    ecmaVersion: 2020,
    sourceType: 'module',
    project: './tsconfig.json',
    tsconfigRootDir: __dirname,
  },
  env: {
    node: true,
    es2020: true,
  },
  rules: {
    // Enforce explicit function return types
    '@typescript-eslint/explicit-function-return-type': 'warn',

    // Disallow unused variables
    '@typescript-eslint/no-unused-vars': ['error', { argsIgnorePattern: '^_' }],

    // Enforce consistent use of type imports
    '@typescript-eslint/consistent-type-imports': 'error',

    // Enforce naming conventions
    '@typescript-eslint/naming-convention': [
      'error',
      { selector: 'interface', format: ['PascalCase'], prefix: ['I'] },
      { selector: 'typeAlias', format: ['PascalCase'] },
      { selector: 'enum', format: ['PascalCase'] },
    ],

    // Prefer nullish coalescing
    '@typescript-eslint/prefer-nullish-coalescing': 'warn',

    // Enforce optional chain syntax
    '@typescript-eslint/prefer-optional-chain': 'error',

    // Disallow any type
    '@typescript-eslint/no-explicit-any': 'warn',

    // Enforce consistent type assertions
    '@typescript-eslint/consistent-type-assertions': 'error',

    // Enforce `includes` method over `indexOf` method
    '@typescript-eslint/prefer-includes': 'error',

    // Enforce the use of `for-of` loop over the standard `for` loop where possible
    '@typescript-eslint/prefer-for-of': 'warn',
  },
};
