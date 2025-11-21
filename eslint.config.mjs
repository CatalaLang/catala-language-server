import js from '@eslint/js';
import tsParser from '@typescript-eslint/parser';
import tseslint from '@typescript-eslint/eslint-plugin';

export default [
  // Files/directories to ignore (replaces .eslintignore in ESLint v9+)
  {
    ignores: [
      'src/generated',
      'dist/',
      'server/_build',
      'server/vendors',
      '_opam',
      '.eslintrc.js',
      'webpack.config.js',
      'lint_scripts/',
      'tests/',
      'vitest.config.*',
    ],
  },

  // Base JS recommended rules
  js.configs.recommended,

  // TypeScript rules (typed)
  {
    files: ['**/*.{ts,tsx,js}'],
    languageOptions: {
      parser: tsParser,
      ecmaVersion: 2020,
      sourceType: 'module',
      parserOptions: {
        // Typed linting against the project tsconfig
        project: './tsconfig.json',
        tsconfigRootDir: process.cwd(),
      },
      globals: {
        window: 'readonly',
        document: 'readonly',
        console: 'readonly',
        setTimeout: 'readonly',
        clearTimeout: 'readonly',
        Buffer: 'readonly',
        Thenable: 'readonly',
        NodeJS: 'readonly',
        acquireVsCodeApi: 'readonly',
        crypto: 'readonly',
      },
    },
    plugins: {
      '@typescript-eslint': tseslint,
    },
    rules: {
      // Base JS rules to turn off in TS (handled by TS or @typescript-eslint)
      'no-undef': 'off',
      'no-unused-vars': 'off',

      // Enforce explicit function return types
      '@typescript-eslint/explicit-function-return-type': 'warn',

      // Disallow unused variables (TS-aware)
      '@typescript-eslint/no-unused-vars': [
        'error',
        { argsIgnorePattern: '^_' },
      ],

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

      // Enforce optional chain syntax (warn to reduce churn)
      '@typescript-eslint/prefer-optional-chain': 'warn',

      // Disallow any type
      '@typescript-eslint/no-explicit-any': 'warn',

      // Enforce consistent type assertions
      '@typescript-eslint/consistent-type-assertions': 'error',

      // Prefer includes over indexOf
      '@typescript-eslint/prefer-includes': 'error',
    },
  },

  // Mocha globals for VS Code extension tests
  {
    files: ['src/test/**/*.ts'],
    languageOptions: {
      globals: {
        suite: 'readonly',
        suiteTeardown: 'readonly',
        test: 'readonly',
      },
    },
  },
];
