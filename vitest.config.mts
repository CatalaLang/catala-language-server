import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    environment: 'jsdom',
    setupFiles: ['./tests/setup.ts'],
    globals: true,
    include: ['tests/**/*.{test,spec}.{ts,tsx}'],
    exclude: ['src/test/**', 'node_modules/**', 'dist/**'],
    coverage: {
      provider: 'v8',
      include: ['src/**'],
      exclude: [
        'src/generated/**',
        'src/test/**',
        'src/extension/**',
        'src/scope-editor/**',
        'src/shared/**',
        'src/styles/**',
        'src/locales/**',
        'src/i18n/**',
        'src/messaging/**',
        'src/test-case-editor/TestEditor.tsx',
        'src/test-case-editor/TestFileEditor.tsx',
        'src/test-case-editor/TestInputsEditor.tsx',
        'src/test-case-editor/testCaseCompilerInterop.ts',
        'src/App.tsx',
        'src/ContextMenu.tsx',
        'src/uiEntryPoint.ts',
        'src/extension.ts',
      ],
      reporter: ['text', 'html'],
    },
  },
});
