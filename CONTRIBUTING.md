# Contributing

## Test coverage

Run `npm run webview:test:coverage` to generate a coverage report for `src/` (excluding generated code). The terminal shows a summary table; a detailed per-line HTML report is written to `coverage/index.html` (open in a browser to inspect which functions and branches are uncovered).

## Code duplication

Run `npm run dupcheck` to detect copy-pasted code across `src/` and `tests/`. The report lists each clone with file locations and line numbers. Shared test helpers should go in `tests/helpers.ts` rather than being redefined per file.
