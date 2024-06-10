// module.exports = function (wallaby) {
//   return {
//     files: [
//       'server/**/*.ts',
//       { pattern: 'tests/**/*.catala_en', instrument: false },
//       // { pattern: 'tests/testFixture/**/*.{ts,html,json}', instrument: false },
//       // 'tests/minimal-jest/**/*.{ts,feature}',
//       // 'tests/dev-test-helpers/**/*.{ts,json}',
//       // 'tests/jest-cucumber-setup.spec.ts',
//     ],

//     tests: [
//       'tests/testLauncher/withWallaby.spec.ts',
//       // 'tests/unit/core/**/*.spec.ts'
//     ],
//     testFramework: 'jest',
//     env: {
//       type: 'node',
//     },
//     debug: true,
//     filesWithNoCoverageCalculated: [
//       '**/node_modules/**',
//       'client/**',
//       'tests/**/*.spec.ts',
//     ]
//   },
// }
