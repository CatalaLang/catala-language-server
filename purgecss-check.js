/**
 * CSS Unused Selectors Analyzer using PurgeCSS
 *
 * Installation: npm install --save-dev purgecss glob
 * Usage: npx node purgecss-check.js
 */

// eslint-disable-next-line @typescript-eslint/no-var-requires
const { PurgeCSS } = require('purgecss');
// eslint-disable-next-line @typescript-eslint/no-var-requires
const glob = require('glob');

// eslint-disable-next-line @typescript-eslint/explicit-function-return-type
let hasUnused = false;

async function findUnusedCSS() {
  console.log('Analyzing unused CSS with PurgeCSS...');

  try {
    const cssFiles = glob.sync('src/**/*.css');
    const contentFiles = glob.sync('src/**/*.{tsx,jsx}');

    if (cssFiles.length === 0) {
      console.log('No CSS files found in src/ directory');
      return;
    }

    console.log(`CSS files found (${cssFiles.length}):`, cssFiles);
    console.log(
      `Content files found (${contentFiles.length}):`,
      contentFiles.length > 10
        ? `${contentFiles.slice(0, 10).join(', ')}...`
        : contentFiles.join(', ')
    );

    const result = await new PurgeCSS().purge({
      content: contentFiles,
      css: cssFiles,
      rejected: true,
      safelist: ['html', 'body'],
    });

    console.log('\n=== RESULTS ===');

    result.forEach((file) => {
      if (!file.css) {
        console.log(`\nFile: ${file.file}`);
        console.log('Error: Unable to analyze this CSS file');
        return;
      }

      const purgedSize = file.css.length;

      console.log(`\nFile: ${file.file}`);
      console.log(`Size after purge: ${purgedSize} bytes`);

      if (file.rejected && file.rejected.length > 0) {
        console.log('\nUnused CSS selectors:');
        file.rejected.forEach((selector) => {
          console.log(`  - ${selector}`);
        });

        const totalSelectors =
          file.rejected.length + (file.selectors ? file.selectors.length : 0);
        const unusedPercent = (
          (file.rejected.length / totalSelectors) *
          100
        ).toFixed(2);
        console.log(`Percentage of unused selectors: ${unusedPercent}%`);
        hasUnused = true;
      } else {
        console.log('\nNo unused CSS selectors found.');
      }
    });
  } catch (error) {
    console.error('Error during analysis:', error);
  }
}

findUnusedCSS()
  .then(() => {
    if (hasUnused) {
      console.error('\nError: Unused CSS selectors found');
      process.exit(1);
    } else {
      console.log('\nSuccess: No unused CSS selectors found');
      process.exit(0);
    }
  })
  .catch((error) => {
    console.error('Error during analysis:', error);
    process.exit(1);
  });
