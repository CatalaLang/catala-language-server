import * as fs from 'fs';
import { exec } from 'child_process';
import * as os from 'os';
import * as path from 'path';
import { Position, Range } from 'vscode-languageserver';

const CLI_COMMAND = '/home/user/.opam/4.14.0/bin/catala html';
const inputText = fs.readFileSync(
  path.resolve(__dirname, '../../../tests/fixtures/cli.catala_en'),
  'utf-8'
);
const tempFileName = 'tempfile.catala_en';

/**
 * Match "3.25-3.34" of example output from `catala html tempfile.catala_en`:
```
[ERROR]
Syntax error at token "TaxCredit"
Message: expected 'content <type>'
You could have written : "content"

Error token:
┌─⯈ /path/to/cli.catala_en:3.25-3.34:
└─┐
3 │ declaration enumaration TaxCredit:
  │                         ‾‾‾‾‾‾‾‾‾

Last good token:
┌─⯈ /path/to/cli.catala_en:3.13-3.24:
└─┐
3 │ declaration enumaration TaxCredit:
```
 */
type CatalaCliOutput = string;
/** "4.3-4.8" */
type CatalaRangeString = string;

// const matchRangeRegex = new RegExp(`${tempFileName}:(.*):`, 'g');
const matchRangeRegex = new RegExp(`${tempFileName}:([0-9.-]+):`, 'g');

type FunctionReturn = {
  text: string;
  range: Range | undefined;
};

export function getCatalaCliOutput(text: string): Promise<FunctionReturn> {
  return new Promise((resolve) => {
    const tempDir = os.tmpdir();
    const tempFile = path.join(tempDir, tempFileName);
    const output = {
      text: '',
      range: undefined,
    };

    fs.writeFile(tempFile, text, (err) => {
      if (err) {
        output.text = err.toString();
        resolve(output);
      }

      const fullCommand = `${CLI_COMMAND} ${tempFile}`;
      exec(fullCommand, (error, stdout, stderr) => {
        const outputText: CatalaCliOutput = (
          stderr ??
          error ??
          stdout
        ).toString();
        if (outputText) {
          const rangeString = getRangeString(outputText);
          if (rangeString.includes('FILE argument')) {
            /* prettier-ignore */ console.log('>>>> _ >>>> ~ file: getCatalaCliOutput.ts ~ line 113 ~ rangeString', rangeString);
          }
          if (rangeString) {
            const range = convertStringToRange(rangeString);
            const output = {
              text: outputText,
              range,
            };
            resolve(output);
          } else {
            output.text = outputText;
            resolve(output);
          }
        }

        // fs.unlink(tempFile, (err) => {
        //   if (err) {
        //     console.error(`Failed to delete temporary file: ${err}`);
        //     if (err) {
        //       output.text = err.toString();
        //       resolve(output);
        //     }
        //   }
        // });
      });
    });
  });
}

function getRangeString(text: CatalaCliOutput): CatalaRangeString {
  const split = text.split('\n');
  /**
   * There are two lines.
   * First one is "Error token:", second one is "Last good token:".
   * We only want the first line
   *
   * @returns found // "┌─⯈ /tmp/tempfile.catala_en:4.3-4.8:"
   */
  const found = split.find((line) => line.includes(tempFileName));
  if (!found) return '';
  const rangeString = found?.split(':')[1] as CatalaRangeString;
  return rangeString;
}

function convertStringToRange(rangeString: CatalaRangeString): Range {
  const [start, end] = rangeString.split('-');
  const [startLine, startCharacter] = start
    .split('.')
    .map(convert1BasedTo0Based);
  const [endLine, endCharacter] = end.split('.').map(convert1BasedTo0Based);
  const startPosition = Position.create(startLine, startCharacter);
  const endPosition = Position.create(endLine, endCharacter);
  const range = Range.create(startPosition, endPosition);
  return range;
}

function convert1BasedTo0Based(value: string): number {
  const num = Number(value);
  const result = Math.max(0, num - 1);
  return result;
}

async function main() {
  const result = await getCatalaCliOutput(inputText);
  result; /*?*/
  // result.text.match(matchRangeRegex); /*?*/
  // matchRangeRegex.exec(result.text); /*?*/
}
// main();
