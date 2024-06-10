import {
  TEST_FILE_NAMES,
  readCatalaFile,
} from './test-helpers/readCatalaTestFile';
import { parse } from '../src/catala/parser';
import { matchSnapshot } from './test-helpers/matchSnapshot';

describe('parser', () => {
  TEST_FILE_NAMES.forEach((fileName) => {
    it(fileName, async () => {
      const content = readCatalaFile(fileName);
      const parsed = await parse(content);
      matchSnapshot(content, parsed);
    });
  });
});
