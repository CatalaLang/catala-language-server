import { readCatalaFile } from './test-helpers/readCatalaTestFile';
import { parse } from '../src/catala/parser';
import { mapPropsForSnapshot } from './test-helpers/matchSnapshot';

describe('parser - debugging', () => {
  it('debugging', async () => {
    const content = readCatalaFile('combination_declStruct_declScope_scope');
    const parsed = await parse(content);

    const result = mapPropsForSnapshot(parsed);
    result; /*?*/
    // For debugging, we want to see the result being printed, thus always fail the test
    expect(result).toBeFalsy();
  });
});
