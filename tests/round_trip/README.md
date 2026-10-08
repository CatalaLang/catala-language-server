# Round-trip tests

`./round_trip.sh` checks the test case editor's reading and writing of test
files. Run it with catala's testcase plugin built from this checkout (installed,
or `CATALA_PLUGINS` pointing at `_build/default/test-case-parser`).

## Reference files

`reference_*` are tests exactly as the editor writes them. Reading and writing
one must give it back byte for byte: saving is idempotent and its format fixed.
Any change to the written form fails, because every saved test in every project
would get the same diff on its next save.

### Changing the format on purpose

1. Change the writer; `round_trip.sh` fails with the diff of each reference.
   That diff is what users will see.
2. `./round_trip.sh --update-reference` rewrites the references.
3. `./round_trip.sh` again, without the flag: the update writes without
   comparing, so only this run shows the new format is stable.
4. Read `git diff reference_*`: only the intended change should be there.
5. Commit the references with the writer change, and say in the PR that saved
   tests will get this diff once.

If the new writer can no longer read the old format, `--update-reference` cannot
rewrite the references: start again from the fixtures
(`cp test_X.catala_en reference_X.catala_en`), then update. Users' files would
not open either: that change is a breaking one.

### Adding a reference

Copy a test file that uses the feature to `reference_<name>.catala_<lang>`, run
`--update-reference`, check the result and commit it. The script checks every
`reference_*` file.
