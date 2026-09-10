#!/bin/bash

cd "$(dirname "$0")"

function cleanup(){
    rm -f rename_to_typecheck.catala_en
    rm -f to_typecheck.catala_en
    rm -f context_vars_roundtrip.catala_en
    rm -rf _build
}

trap cleanup EXIT

clerk start

# make round-trip (read then write)
catala testcase read test_implicit_import.catala_en | catala testcase write --language en > to_typecheck.catala_en

# ensure it typechecks
clerk typecheck to_typecheck.catala_en || exit 1

# generation + typecheck (regression test)
catala testcase generate rename.catala_en --scope Example | catala testcase write --language en > rename_to_typecheck.catala_en
clerk typecheck rename_to_typecheck.catala_en || exit 1

# context variables: read/write round-trip must not emit definitions for unset context vars
catala testcase read test_context_vars.catala_en | catala testcase write --language en > context_vars_roundtrip.catala_en
# y override must be preserved, z must be absent (it was not in the source)
grep -q "definition c\.y equals 99" context_vars_roundtrip.catala_en || { echo "FAIL: expected y override in round-trip output"; exit 1; }
grep -q "definition c\.z" context_vars_roundtrip.catala_en && { echo "FAIL: unset context var z should not appear in round-trip output"; exit 1; }
# the round-tripped file must typecheck
clerk typecheck context_vars_roundtrip.catala_en || exit 1
# run the original test and check the assertion passes (z = y*2 = 198)
clerk run test_context_vars.catala_en || exit 1
catala testcase run --scope C_test test_context_vars.catala_en || exit 1

# ── a diff inside an optional names the payload ─────────────────────────────
# The wire addresses the payload explicitly (EnumPayload), so exactly one
# editor node owns each diff.
clerk test test_opt_out.catala_en >/dev/null 2>&1 || true  # build the plugin; the test is meant to fail
out=$(catala testcase run --scope Give_test test_opt_out.catala_en)
echo "$out" | grep -q '"assert_failures":true' \
    || { echo "FAIL: the wrong expectation did not fail"; exit 1; }
echo "$out" | grep -q '\["EnumPayload","Present"\]' \
    || { echo "FAIL: the diff path does not name the payload"; exit 1; }
