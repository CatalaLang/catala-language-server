#!/bin/bash

cd "$(dirname "$0")"

function cleanup(){
    rm -f rename_to_typecheck.catala_en
    rm -f to_typecheck.catala_en
    rm -f context_vars_roundtrip.catala_en
    rm -rf _build
    rm -rf _sig_stub
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

# ============================================================================
# Signature projection + stub synthesis (test-migration feature)
# Fixture: opttup.catala_en (OptTup.Calc) exercises option, tuple, nested
# struct/enum in I/O; test_opttup.catala_en holds rich values for them.
# ============================================================================

sighash() { catala testcase sig-hash | cut -f2; }

# --- Golden canonical text: guards accidental hash-scheme drift -------------
catala testcase list-scopes opttup.catala_en \
  | catala testcase sig-hash --show-canonical | tail -n +2 > opttup_sig.actual
diff opttup_sig.golden opttup_sig.actual || {
  echo "FAIL: canonical projection changed; if intended, bump sig_scheme_version and refresh opttup_sig.golden"; exit 1; }
rm -f opttup_sig.actual

# --- Representation-independence: list-scopes hash == test tested_scope hash -
# (closes the drift-detection loop: save-side and read-side must agree)
LS_HASH=$(catala testcase list-scopes opttup.catala_en | sighash)
TS_HASH=$(catala testcase read test_opttup.catala_en | jq -c '[.[0].tested_scope]' | sighash)
[ "$LS_HASH" = "$TS_HASH" ] || { echo "FAIL: list-scopes hash ($LS_HASH) != tested_scope hash ($TS_HASH)"; exit 1; }

# --- Projection invariant matrix --------------------------------------------
catala testcase list-scopes opttup.catala_en > _sig_base.json
BASE=$(sighash < _sig_base.json)
same() { # cosmetic mutation must NOT change the hash
  local h; h=$(jq -c "$1" _sig_base.json | sighash)
  [ "$h" = "$BASE" ] || { echo "FAIL(same): '$1' changed hash ($h != $BASE)"; exit 1; }
}
diff_() { # contract mutation MUST change the hash
  local h; h=$(jq -c "$1" _sig_base.json | sighash)
  [ "$h" != "$BASE" ] || { echo "FAIL(diff): '$1' left hash unchanged"; exit 1; }
}
same '.[0].outputs |= (to_entries|reverse|from_entries)'   # reorder outputs
same '.[0].module_deps = ["ZZZ","AAA"]'                    # module_deps excluded
diff_ '.[0].inputs.pair.is_context = true'                 # context vs input
diff_ '.[0].inputs.pair.typ = ["TTuple",["TMoney","TInt"]]' # tuple order is positional
diff_ '.[0].inputs.maybe_amount.typ = ["TOption","TInt"]'  # retype option payload
diff_ '.[0].inputs.pair2 = .[0].inputs.pair | del(.[0].inputs.pair)' # rename input
diff_ '.[0].outputs.extra = "TBool"'                       # added output
rm -f _sig_base.json

# --- Stub synthesis value recovery (the migration sidestep) -----------------
# Read rich option/tuple values against the REAL module, then against ONLY a
# synthesized stub (real module absent). Both must recover identical values.
catala testcase read test_opttup.catala_en > _sig_real.json
mkdir -p _sig_stub
catala testcase list-scopes opttup.catala_en \
  | catala testcase stub --output-dir _sig_stub --language en
cp test_opttup.catala_en _sig_stub/
printf '[project]\ninclude_dirs = ["."]\n' > _sig_stub/clerk.toml
( cd _sig_stub && clerk start >/dev/null 2>&1 && catala testcase read test_opttup.catala_en ) > _sig_stub.json
diff <(jq -S . _sig_real.json) <(jq -S . _sig_stub.json) > /dev/null \
  || { echo "FAIL: stub-recovered values differ from real-module values"; exit 1; }
rm -f _sig_real.json _sig_stub.json

# --- Stub + value round-trip must typecheck (tuples emitted as (a, b)) -------
catala testcase read test_opttup.catala_en | catala testcase write --language en > _sig_stub/test_opttup_rt.catala_en
grep -q 'definition calc.pair equals (7, $3.00)' _sig_stub/test_opttup_rt.catala_en \
  || { echo "FAIL: tuple value not emitted as (a, b)"; exit 1; }
( cd _sig_stub && clerk typecheck test_opttup_rt.catala_en ) || exit 1
