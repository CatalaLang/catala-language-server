#!/bin/bash

cd "$(dirname "$0")"

function cleanup(){
    rm -f rename_to_typecheck.catala_en
    rm -f to_typecheck.catala_en
    rm -f context_vars_roundtrip.catala_en
    rm -rf _build
    rm -rf _sig_stub
    rm -rf _sig_snap
    rm -rf _migrate
    rm -rf _init
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

# The canonical projection and its drift invariants are pure logic and are
# unit-tested in test-case-parser/test/test.ml. Here we only cover what needs
# the real compiler end-to-end.

# --- Representation-independence: list-scopes hash == test tested_scope hash -
# (closes the drift-detection loop: save-side and read-side must agree, against
# the REAL compiler output for both paths)
LS_HASH=$(catala testcase list-scopes opttup.catala_en | sighash)
TS_HASH=$(catala testcase read test_opttup.catala_en | jq -c '[.[0].tested_scope]' | sighash)
[ "$LS_HASH" = "$TS_HASH" ] || { echo "FAIL: list-scopes hash ($LS_HASH) != tested_scope hash ($TS_HASH)"; exit 1; }

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
# Also exercises the signature pin (#[testcase.sig]) + snapshot store (--sig-dir).
catala testcase read test_opttup.catala_en \
  | catala testcase write --language en --sig-dir _sig_snap > _sig_stub/test_opttup_rt.catala_en
grep -q 'definition calc.pair equals (7, $3.00)' _sig_stub/test_opttup_rt.catala_en \
  || { echo "FAIL: tuple value not emitted as (a, b)"; exit 1; }
( cd _sig_stub && clerk typecheck test_opttup_rt.catala_en ) || exit 1

# Pin is stamped on write and its hash matches the live (list-scopes) hash.
grep -q "#\[testcase.sig = \"OptTup.Calc@$LS_HASH\"\]" _sig_stub/test_opttup_rt.catala_en \
  || { echo "FAIL: signature pin not stamped as OptTup.Calc@$LS_HASH"; exit 1; }
# Pin round-trips back through read into the sig_pin field.
( cd _sig_stub && catala testcase read test_opttup_rt.catala_en ) \
  | jq -e --arg p "OptTup.Calc@$LS_HASH" '.[0].sig_pin == $p' > /dev/null \
  || { echo "FAIL: sig_pin not recovered on read"; exit 1; }
# Snapshot store: content-addressed scope_def JSON, named by the same hash.
[ -f "_sig_snap/OptTup.Calc@$LS_HASH.sig.json" ] \
  || { echo "FAIL: signature snapshot not written"; exit 1; }
jq -e '.name == "Calc" and .module_name == "OptTup"' "_sig_snap/OptTup.Calc@$LS_HASH.sig.json" > /dev/null \
  || { echo "FAIL: snapshot is not a valid scope_def for OptTup.Calc"; exit 1; }
# sig-hash accepts a single scope_def object (a snapshot file), not just a list,
# from stdin OR a filename argument.
[ "$(catala testcase sig-hash < "_sig_snap/OptTup.Calc@$LS_HASH.sig.json" | cut -f2)" = "$LS_HASH" ] \
  || { echo "FAIL: sig-hash on a single snapshot object (stdin) did not match LS_HASH"; exit 1; }
[ "$(catala testcase sig-hash "_sig_snap/OptTup.Calc@$LS_HASH.sig.json" | cut -f2)" = "$LS_HASH" ] \
  || { echo "FAIL: sig-hash on a single snapshot file (argument) did not match LS_HASH"; exit 1; }
# Content-addressed: a second write must not error and must not duplicate.
catala testcase read test_opttup.catala_en \
  | catala testcase write --language en --sig-dir _sig_snap > /dev/null
[ "$(ls _sig_snap | wc -l)" -eq 1 ] \
  || { echo "FAIL: snapshot store not content-addressed (expected 1 file)"; exit 1; }

# ============================================================================
# migrate status: drift triage into fresh / stale / unknown / blocked.
# Self-contained project in _migrate: a stub OptTup module + a pinned test +
# its committed snapshot, then we drift the module and re-triage.
# ============================================================================
rm -rf _migrate && mkdir -p _migrate
catala testcase list-scopes opttup.catala_en \
  | catala testcase stub --output-dir _migrate --language en
catala testcase read test_opttup.catala_en \
  | catala testcase write --language en --sig-dir _migrate > _migrate/t.catala_en
printf '[project]\ninclude_dirs = ["."]\n' > _migrate/clerk.toml
( cd _migrate && clerk start >/dev/null 2>&1 )

state() { # state() FILE -> prints the first entry's state
  ( cd _migrate && catala testcase migrate status --json "$1" 2>/dev/null ) | jq -r '.[0].state'
}

# fresh: pin == live
[ "$(state t.catala_en)" = "Fresh" ] \
  || { echo "FAIL(migrate): expected Fresh for pinned, undrifted test"; exit 1; }

# unknown: a test with no #[testcase.sig] pin
grep -v 'testcase.sig' _migrate/t.catala_en > _migrate/u.catala_en
[ "$(state u.catala_en)" = "Unknown" ] \
  || { echo "FAIL(migrate): expected Unknown for unpinned test"; exit 1; }

# drift the live module: rename an input (changes the signature)
sed -i 's/input pair content/input pair2 content/' _migrate/OptTup.catala_en
( cd _migrate && clerk start >/dev/null 2>&1 )

# stale: pin != live, pinned snapshot still present -> migratable
[ "$(state t.catala_en)" = "Stale" ] \
  || { echo "FAIL(migrate): expected Stale after drift with snapshot present"; exit 1; }

# blocked: same drift but the pinned snapshot is gone -> not recoverable
mv _migrate/OptTup.Calc@*.sig.json _migrate/_hidden.sig.json
[ "$(state t.catala_en)" = "Blocked" ] \
  || { echo "FAIL(migrate): expected Blocked when snapshot is missing"; exit 1; }
mv _migrate/_hidden.sig.json _migrate/OptTup.Calc@"$LS_HASH".sig.json

# blocked (compile error): if the live module itself won't compile, status must
# report a clear reason and not crash.
cp _migrate/OptTup.catala_en _migrate/OptTup.bak
sed -i 's/  input items content list of optional of integer/  input items content list of optional of integer\n  input items content list of optional of integer/' _migrate/OptTup.catala_en
( cd _migrate && clerk start >/dev/null 2>&1 )
( cd _migrate && catala testcase migrate status t.catala_en 2>/dev/null ) | grep -qi 'does not compile' \
  || { echo "FAIL(migrate): expected a 'does not compile' blocked reason"; exit 1; }
mv _migrate/OptTup.bak _migrate/OptTup.catala_en
( cd _migrate && clerk start >/dev/null 2>&1 )

# directory mode: recurses, triages every test, skips the module file
DIR_STATES=$( ( cd _migrate && catala testcase migrate status --json . 2>/dev/null ) \
  | jq -r '[.[].state] | sort | join(",")' )
[ "$DIR_STATES" = "Stale,Unknown" ] \
  || { echo "FAIL(migrate): dir mode states = '$DIR_STATES' (expected Stale,Unknown)"; exit 1; }

# ============================================================================
# migrate init: seed pins onto unpinned tests that typecheck; refuse drifted.
# Own project (_init) so it doesn't perturb the status triage above.
# ============================================================================
rm -rf _init && mkdir -p _init
catala testcase list-scopes opttup.catala_en \
  | catala testcase stub --output-dir _init --language en
catala testcase read test_opttup.catala_en \
  | catala testcase write --language en > _init/s.catala_en
sed -i '/testcase.sig/d' _init/s.catala_en   # make it unpinned
printf '[project]\ninclude_dirs = ["."]\n' > _init/clerk.toml
( cd _init && clerk start >/dev/null 2>&1 )

# seed: unpinned + typechecks -> stamps the live pin and the snapshot
( cd _init && catala testcase migrate init s.catala_en >/dev/null 2>&1 )
grep -q "#\[testcase.sig = \"OptTup.Calc@$LS_HASH\"\]" _init/s.catala_en \
  || { echo "FAIL(init): pin not seeded into unpinned test"; exit 1; }
[ -f "_init/OptTup.Calc@$LS_HASH.sig.json" ] \
  || { echo "FAIL(init): snapshot not written by init"; exit 1; }
( cd _init && catala testcase migrate status s.catala_en 2>/dev/null ) | grep -q '^fresh:   1' \
  || { echo "FAIL(init): seeded test is not Fresh"; exit 1; }

# idempotent: re-running init on a now-pinned file adds no second pin
( cd _init && catala testcase migrate init s.catala_en >/dev/null 2>&1 )
[ "$(grep -c 'testcase.sig' _init/s.catala_en)" -eq 1 ] \
  || { echo "FAIL(init): re-init duplicated the pin"; exit 1; }

# refuse: an unpinned test that no longer typechecks (drift) is left untouched
catala testcase read test_opttup.catala_en \
  | catala testcase write --language en > _init/r.catala_en
sed -i '/testcase.sig/d' _init/r.catala_en
sed -i 's/input pair content/input pair2 content/' _init/OptTup.catala_en
( cd _init && clerk start >/dev/null 2>&1 )
( cd _init && catala testcase migrate init r.catala_en >/dev/null 2>&1 )
[ "$(grep -c 'testcase.sig' _init/r.catala_en)" -eq 0 ] \
  || { echo "FAIL(init): drifted unpinned test must be left untouched"; exit 1; }
