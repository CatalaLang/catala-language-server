#!/bin/bash

cd "$(dirname "$0")"

function cleanup(){
    rm -f rename_to_typecheck.catala_en
    rm -f to_typecheck.catala_en
    rm -f context_vars_roundtrip.catala_en
    rm -f partial_*_roundtrip.catala_en
    rm -f rebuilt_details.catala_en
    rm -f rebuilt_bare.catala_en
    rm -f opt_fr_roundtrip.catala_fr
    rm -f written_*.catala_en written2_*.catala_en
    rm -rf _build
    rm -rf "$notes_scratch"
}

trap cleanup EXIT

clerk start

# make round-trip (read then write)
catala testcase read test_implicit_import.catala_en | catala testcase write --language en > to_typecheck.catala_en

# ensure it typechecks
clerk typecheck to_typecheck.catala_en || exit 1
# ...and that it is not vacuous: an empty result writes an empty file that
# typechecks happily.
grep -q "definition example\\.inp" to_typecheck.catala_en || { echo "FAIL: round-trip produced no test at all"; exit 1; }

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

# ── partial read ────────────────────────────────────────────────────────────
# A healthy test must survive recovery intact: write the recovery back out and
# typecheck it against the real module.
for f in test_implicit_import test_context_vars test_optionals test_items test_bare test_spans; do
    catala testcase partial-read $f.catala_en \
        | catala testcase write --language en > partial_${f}_roundtrip.catala_en \
        || { echo "FAIL: partial read of $f"; exit 1; }
    clerk typecheck partial_${f}_roundtrip.catala_en \
        || { echo "FAIL: partial read of $f does not typecheck"; exit 1; }
done

# every test in the file, not just the first one
n=$(grep -c '^declaration scope' partial_test_optionals_roundtrip.catala_en)
[ "$n" = 3 ] || { echo "FAIL: recovered $n of 3 tests"; exit 1; }

# values a partial read cannot type (`Absent`, `impossible`) must survive it
grep -q 'definition grant\.bonus equals Absent' partial_test_optionals_roundtrip.catala_en \
    || { echo "FAIL: an absent optional did not survive the partial read"; exit 1; }
grep -q 'definition grant\.base equals impossible' partial_test_optionals_roundtrip.catala_en \
    || { echo "FAIL: an unfilled field did not survive the partial read"; exit 1; }
grep -q 'assertion (grant\.total = \$1000\.00)' partial_test_optionals_roundtrip.catala_en \
    || { echo "FAIL: an expected output did not survive the partial read"; exit 1; }


# Both readers must spell a type the same way; the typecheck above cannot see
# a difference in name alone, since Catala infers a struct from its fields.
for reader in read partial-read; do
    catala testcase $reader test_implicit_import.catala_en \
        | grep -q '"struct_name":"Period.Period"' \
        || { echo "FAIL: $reader does not qualify a struct with its module"; exit 1; }
    catala testcase $reader test_optionals.catala_en | grep -q '"TOption"' \
        || { echo "FAIL: $reader does not type an optional as TOption"; exit 1; }
    if catala testcase $reader test_optionals.catala_en | grep -q '"TEnum"'; then
        echo "FAIL: $reader types an optional as a bare enum"; exit 1
    fi
done

# uids: the identity the array editor tracks rows by, and what the
# original-vs-working-copy diff matches on
for reader in read partial-read; do
    n=$(catala testcase $reader test_items.catala_en | grep -o '"Uid"' | wc -l)
    [ "$n" = 2 ] || { echo "FAIL: $reader recovered $n of 2 item uids"; exit 1; }
done
# ...and the write side must put them back, or the round trip loses them anyway.
n=$(grep -c 'testcase.uid' partial_test_items_roundtrip.catala_en)
[ "$n" = 2 ] || { echo "FAIL: write emitted $n of 2 item uids"; exit 1; }

# Catala accepts a scope declaration in a plain block; both readers must.
clerk typecheck test_plain_block.catala_en || { echo "FAIL: plain-block test does not typecheck"; exit 1; }
for reader in read partial-read; do
    catala testcase $reader test_plain_block.catala_en | grep -q '"Grant_plain"' \
        || { echo "FAIL: $reader misses a test declared in a plain block"; exit 1; }
done
# ...and under a heading, which nests the code block one level down.
clerk typecheck test_heading.catala_en || { echo "FAIL: heading test does not typecheck"; exit 1; }
for reader in read partial-read; do
    catala testcase $reader test_heading.catala_en | grep -q '"Grant_heading"' \
        || { echo "FAIL: $reader misses a test declared under a heading"; exit 1; }
done

# ...while what the editor writes stays canonical.
awk '
  /^```catala-metadata/ { fence = "meta"; next }
  /^```catala/         { fence = "code"; next }
  /^```/               { fence = ""; next }
  /^declaration scope/ { if (fence != "meta") { exit 1 } }
' to_typecheck.catala_en || { echo "FAIL: write emitted a declaration outside a metadata block"; exit 1; }

# ── ownership ───────────────────────────────────────────────────────────────
# A file mixing editor-owned and hand-written tests is refused by both readers:
# a test the editor cannot represent would be deleted on the next save.
for reader in read rebuild; do
    if catala testcase $reader mixed.catala_en >/dev/null 2>&1; then
        echo "FAIL: $reader accepted a file mixing owned and hand-written tests"
        exit 1
    fi
    catala testcase $reader mixed.catala_en 2>&1 | grep -q "Hand_written" \
        || { echo "FAIL: $reader does not name the offending test"; exit 1; }
done

# ...but a partial read recovers whatever it can and leaves the decision to
# its caller.
n=$(catala testcase partial-read mixed.catala_en | grep -o '"testing_scope"' | wc -l)
[ "$n" = 2 ] || { echo "FAIL: partial read recovered $n of 2 scopes"; exit 1; }

# ── rebuilding a struct ─────────────────────────────────────────────────────
# test_details writes Detail's fields in another order than the module declares
# them and predates a field it has since gained: the writer must pair fields by
# name.
catala testcase rebuild test_details.catala_en \
    | node -e 'const d=JSON.parse(require("fs").readFileSync(0,"utf8"));
               const bv=Array.isArray(d)?d[1]:d;
               process.stdout.write(JSON.stringify(bv.rebuilt))' \
    | catala testcase write --language en > rebuilt_details.catala_en \
    || { echo "FAIL: could not write a rebuilt test"; exit 1; }

# each field keeps its OWN type, whatever order the test wrote them in
grep -q -- '-- stamp: |2026-01-01|' rebuilt_details.catala_en \
    || { echo "FAIL: a date field was not written as a date"; exit 1; }
grep -q -- '-- rank: 3' rebuilt_details.catala_en \
    || { echo "FAIL: an integer field was not written as an integer"; exit 1; }
grep -q -- '-- fee: \$12.00' rebuilt_details.catala_en \
    || { echo "FAIL: a money field was not written as money"; exit 1; }
# ...and in DECLARATION order (the test wrote stamp, rank, fee), so a rebuilt
# test writes the same bytes an ordinary read of it would.
[ "$(grep -o -- '-- [a-z]*:' rebuilt_details.catala_en | tr '\n' ' ')" = "-- rank: -- fee: -- stamp: " ] \
    || { echo "FAIL: a rebuilt struct keeps the test's field order, not the declaration's"; exit 1; }


# ── why a rebuild could not proceed ─────────────────────────────────────────
# A renamed scope and a module that will not build are distinct notes, and the
# latter carries the compiler's diagnostic. Built here rather than committed: a
# module that does not compile cannot sit in a project clerk scans.
notes_scratch=$(mktemp -d)

# a scope that was renamed: the module compiles perfectly
mkdir -p "$notes_scratch/renamed"
cp clerk.toml optionals.catala_en test_optionals.catala_en "$notes_scratch/renamed"/
sed -i 's/\bGrant\b/Attribution/g' "$notes_scratch/renamed/optionals.catala_en"
(cd "$notes_scratch/renamed" && clerk start >/dev/null 2>&1 \
    && catala testcase rebuild test_optionals.catala_en 2>/dev/null) > "$notes_scratch/renamed.json"
grep -q '"ScopeNotFound"' "$notes_scratch/renamed.json" \
    || { echo "FAIL: a renamed scope is not reported as a missing scope"; exit 1; }
grep -q '"Attribution"' "$notes_scratch/renamed.json" \
    || { echo "FAIL: the note does not name the scopes the module does have"; exit 1; }
if grep -q '"ModuleWontCompile"' "$notes_scratch/renamed.json"; then
    echo "FAIL: a module that compiles was blamed for a renamed scope"; exit 1
fi

# a field renamed in the declaration only: the module no longer compiles
mkdir -p "$notes_scratch/broken"
cp clerk.toml optionals.catala_en test_optionals.catala_en "$notes_scratch/broken"/
sed -i 's/  input base content money/  input assiette content money/' "$notes_scratch/broken/optionals.catala_en"
(cd "$notes_scratch/broken" && clerk start >/dev/null 2>&1 \
    && catala testcase rebuild test_optionals.catala_en 2>/dev/null) > "$notes_scratch/broken.json"
grep -q '"ModuleWontCompile"' "$notes_scratch/broken.json" \
    || { echo "FAIL: a module that does not compile is not reported as such"; exit 1; }
grep -q 'unknown identifier' "$notes_scratch/broken.json" \
    || { echo "FAIL: the compiler's own diagnostic was not kept"; exit 1; }

# either way the tester's values survive: that is the whole point of the view
for f in renamed broken; do
    n=$(grep -o '"testing_scope"' "$notes_scratch/$f.json" | wc -l)
    [ "$n" = 3 ] || { echo "FAIL: $f recovered $n of 3 tests"; exit 1; }
done


# ── a constructor the test wrote bare ───────────────────────────────────────
# A bare constructor names no enum; the sentinel the partial read records must
# never be written as a name.
for f in partial_test_bare_roundtrip.catala_en; do
    if grep -q 'unknown\.' "$f"; then
        echo "FAIL: the unknown-enum sentinel was written as a name"; exit 1
    fi
done
# with no live type, it is written bare for Catala to infer
grep -q 'equals Present content Green' partial_test_bare_roundtrip.catala_en \
    || { echo "FAIL: a bare constructor was not written bare"; exit 1; }

# ...but a rebuild has the live type, and the value adopts it
catala testcase rebuild test_bare.catala_en \
    | node -e 'const d=JSON.parse(require("fs").readFileSync(0,"utf8"));
               const bv=Array.isArray(d)?d[1]:d;
               process.stdout.write(JSON.stringify(bv.rebuilt))' \
    | catala testcase write --language en > rebuilt_bare.catala_en \
    || { echo "FAIL: could not write a rebuilt bare-constructor test"; exit 1; }
grep -q 'equals Present content Bare.Colour.Green' rebuilt_bare.catala_en \
    || { echo "FAIL: a rebuilt value did not adopt the live type's name"; exit 1; }
clerk typecheck rebuilt_bare.catala_en >/dev/null 2>&1 \
    || { echo "FAIL: the rebuilt bare-constructor test does not typecheck"; exit 1; }


# ── an option's constructor is not a keyword ────────────────────────────────
# Both readers carry an option with the runtime's constructor names, which the
# editor's option form is built from -- never the surface keyword.
for reader in read partial-read; do
    catala testcase $reader test_opt_fr.catala_fr | grep -q '"Present"' \
        || { echo "FAIL: $reader does not use the runtime's name for an option"; exit 1; }
    if catala testcase $reader test_opt_fr.catala_fr | grep -q '"Présent"'; then
        echo "FAIL: $reader spells an option's constructor with a surface keyword"; exit 1
    fi
done

# the surface keyword belongs to the writer
catala testcase read test_opt_fr.catala_fr | catala testcase write --language fr \
    > opt_fr_roundtrip.catala_fr
grep -q 'Présent contenu 50,00' opt_fr_roundtrip.catala_fr \
    || { echo "FAIL: the writer does not emit the French keyword"; exit 1; }
clerk typecheck opt_fr_roundtrip.catala_fr >/dev/null 2>&1 \
    || { echo "FAIL: the French round-trip does not typecheck"; exit 1; }


# ── write must not emit more than partial-read accepts ──────────────────────
# `write` and `partial-read` must compose, as `read` and `write` do above: one
# unreadable definition loses the whole test. Multi-unit durations are the
# known case.
for f in test_implicit_import test_context_vars test_optionals test_items \
         test_bare test_spans test_plain_block test_heading; do
    catala testcase read $f.catala_en | catala testcase write --language en \
        > written_$f.catala_en \
        || { echo "FAIL: could not write $f"; exit 1; }
    catala testcase partial-read written_$f.catala_en >/dev/null 2>&1 \
        || { echo "FAIL: partial-read cannot read back what write emitted for $f"; exit 1; }
    # ...and writing is idempotent: a file the editor wrote is not rewritten
    # differently the next time it is saved.
    catala testcase read written_$f.catala_en | catala testcase write --language en \
        > written2_$f.catala_en \
        || { echo "FAIL: could not read back what write emitted for $f"; exit 1; }
    cmp -s written_$f.catala_en written2_$f.catala_en \
        || { echo "FAIL: write is not idempotent on $f"; diff written_$f.catala_en written2_$f.catala_en; exit 1; }
    # the file says who owns it, once
    [ "$(grep -c 'Written by the Catala testcase editor' written_$f.catala_en)" = 1 ] \
        || { echo "FAIL: written $f does not carry the editor's header exactly once"; exit 1; }
done

# the case that motivated it, spelled out: a duration of more than one unit
grep -q 'equals 1 year + 2 month + 3 day' written_test_spans.catala_en \
    || { echo "FAIL: write no longer emits a joined duration; check this test still bites"; exit 1; }
catala testcase partial-read test_spans.catala_en \
    | grep -q '"years":1,"months":2,"days":3' \
    || { echo "FAIL: a multi-unit duration was not recovered"; exit 1; }


# ── a field renamed, everything else carried ────────────────────────────────
# The ordinary case: one input renamed in the scope, so the test no longer
# reads, and every OTHER field comes across. The renamed field has nothing to
# carry and says so.
mkdir -p "$notes_scratch/field"
cp clerk.toml test_optionals.catala_en "$notes_scratch/field"/
sed 's/\bbase\b/amount/g' optionals.catala_en > "$notes_scratch/field/optionals.catala_en"
(cd "$notes_scratch/field" && clerk start >/dev/null 2>&1 \
    && catala testcase rebuild test_optionals.catala_en 2>/dev/null) > "$notes_scratch/field.json"
node -e '
  const d = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
  if (d.notes.length) { console.error("FAIL: a renamed field produced a note: " + JSON.stringify(d.notes)); process.exit(1); }
  if (d.rebuilt.length !== 3) { console.error("FAIL: rebuilt " + d.rebuilt.length + " of 3 tests"); process.exit(1); }
  const by = {};
  for (const c of d.carry_outcomes) { const k = Array.isArray(c.outcome) ? c.outcome[0] : c.outcome; by[k] = (by[k] || 0) + 1; }
  // per test: amount (new) unset; bonus and total carried wherever the test set them
  const amount = d.carry_outcomes.filter((c) => c.field === "amount").map((c) => c.outcome);
  if (amount.length !== 3 || amount.some((o) => o !== "WasUnset"))
    { console.error("FAIL: the renamed field should be unset in all 3 tests: " + JSON.stringify(amount)); process.exit(1); }
  const bonus = d.carry_outcomes.filter((c) => c.field === "bonus").map((c) => c.outcome);
  if (bonus.length !== 3 || bonus.some((o) => o !== "Fits"))
    { console.error("FAIL: an unchanged field did not carry: " + JSON.stringify(bonus)); process.exit(1); }
  // total is asserted by two tests; the third never asserts it, and an output
  // a test does not assert is not damage to report
  const total = d.carry_outcomes.filter((c) => c.field === "total").map((c) => c.outcome);
  if (total.length !== 2 || total.some((o) => o !== "Fits"))
    { console.error("FAIL: expected two carried assertions on total and silence for the unasserted one: " + JSON.stringify(total)); process.exit(1); }
  if (Object.keys(by).some((k) => k !== "Fits" && k !== "WasUnset"))
    { console.error("FAIL: unexpected outcomes " + JSON.stringify(by)); process.exit(1); }
' "$notes_scratch/field.json" || exit 1

# ── a scope the editor cannot describe ──────────────────────────────────────
# The module compiles and has the scope, but it gained an input of a type the
# editor has no form for. Nothing to rebuild against; the note says what.
mkdir -p "$notes_scratch/other"
cp clerk.toml test_optionals.catala_en "$notes_scratch/other"/
sed 's/^  input bonus content optional of money$/  input bonus content optional of money\n  input rate content decimal depends on x content integer/' optionals.catala_en \
    > "$notes_scratch/other/optionals.catala_en"
(cd "$notes_scratch/other" && clerk start >/dev/null 2>&1 \
    && catala testcase rebuild test_optionals.catala_en 2>/dev/null) > "$notes_scratch/other.json"
node -e '
  const d = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
  const n = d.notes.find(([k]) => k === "Other");
  if (!n) { console.error("FAIL: expected an Other note, got " + JSON.stringify(d.notes)); process.exit(1); }
  if (!/unsupported: function type/.test(n[1].error))
    { console.error("FAIL: the note does not say what was unsupported: " + n[1].error); process.exit(1); }
  if (d.rebuilt.length !== 0) { console.error("FAIL: rebuilt against a scope the editor cannot describe"); process.exit(1); }
' "$notes_scratch/other.json" || exit 1

# ── rebuilding against a scope the tester chose ─────────────────────────────
# Candidates ranked by shared field names, never picked. `--scope` is the
# tester's answer; a working copy saved that way remembers it.
mkdir -p "$notes_scratch/picker"
cp clerk.toml optionals.catala_en test_optionals.catala_en "$notes_scratch/picker"/
# two scopes: the renamed one, and an unrelated one that shares no field name
python3 - "$notes_scratch/picker/optionals.catala_en" <<'PYEOF'
import sys
p = sys.argv[1]
s = open(p).read().replace('scope Grant', 'scope Attribution')
s += '''
```catala-metadata
declaration scope Unrelated:
  input widget content integer
  output gadget content integer
```

```catala
scope Unrelated:
  definition gadget equals widget
```
'''
open(p, 'w').write(s)
PYEOF
(cd "$notes_scratch/picker" && clerk start >/dev/null 2>&1 \
    && catala testcase rebuild test_optionals.catala_en 2>/dev/null) > "$notes_scratch/picker.json"
# the renamed scope shares base/bonus/total; Unrelated shares nothing: ranked first
node -e '
  const d = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
  const n = d.notes.find(([k]) => k === "ScopeNotFound");
  if (!n) { console.error("FAIL: no ScopeNotFound note"); process.exit(1); }
  const c = n[1].candidates;
  if (c[0].name !== "Attribution" || c[0].shared !== 3)
    { console.error("FAIL: best candidate is " + JSON.stringify(c[0])); process.exit(1); }
  if (c[1].name !== "Unrelated" || c[1].shared !== 0)
    { console.error("FAIL: second candidate is " + JSON.stringify(c[1])); process.exit(1); }
' "$notes_scratch/picker.json" || exit 1

# the tester answers: a full rebuild against the chosen scope
(cd "$notes_scratch/picker" \
    && catala testcase rebuild --scope Attribution test_optionals.catala_en 2>/dev/null) > "$notes_scratch/picked.json"
grep -q '"notes":\[\]' "$notes_scratch/picked.json" \
    || { echo "FAIL: rebuilding against the chosen scope still reports a note"; exit 1; }
n=$(grep -o '"Fits"' "$notes_scratch/picked.json" | wc -l)
[ "$n" -ge 6 ] || { echo "FAIL: only $n fields carried against the chosen scope"; exit 1; }

# ...and the answer survives a save
node -e '
  const d = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
  process.stdout.write(JSON.stringify(d.rebuilt));
' "$notes_scratch/picked.json" \
    | catala testcase write --language en > "$notes_scratch/picker/test_optionals.catala_en.updated"
(cd "$notes_scratch/picker" && catala testcase rebuild test_optionals.catala_en 2>/dev/null) > "$notes_scratch/reopened.json"
grep -q '"notes":\[\]' "$notes_scratch/reopened.json" \
    || { echo "FAIL: reopening forgot which scope the tester chose"; exit 1; }
grep -q '"name":"Attribution"' "$notes_scratch/reopened.json" \
    || { echo "FAIL: the reopened rebuild does not target the chosen scope"; exit 1; }

# ── rebuilding against a scope in another module ────────────────────────────
# The module itself is gone (renamed): candidates come from anywhere in the
# project, surface-parsed only, and a qualified `--scope` retargets the test.
# The working copy then names the new module, so reopening remembers it.
mkdir -p "$notes_scratch/modrename"
cp clerk.toml test_optionals.catala_en "$notes_scratch/modrename"/
sed 's/^> Module Optionals$/> Module Benefits/' optionals.catala_en > "$notes_scratch/modrename/benefits.catala_en"
(cd "$notes_scratch/modrename" && clerk start >/dev/null 2>&1 \
    && catala testcase rebuild test_optionals.catala_en 2>/dev/null) > "$notes_scratch/modrename.json"
node -e '
  const d = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
  const n = d.notes.find(([k]) => k === "ModuleNotFound");
  if (!n) { console.error("FAIL: no ModuleNotFound note"); process.exit(1); }
  if (n[1].module_name !== "Optionals")
    { console.error("FAIL: note names " + n[1].module_name); process.exit(1); }
  const c = n[1].candidates[0];
  if (!c || c.module_name !== "Benefits" || c.name !== "Grant" || c.shared !== 3)
    { console.error("FAIL: best candidate is " + JSON.stringify(c)); process.exit(1); }
' "$notes_scratch/modrename.json" || exit 1
(cd "$notes_scratch/modrename" \
    && catala testcase rebuild --scope Benefits.Grant test_optionals.catala_en 2>/dev/null) > "$notes_scratch/modpicked.json"
grep -q '"notes":\[\]' "$notes_scratch/modpicked.json" \
    || { echo "FAIL: rebuilding against a scope in another module still reports a note"; exit 1; }
grep -q '"module_name":"Benefits"' "$notes_scratch/modpicked.json" \
    || { echo "FAIL: the rebuild does not target the new module"; exit 1; }
node -e '
  const d = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
  process.stdout.write(JSON.stringify(d.rebuilt));
' "$notes_scratch/modpicked.json" \
    | catala testcase write --language en > "$notes_scratch/modrename/test_optionals.catala_en.updated"
grep -q '^> Using Benefits$' "$notes_scratch/modrename/test_optionals.catala_en.updated" \
    || { echo "FAIL: the working copy does not use the new module"; exit 1; }
(cd "$notes_scratch/modrename" && catala testcase rebuild test_optionals.catala_en 2>/dev/null) > "$notes_scratch/modreopened.json"
grep -q '"notes":\[\]' "$notes_scratch/modreopened.json" \
    || { echo "FAIL: reopening forgot which module the tester chose"; exit 1; }
# ...and the working copy can be RUN from memory, the way the editor runs it:
# fed on stdin with the broken original as buffer path. The plugins for the new
# module must get built although clerk refuses the original.
(cd "$notes_scratch/modrename" \
    && catala testcase run -l en -s Grant_absent --buffer-path test_optionals.catala_en - \
         < test_optionals.catala_en.updated 2>/dev/null) > "$notes_scratch/modrun.json"
grep -q '"assert_failures":false' "$notes_scratch/modrun.json" \
    || { echo "FAIL: the retargeted working copy could not be run"; exit 1; }
if [ -e "$notes_scratch/modrename/test_optionals__run.catala_en" ]; then
    echo "FAIL: the run left its temporary file behind"; exit 1
fi
# ...and a run that FAILS reports what it disagrees with, not just that it did.
sed 's/\.total = \$1000\.00)/.total = $999.00)/' \
    "$notes_scratch/modrename/test_optionals.catala_en.updated" > "$notes_scratch/modrename/failing.txt"
(cd "$notes_scratch/modrename" \
    && catala testcase run -l en -s Grant_absent --buffer-path test_optionals.catala_en - \
         < failing.txt 2>/dev/null) > "$notes_scratch/modfail.json"
node -e '
  const d = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
  if (d.assert_failures !== true) { console.error("FAIL: a wrong expectation did not fail the run"); process.exit(1); }
  if (!d.diffs.length) { console.error("FAIL: a failing run reported no diff"); process.exit(1); }
  const p = d.diffs[0].path.map((s) => s[1]).join(".");
  if (!/total/.test(p)) { console.error("FAIL: the diff is not on total: " + p); process.exit(1); }
' "$notes_scratch/modfail.json" || exit 1

# ── a value the live declaration no longer allows ───────────────────────────
# A struct that lost a field the test filled, and an enum constructor that now
# requires a payload. Neither value fits, and the rule these pin is stronger
# than either case: whatever a rebuild carries, its working copy reads back.

# the struct loses `stamp`, which the test fills
mkdir -p "$notes_scratch/lostfield"
cp clerk.toml details.catala_en test_details.catala_en "$notes_scratch/lostfield"/
sed -i '/data stamp content date/d' "$notes_scratch/lostfield/details.catala_en"
(cd "$notes_scratch/lostfield" && clerk start >/dev/null 2>&1 \
    && catala testcase rebuild test_details.catala_en 2>/dev/null) > "$notes_scratch/lostfield.json"

# the enum constructor the test wrote bare now wants a payload
mkdir -p "$notes_scratch/payload"
cp clerk.toml bare.catala_en test_bare.catala_en "$notes_scratch/payload"/
sed -i 's/^  -- Green$/  -- Green content money/' "$notes_scratch/payload/bare.catala_en"
(cd "$notes_scratch/payload" && clerk start >/dev/null 2>&1 \
    && catala testcase rebuild test_bare.catala_en 2>/dev/null) > "$notes_scratch/payload.json"

# ── context vars through a rebuild ──────────────────────────────────────────
# A context var the test never overrode is not damage: the rebuilt field
# defaults, like the authored one. z is `context output`: its In side must
# stay silent while its assertion carries on the Out side.
mkdir -p "$notes_scratch/ctx"
cp clerk.toml test_context_vars.catala_en "$notes_scratch/ctx"/
sed 's/\bx\b/amount/g' context_vars.catala_en > "$notes_scratch/ctx/context_vars.catala_en"
(cd "$notes_scratch/ctx" && clerk start >/dev/null 2>&1 \
    && catala testcase rebuild test_context_vars.catala_en 2>/dev/null) > "$notes_scratch/ctx.json"
node -e '
  const d = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
  const marks = d.carry_outcomes.map((c) =>
    c.field + ":" + c.io + ":" + (Array.isArray(c.outcome) ? c.outcome[0] : c.outcome)).sort();
  const want = ["amount:In:WasUnset", "y:In:Fits", "z:Out:Fits"];
  if (JSON.stringify(marks) !== JSON.stringify(want))
    { console.error("FAIL: expected marks " + want + ", got " + marks); process.exit(1); }
' "$notes_scratch/ctx.json" || exit 1

for case in lostfield:detail:test_details payload:shade:test_bare; do
    dir=${case%%:*}; rest=${case#*:}; field=${rest%%:*}; test=${rest##*:}
    node -e '
      const d = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
      const c = d.carry_outcomes.find((c) => c.field === process.argv[2]);
      const k = c && (Array.isArray(c.outcome) ? c.outcome[0] : c.outcome);
      if (k !== "TypeChanged")
        { console.error("FAIL: a value the declaration no longer allows must not fit: " + JSON.stringify(c)); process.exit(1); }
      process.stdout.write(JSON.stringify(d.rebuilt));
    ' "$notes_scratch/$dir.json" "$field" \
        | catala testcase write --language en > "$notes_scratch/$dir/rebuilt.catala_en" || exit 1
    (cd "$notes_scratch/$dir" \
        && clerk typecheck rebuilt.catala_en >/dev/null 2>&1 \
        && catala testcase read rebuilt.catala_en >/dev/null 2>&1) \
        || { echo "FAIL: the $dir working copy does not read back"; exit 1; }
done
