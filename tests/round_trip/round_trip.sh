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
    rm -f dup_assert.catala_en unqual.catala_en

# ── a test that does not name its module ────────────────────────────────────
# Only a hand edit unqualifies the tested scope; guessing a module would
# rebuild against the wrong one. Refused with a reason, never a bare crash.
sed 's/scope Optionals\.Grant/scope Grant/' test_optionals.catala_en > unqual.catala_en
out=$(catala testcase partial-read unqual.catala_en 2>&1)
n=$(echo "$out" | grep -o '"testing_scope"' | wc -l)
[ "$n" = 0 ] || { echo "FAIL: partial read kept $n unqualified tests"; exit 1; }
echo "$out" | grep -q "does not say which module" \
    || { echo "FAIL: the refusal does not explain the missing module"; exit 1; }
if catala testcase rebuild unqual.catala_en >/dev/null 2>&1; then
    echo "FAIL: rebuild accepted a test that does not name its module"; exit 1
fi
catala testcase rebuild unqual.catala_en 2>&1 | grep -q "does not say which module" \
    || { echo "FAIL: the rebuild refusal does not explain the missing module"; exit 1; }
rm -f unqual.catala_en
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


# ── the readers must spell the overlap identically ──────────────────────────
# The same file through read|write and partial-read|write, byte for byte:
# write prints no types, so this compares exactly the spelling of every value,
# name and attribute. test_bare is exempt by design: it authors a constructor
# bare, and a partial read will not invent the enum name a full read learns
# from the declaration.
for f in test_implicit_import test_context_vars test_optionals test_items test_spans; do
    catala testcase read $f.catala_en | catala testcase write --language en \
        > written2_$f.catala_en
    diff written2_$f.catala_en partial_${f}_roundtrip.catala_en \
        || { echo "FAIL: the readers spell $f differently"; exit 1; }
done

# ...except what write does not render: both readers must type an optional as
# TOption in the JSON, never as a bare enum -- written out, the two look alike.
for reader in read partial-read; do
    catala testcase $reader test_optionals.catala_en | grep -q '"TOption"' \
        || { echo "FAIL: $reader does not type an optional as TOption"; exit 1; }
    if catala testcase $reader test_optionals.catala_en | grep -q '"TEnum"'; then
        echo "FAIL: $reader types an optional as a bare enum"; exit 1
    fi
done

# ...and write must put uids back, or the round trip loses them anyway (a
# writer dropping them would drop them on both sides of the diff above).
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

# ...but a partial read recovers what it can honestly show, one test at a
# time: the GUI-owned test comes through, the hand-written one is excluded
# with a warning naming it -- not quietly slimmed of its `>` assertion.
out=$(catala testcase partial-read mixed.catala_en 2>&1)
n=$(echo "$out" | grep -o '"testing_scope"' | wc -l)
[ "$n" = 1 ] || { echo "FAIL: partial read recovered $n tests, wanted 1"; exit 1; }
echo "$out" | grep -q "Hand_written" \
    || { echo "FAIL: the exclusion does not name the hand-written test"; exit 1; }

# ── a field asserted twice ──────────────────────────────────────────────────
# Only a hand edit produces it; kept, one assertion would silently shadow the
# other (and the run's diff used to crash on the count). Both readers refuse,
# naming the field; partial read keeps the clean tests.
sed 's/^  assertion (calc\.total = \$1000\.00)$/&\n&/' test_optionals.catala_en > dup_assert.catala_en
if catala testcase read dup_assert.catala_en >/dev/null 2>&1; then
    echo "FAIL: read accepted a field asserted twice"; exit 1
fi
catala testcase read dup_assert.catala_en 2>&1 | grep -q "asserted twice" \
    || { echo "FAIL: the refusal does not say the field is asserted twice"; exit 1; }
out=$(catala testcase partial-read dup_assert.catala_en 2>&1)
n=$(echo "$out" | grep -o '"testing_scope"' | wc -l)
[ "$n" = 1 ] || { echo "FAIL: partial read kept $n tests, wanted the 1 clean one"; exit 1; }
echo "$out" | grep -q "asserted twice" \
    || { echo "FAIL: the exclusion does not say why"; exit 1; }
rm -f dup_assert.catala_en

# ── rebuilding a struct ─────────────────────────────────────────────────────
# test_details writes Detail's fields in another order than the module declares
# them and predates a field it has since gained: the writer must pair fields by
# name.
catala testcase rebuild test_details.catala_en \
    | node -e 'const d=JSON.parse(require("fs").readFileSync(0,"utf8"));
               const bv=Array.isArray(d)?d[1]:d;
               process.stdout.write(JSON.stringify(bv.tests.map((t)=>t.rebuilt)))' \
    | catala testcase write --language en > rebuilt_details.catala_en \
    || { echo "FAIL: could not write a rebuilt test"; exit 1; }

# each field keeps its OWN type, whatever order the test wrote them in
grep -q -- '-- stamp: |2026-01-01|' rebuilt_details.catala_en \
    || { echo "FAIL: a date field was not written as a date"; exit 1; }
grep -q -- '-- rank: 3' rebuilt_details.catala_en \
    || { echo "FAIL: an integer field was not written as an integer"; exit 1; }
grep -q -- '-- fee: \$12.00' rebuilt_details.catala_en \
    || { echo "FAIL: a money field was not written as money"; exit 1; }
# ...in DECLARATION order (the test wrote stamp, rank, fee), so a rebuilt test
# writes the same bytes an ordinary read of it would, and with the field the
# module gained since (note) as a blank to fill, not silently absent.
[ "$(grep -o -- '-- [a-z]*:' rebuilt_details.catala_en | tr '\n' ' ')" = "-- rank: -- fee: -- stamp: -- note: " ] \
    || { echo "FAIL: a rebuilt struct keeps the test's field order, not the declaration's"; exit 1; }
grep -q -- '-- note: impossible' rebuilt_details.catala_en \
    || { echo "FAIL: the field the module gained is not written as a blank"; exit 1; }


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
               process.stdout.write(JSON.stringify(bv.tests.map((t)=>t.rebuilt)))' \
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
  const reb = d.tests.filter((t) => t.rebuilt !== undefined);
  if (reb.length !== 3) { console.error("FAIL: rebuilt " + reb.length + " of 3 tests"); process.exit(1); }
  const outcomes = d.tests.flatMap((t) => t.outcomes);
  const name = (c) => c.path.map((p) => p[1]).join(".");
  const by = {};
  for (const c of outcomes) { const k = Array.isArray(c.outcome) ? c.outcome[0] : c.outcome; by[k] = (by[k] || 0) + 1; }
  // per test: amount (new) unset; bonus and total carried wherever the test set them
  const amount = outcomes.filter((c) => name(c) === "amount").map((c) => c.outcome);
  if (amount.length !== 3 || amount.some((o) => o !== "WasUnset"))
    { console.error("FAIL: the renamed field should be unset in all 3 tests: " + JSON.stringify(amount)); process.exit(1); }
  const bonus = outcomes.filter((c) => name(c) === "bonus").map((c) => c.outcome);
  if (bonus.length !== 3 || bonus.some((o) => o !== "Fits"))
    { console.error("FAIL: an unchanged field did not carry: " + JSON.stringify(bonus)); process.exit(1); }
  // total is asserted by two tests; the third never asserts it, and an output
  // a test does not assert is not damage to report
  const total = outcomes.filter((c) => name(c) === "total").map((c) => c.outcome);
  if (total.length !== 2 || total.some((o) => o !== "Fits"))
    { console.error("FAIL: expected two carried assertions on total and silence for the unasserted one: " + JSON.stringify(total)); process.exit(1); }
  // the lost name is reported as deleted-on-promotion, in each test that set it
  const base = outcomes.filter((c) => name(c) === "base").map((c) => c.outcome);
  if (base.length !== 3 || base.some((o) => o !== "Dropped"))
    { console.error("FAIL: the lost field should be Dropped in all 3 tests: " + JSON.stringify(base)); process.exit(1); }
  if (Object.keys(by).some((k) => k !== "Fits" && k !== "WasUnset" && k !== "Dropped"))
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
  if (d.tests.some((t) => t.rebuilt !== undefined)) { console.error("FAIL: rebuilt against a scope the editor cannot describe"); process.exit(1); }
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
  process.stdout.write(JSON.stringify(d.tests.map((t) => t.rebuilt)));
' "$notes_scratch/picked.json" \
    | catala testcase write --language en > "$notes_scratch/picker/test_optionals.catala_en.repair"
(cd "$notes_scratch/picker" && catala testcase rebuild test_optionals.catala_en 2>/dev/null) > "$notes_scratch/reopened.json"
grep -q '"notes":\[\]' "$notes_scratch/reopened.json" \
    || { echo "FAIL: reopening forgot which scope the tester chose"; exit 1; }
grep -q '"name":"Attribution"' "$notes_scratch/reopened.json" \
    || { echo "FAIL: the reopened rebuild does not target the chosen scope"; exit 1; }

# ── rebuilding against a scope in another module ────────────────────────────
# The module itself is gone (renamed): candidates come from anywhere in the
# project, surface-parsed only, and a qualified `--scope` retargets the test.
# The working copy then names the new module, so reopening remembers it.
# The new module is a literate `.catala_en.md` file: discovery must fold the
# `.md` into the extension or such modules are invisible.
mkdir -p "$notes_scratch/modrename"
cp clerk.toml test_optionals.catala_en "$notes_scratch/modrename"/
sed 's/^> Module Optionals$/> Module Benefits/' optionals.catala_en > "$notes_scratch/modrename/benefits.catala_en.md"
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
  process.stdout.write(JSON.stringify(d.tests.map((t) => t.rebuilt)));
' "$notes_scratch/modpicked.json" \
    | catala testcase write --language en > "$notes_scratch/modrename/test_optionals.catala_en.repair"
grep -q '^> Using Benefits$' "$notes_scratch/modrename/test_optionals.catala_en.repair" \
    || { echo "FAIL: the working copy does not use the new module"; exit 1; }
(cd "$notes_scratch/modrename" && catala testcase rebuild test_optionals.catala_en 2>/dev/null) > "$notes_scratch/modreopened.json"
grep -q '"notes":\[\]' "$notes_scratch/modreopened.json" \
    || { echo "FAIL: reopening forgot which module the tester chose"; exit 1; }
# ...and the working copy can be RUN from memory, the way the editor runs it:
# fed on stdin with the broken original as buffer path. The plugins for the new
# module must get built although clerk refuses the original.
(cd "$notes_scratch/modrename" \
    && catala testcase run -l en -s Grant_absent --buffer-path test_optionals.catala_en - \
         < test_optionals.catala_en.repair 2>/dev/null) > "$notes_scratch/modrun.json"
grep -q '"assert_failures":false' "$notes_scratch/modrun.json" \
    || { echo "FAIL: the retargeted working copy could not be run"; exit 1; }
if [ -e "$notes_scratch/modrename/test_optionals__run.catala_en" ]; then
    echo "FAIL: the run left its temporary file behind"; exit 1
fi
# ...and from OUTSIDE the project: the editor's cwd is the workspace folder,
# which may sit far above the project. Resolution must come from buffer-path.
(cd "$notes_scratch" \
    && catala testcase run -l en -s Grant_absent --buffer-path modrename/test_optionals.catala_en - \
         < modrename/test_optionals.catala_en.repair 2>/dev/null) > "$notes_scratch/modrun_outside.json"
grep -q '"assert_failures":false' "$notes_scratch/modrun_outside.json" \
    || { echo "FAIL: the run does not work from outside the project"; exit 1; }
# ...and a run that FAILS reports what it disagrees with, not just that it did.
sed 's/\.total = \$1000\.00)/.total = $999.00)/' \
    "$notes_scratch/modrename/test_optionals.catala_en.repair" > "$notes_scratch/modrename/failing.txt"
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
  const marks = d.tests.flatMap((t) => t.outcomes).map((c) =>
    c.path.map((p) => p[1]).join(".") + ":" + c.side + ":" + (Array.isArray(c.outcome) ? c.outcome[0] : c.outcome)).sort();
  const want = ["amount:In:WasUnset", "x:In:Dropped", "y:In:Fits", "z:Out:Fits"];
  if (JSON.stringify(marks) !== JSON.stringify(want))
    { console.error("FAIL: expected marks " + want + ", got " + marks); process.exit(1); }
' "$notes_scratch/ctx.json" || exit 1

for case in lostfield:detail:test_details payload:shade:test_bare; do
    dir=${case%%:*}; rest=${case#*:}; field=${rest%%:*}; test=${rest##*:}
    node -e '
      const d = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
      const c = d.tests.flatMap((t) => t.outcomes).find((c) => c.path.map((p) => p[1]).join(".") === process.argv[2]);
      const k = c && (Array.isArray(c.outcome) ? c.outcome[0] : c.outcome);
      // A whole value the declaration no longer allows is TypeChanged; a
      // record that lost one field carries the others and is Partial. Never Fits.
      if (k === undefined || k === "Fits")
        { console.error("FAIL: a value the declaration no longer allows must not fit: " + JSON.stringify(c)); process.exit(1); }
      process.stdout.write(JSON.stringify(d.tests.map((t) => t.rebuilt)));
    ' "$notes_scratch/$dir.json" "$field" \
        | catala testcase write --language en > "$notes_scratch/$dir/rebuilt.catala_en" || exit 1
    (cd "$notes_scratch/$dir" \
        && clerk typecheck rebuilt.catala_en >/dev/null 2>&1 \
        && catala testcase read rebuilt.catala_en >/dev/null 2>&1) \
        || { echo "FAIL: the $dir working copy does not read back"; exit 1; }
done

# and the tripwire: ordinary read refuses the misfit original, naming the
# spot. (Here the compiler itself refuses, at typechecking; check_tests_fit
# behind it guards the same line for values the compiler cannot see.)
err=$( (cd "$notes_scratch/lostfield" && catala testcase read test_details.catala_en) 2>&1 ) \
    && { echo "FAIL: read accepted a test whose value no longer fits"; exit 1; }
echo "$err" | grep -q "stamp" \
    || { echo "FAIL: the refusal does not name the misfit field"; exit 1; }

# ── mixed ownership at the rebuild door ─────────────────────────────────────
# A drifted file never reaches read's ownership check (the compiler refuses
# first), so rebuild re-checks from the surface (surface_scopes_by_ownership;
# "the two must agree"). Routed into recovery instead, promoting the working
# copy would delete the hand-written test.
mkdir -p "$notes_scratch/mixed"
cp "$notes_scratch/lostfield"/{clerk.toml,details.catala_en,test_details.catala_en} "$notes_scratch/mixed"/
cat >> "$notes_scratch/mixed/test_details.catala_en" <<'EOF'

```catala-metadata
#[test]
declaration scope HandWritten:
  output ok content boolean
```

```catala
scope HandWritten:
  definition ok equals true
```
EOF
err=$( (cd "$notes_scratch/mixed" && catala testcase rebuild test_details.catala_en) 2>&1 ) \
    && { echo "FAIL: rebuild accepted a file with a hand-written test"; exit 1; }
echo "$err" | grep -q "HandWritten" \
    || { echo "FAIL: the refusal does not name the hand-written test"; exit 1; }

# ── an assertion the pane cannot show refuses the test ──────────────────────
# Partial read must not quietly slim a test: an assertion richer than
# field = literal excludes the whole test with a warning, like an unreadable
# definition. Skipped silently, promoting the working copy would delete it.
mkdir -p "$notes_scratch/richassert"
cp clerk.toml details.catala_en test_details.catala_en "$notes_scratch/richassert"/
sed -i 's/^  assertion (calc.total = \$12\.00)$/  assertion (calc.total >= $12.00)/' \
    "$notes_scratch/richassert/test_details.catala_en"
out=$( (cd "$notes_scratch/richassert" && catala testcase partial-read test_details.catala_en) 2>&1 )
echo "$out" | grep -q '"testing_scope"' \
    && { echo "FAIL: partial read kept a test whose assertion it cannot show"; exit 1; }
echo "$out" | grep -q "Record_unordered" \
    || { echo "FAIL: the exclusion does not name the test"; exit 1; }

# ── a scope use split across blocks ─────────────────────────────────────────
# Catala merges a scope's uses; partial read must too, or every block after
# the first would be silently dropped -- and deleted on promotion. Merged,
# the split file must spell exactly like the canonical one.
mkdir -p "$notes_scratch/split"
sed 's/^  assertion (c.z = 198)$/```\n\n```catala\nscope C_test:\n  assertion (c.z = 198)/' \
    test_context_vars.catala_en > "$notes_scratch/split/test_split.catala_en"
catala testcase partial-read "$notes_scratch/split/test_split.catala_en" \
    | catala testcase write --language en > "$notes_scratch/split/roundtrip.catala_en" \
    || { echo "FAIL: partial read of a split scope use"; exit 1; }
diff written2_test_context_vars.catala_en "$notes_scratch/split/roundtrip.catala_en" \
    || { echo "FAIL: a second scope block was dropped or respelled"; exit 1; }
