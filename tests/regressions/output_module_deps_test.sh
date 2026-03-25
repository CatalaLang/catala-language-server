#!/bin/bash
# Regression test for missing module deps from output-only scope fields
# See commit "Fix missing module deps for output-only scope fields"
#
# The bug: when generating test cases for scopes where some output types come
# from a module not referenced in any input, the corresponding "Using" import
# line was missing from the generated test file, causing "Module not found"
# errors when running the test.
#
# This test generates a test case for a scope whose output type (Output_types.EOut)
# comes from a module not referenced in any input, and verifies that the
# "Using Output_types" import is present in the generated file.

set -e
cd "$(dirname "$0")/output_module_deps_fixtures"

clerk start

output=$(catala testcase generate my_scope.catala_en --scope S | catala testcase write -l en 2>&1)

if ! echo "$output" | grep -q '> Using Output_types'; then
    echo "output_module_deps_test: FAIL - missing '> Using Output_types' import for output-only module"
    echo "Generated output:"
    echo "$output"
    exit 1
fi

if ! echo "$output" | grep -q '> Using Input_types'; then
    echo "output_module_deps_test: FAIL - missing '> Using Input_types' import"
    echo "Generated output:"
    echo "$output"
    exit 1
fi

echo "output_module_deps_test: PASS"
