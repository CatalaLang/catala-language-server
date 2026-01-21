#!/bin/bash
# Regression test for enum path serialization
# See commit "Remove module_name shenanigans"
#
# The bug: when generating test cases for scopes with enums accessed through
# nested module aliases (like MMS.ME.E), the enum paths were incorrectly
# serialized as "My_struct.My_enum.E" instead of the correct "My_enum.E".
#
# This test generates a test case and verifies the enum paths are correct
# (i.e., don't contain redundant module prefixes).

set -e
cd "$(dirname "$0")/.."

# Generate test case and check for incorrect long paths
output=$(catala testcase generate my_scope.catala_en --scope S 2>&1)

# The bug would produce "My_struct.My_enum.E" - this should NOT appear
if echo "$output" | grep -q '"enum_name":"My_struct.My_enum.E"'; then
    echo "enum_path_test: FAIL - found incorrect long enum path 'My_struct.My_enum.E'"
    echo "Expected all enum paths to be 'My_enum.E'"
    exit 1
fi

# Verify the correct short path is present
if ! echo "$output" | grep -q '"enum_name":"My_enum.E"'; then
    echo "enum_path_test: FAIL - missing expected enum path 'My_enum.E'"
    exit 1
fi

echo "enum_path_test: PASS"
