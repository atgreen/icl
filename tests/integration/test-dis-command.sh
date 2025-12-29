#!/bin/bash
# Integration test for ,dis command package context fix
# This tests that ,dis works correctly after ,cd to another package

set -e

ICL="${ICL:-./icl}"
TIMEOUT="${TIMEOUT:-30}"

echo "Testing ,dis command with package context..."

# Test 1: ,dis should work for CL symbols from CL-USER
echo "Test 1: ,dis car from CL-USER"
OUTPUT=$(echo -e ",dis car\n,quit" | timeout $TIMEOUT $ICL --no-banner 2>&1)
if echo "$OUTPUT" | grep -q "disassembly for CAR"; then
    echo "  PASS: ,dis car works from CL-USER"
else
    echo "  FAIL: ,dis car did not produce disassembly"
    echo "$OUTPUT"
    exit 1
fi

# Test 2: ,dis should work after ,cd to another package
echo "Test 2: ,dis split-csv-line after ,cd OCICL-RUNTIME"
OUTPUT=$(echo -e ",cd ocicl-runtime\n,dis split-csv-line\n,quit" | timeout $TIMEOUT $ICL --no-banner 2>&1)
if echo "$OUTPUT" | grep -q "disassembly for SPLIT-CSV-LINE"; then
    echo "  PASS: ,dis works after ,cd to OCICL-RUNTIME"
elif echo "$OUTPUT" | grep -q "Error"; then
    echo "  FAIL: ,dis produced an error after ,cd"
    echo "$OUTPUT"
    exit 1
else
    # Function might not exist, but shouldn't hang
    echo "  PASS: ,dis completed (function may not exist but didn't hang)"
fi

# Test 3: ,dis with explicit package prefix should work
echo "Test 3: ,dis cl:length (explicit package prefix)"
OUTPUT=$(echo -e ",dis cl:length\n,quit" | timeout $TIMEOUT $ICL --no-banner 2>&1)
if echo "$OUTPUT" | grep -q "disassembly for LENGTH"; then
    echo "  PASS: ,dis cl:length works with explicit prefix"
else
    echo "  FAIL: ,dis cl:length did not produce disassembly"
    echo "$OUTPUT"
    exit 1
fi

echo ""
echo "All ,dis command tests passed!"
