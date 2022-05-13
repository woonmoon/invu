#!/bin/bash

rm -r inputs
mkdir inputs
mkdir inputs/jumped_inputs
mkdir inputs/non_jumped_inputs
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color
for i in {1..50}
do
    filename="input${i}.txt"
    lein run > "$filename"
    passed=`./test_jumps.py "$filename"`
    if grep -q true "$filename";
    then
        mv "$filename" inputs/jumped_inputs
    else
        mv "$filename" inputs/non_jumped_inputs
    fi
    if $passed;
    then
        printf "$filename ------------------------------ ${GREEN}PASSED${NC}\n"
    else
        printf "$filename ------------------------------ ${RED}FAILED${NC}\n"
    fi
done