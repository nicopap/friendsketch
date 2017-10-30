#!/bin/bash

# Finds all functions in a given haskell source file.
sourceFile="$1"

cat <(rg '^[a-zA-Z]+ ::' $sourceFile) <(rg -A 4 '^\w+$' $sourceFile)
