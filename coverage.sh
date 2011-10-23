#!/bin/bash

EXCLUDES=$(find tests/haskell -name '*.hs' |
        xargs sed -n 's/^module //p' |
        sed 's/^/--exclude=/' |
        xargs echo)

ghc -isrc -itests/haskell -fhpc --make tests/haskell/TestSuite.hs
./tests//haskell/TestSuite
mkdir -p tests/coverage
hpc markup --destdir=tests/coverage --exclude=Main $EXCLUDES TestSuite.tix
hpc report --exclude=Main $EXCLUDES TestSuite.tix
rm TestSuite.tix

echo "Output written to tests/coverage/hpc_index.html"
