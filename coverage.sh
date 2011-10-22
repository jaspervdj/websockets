#!/bin/bash

ghc -isrc -itests/haskell -fhpc --make tests/haskell/TestSuite.hs
./tests//haskell/TestSuite
mkdir -p tests/coverage
hpc markup --destdir=tests/coverage --exclude=Main TestSuite.tix
hpc report TestSuite.tix
rm TestSuite.tix

echo "Output written to tests/coverage/hpc_index.html"
