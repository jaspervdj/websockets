#!/bin/bash

EXCLUDES=$(find tests/haskell -name '*.hs' |
        xargs sed -n 's/^module //p' |
        sed 's/^/--exclude=/' |
        xargs echo)

TARGET=websockets-tests

cabal configure --enable-tests && cabal build
./dist/build/$TARGET/$TARGET

mkdir -p tests/coverage
hpc markup --destdir=tests/coverage --exclude=Main $EXCLUDES $TARGET.tix
hpc report --exclude=Main $EXCLUDES $TARGET.tix
rm $TARGET.tix

echo "Output written to tests/coverage/hpc_index.html"
