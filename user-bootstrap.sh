#!/bin/bash
set -x

cd src
git submodule init
git submodule update
cabal update
cabal install --enable-tests