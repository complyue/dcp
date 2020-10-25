#!/usr/bin/env bash

cd "$(dirname "$0")"

cabal run dcp:dcp < samples/basic.txt
