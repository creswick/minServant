#!/bin/bash

cabal sandbox init
cabal sandbox add-source deps/hssqlppp/hssqlppp/
cabal sandbox add-source deps/hssqlppp/hssqlppp-th/
cabal sandbox add-source deps/hssqlppp/hssqlppp-pg/
cabal install --only-dep --enable-tests
cabal configure --enable-tests
cabal build
