#!/bin/bash

cabal sandbox init
cabal sandbox add-source deps/hssqlppp/hssqlppp/
cabal sandbox add-source deps/hssqlppp/hssqlppp-th/
cabal sandbox add-source deps/hssqlppp/hssqlppp-pg/
cabal sandbox add-source deps/servant/servant/
cabal sandbox add-source deps/servant/servant-docs/
cabal sandbox add-source deps/servant/servant-js/
cabal sandbox add-source deps/servant/servant-server/
cabal sandbox add-source deps/servant/servant-foreign/
cabal install --only-dep --enable-tests
cabal configure --enable-tests
cabal build
