#! /bin/bash

set -e

cabal configure --enable-tests --enable-benchmarks -fdevtools
cabal build
cabal test

cabal bench --benchmark-option='--output=$benchmark.html'

cabal-dev haddock --hyperlink-source --html-location='http://hackage.haskell.org/packages/archive/$pkg/$version/doc/html'

exec > /dev/null 2>&1

git config --global user.name "Travis CI"
git config --global user.email "ci+bresson@knsd.net"

git clone https://${GH_TOKEN}@github.com/knsd/bresson.git

cd bresson
git checkout -b gh-pages origin/gh-pages

mv ../*.html .
git add *.html

mv ../dist/doc/html/bresson docs
git add docs

git commit -m "Travis build $TRAVIS_BUILD_NUMBER"
git push
