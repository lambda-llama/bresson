#! /bin/bash

set -e

cabal configure --enable-tests --enable-benchmarks -fdevtools
cabal build
cabal test
cabal bench --benchmark-option="-onew-index.html"

git config --global user.name "Travis CI"
git config --global user.email "ci-bresson@knsd.net"

rm -rf .git

git clone https://${GH_TOKEN}@github.com/knsd/bresson.git

cd bresson

git checkout -b gh-pages origin/gh-pages

mv ../new-index.html index.html

git add index.html
git commit -m "Travis build $TRAVIS_BUILD_NUMBER"
git push
