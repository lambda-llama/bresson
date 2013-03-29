#! /bin/bash

cabal configure --enable-tests --enable-benchmarks -fdevtools
cabal build
cabal test
cabal bench --benchmark-option="-o new-index.html"

git config --global user.name "Travis CI"
git config --global user.email "knsd@knsd.net"

git remote set-url origin https://${GH_TOKEN}@github.com/knsd/bresson.git

git checkout -B gh-pages

mv new-index.html index.html

git add index.html
git commit -m "Travis build $TRAVIS_BUILD_NUMBER"
git push
