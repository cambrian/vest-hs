#!/bin/bash
# Updates any volatile dependencies (those that aren't under some package control system, like our
# own executables) and places them in the Haskell local bin (which should be in your path after
# running setup.sh).

cd $TMPDIR
rm -rf eztz-simple
echo "Cloning eztz-simple CLI."
git clone https://github.com/1protocol/eztz-simple > ~/.vest-hs/eztz-simple-clone.log 2>&1
cd eztz-simple
echo "Installing build deps for eztz-simple."
npm install > ~/.vest-hs/eztz-simple-install.log 2>&1
echo "Building eztz-simple."
npm run-script build > ~/.vest-hs/eztz-simple-build.log 2>&1
cp -f lib/eztz-simple ~/.local/bin
chmod +x ~/.local/bin/eztz-simple
echo "Copied eztz-simple to Haskell bin."