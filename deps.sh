#!/bin/bash
# Updates any volatile dependencies (those that aren't under some package control system, like our
# own executables) and places them in the ~/.vest-hs directory.

cd $TMPDIR
rm -rf eztz-simple
echo "Cloning eztz-simple CLI."
git clone https://github.com/1protocol/eztz-simple > ~/.vest-hs/eztz-simple-clone.log 2>&1
cd eztz-simple
echo "Installing build deps for eztz-simple."
npm install > ~/.vest-hs/eztz-simple-install.log 2>&1
echo "Building eztz-simple."
npm run-script build > ~/.vest-hs/eztz-simple-build.log 2>&1
cp -f lib/eztz-simple ~/.vest-hs/
chmod +x ~/.vest-hs/eztz-simple
echo "Copied eztz-simple to ~/.vest-hs."
echo "Downloading funnel (logging tool)."
# Eventually: Maybe some magic to build the right funnel executable by platform?
curl -L https://github.com/agnivade/funnel/releases/download/v0.2.1/funnel_darwin-amd64 > ~/.vest-hs/funnel_darwin-amd64 2>~/.vest-hs/funnel-download.log
chmod +x ~/.vest-hs/funnel_darwin-amd64
echo "Copied funnel to ~/.vest-hs."
