#!/bin/bash
mkdir -p ~/.vest-hs
echo "Updating Cabal package list."
cabal update > ~/.vest-hs/cabal-update.log 2>&1
echo "Installing Haskell IDE engine."
echo "This might take a while."
git clone https://github.com/haskell/haskell-ide-engine ~/.haskell-ide-engine --recursive > /dev/null 2>&1
cd ~/.haskell-ide-engine > /dev/null 2>&1
stack install > ~/.vest-hs/hie-install.log 2>&1
cd - > /dev/null 2>&1
echo "Installing IDE-related modules."
stack build phoityne-vscode > ~/.vest-hs/phoityne-build.log /dev/null 2>&1
stack install hindent cabal-install > ~/.vest-hs/hindent-install.log /dev/null 2>&1
echo "Installing external dependencies."
brew install rabbitmq libpq postgres > ~/.vest-hs/brew-install.log 2>&1
