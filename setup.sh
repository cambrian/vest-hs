#!/bin/bash
cabal update
git clone https://github.com/haskell/haskell-ide-engine ~/.haskell-ide-engine --recursive
cd ~/.haskell-ide-engine
stack install
cd -
stack build phoityne-vscode
stack install hindent
brew install rabbitmq libpq postgres
