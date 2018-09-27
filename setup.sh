#!/bin/bash
mkdir -p ~/.vest-hs
echo "Updating Cabal package list."
cabal update > ~/.vest-hs/cabal-update.log
# echo "Installing Haskell IDE engine."
# echo "This might take a while."
# git clone https://github.com/haskell/haskell-ide-engine ~/.haskell-ide-engine --recursive > /dev/null
# cd ~/.haskell-ide-engine > /dev/null
# stack install > ~/.vest-hs/hie-install.log
# cd - > /dev/null
# echo "Performing HIE troubleshooting fix."
# mv ~/.stack/programs/x86_64-osx/ghc-8.4.3/lib/ghc-8.4.3/integer-gmp-1.0.2.0/HSinteger-gmp-1.0.2.0.o ~/.stack/programs/x86_64-osx/ghc-8.4.3/lib/ghc-8.4.3/integer-gmp-1.0.2.0/HSinteger-gmp-1.0.2.0.o.bak > /dev/null
echo "Installing Intero."
stack build intero > ~/.vest-hs/intero-build.log
echo "Installing IDE-related modules."
stack build phoityne-vscode > ~/.vest-hs/phoityne-build.log /dev/null
stack install hindent cabal-install > ~/.vest-hs/hindent-install.log /dev/null
echo "Installing external dependencies."
brew install rabbitmq libpq postgres > ~/.vest-hs/brew-install.log
