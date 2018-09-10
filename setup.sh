#!/bin/bash
if [ ! -f ~/.vest_haskell ]; then
  stack install phoityne-vscode hindent
  git clone https://github.com/haskell/haskell-ide-engine --recursive
  cd haskell-ide-engine && make build-all
  mv ~/.stack/programs/x86_64-osx/ghc-8.4.3/lib/ghc-8.4.3/integer-gmp-1.0.2.0/HSinteger-gmp-1.0.2.0.o ~/.stack/programs/x86_64-osx/ghc-8.4.3/lib/ghc-8.4.3/integer-gmp-1.0.2.0/HSinteger-gmp-1.0.2.0.o.bak
  touch ~/.vest_haskell
fi

brew install rabbitmq