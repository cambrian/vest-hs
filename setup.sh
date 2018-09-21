#!/bin/bash
if [ ! -f ~/.vest_haskell ]; then
  stack build --copy-compiler-tool intero phoityne-vscode hindent hlint
  brew install rabbitmq libpq postgres
  touch ~/.vest_haskell
fi
