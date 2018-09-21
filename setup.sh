#!/bin/bash
stack build --copy-compiler-tool intero phoityne-vscode hindent hlint
brew install rabbitmq libpq postgres
