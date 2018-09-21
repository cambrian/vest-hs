#!/bin/bash
stack build --copy-compiler-tool intero phoityne-vscode hlint
stack install hindent
brew install rabbitmq libpq postgres
