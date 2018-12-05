#!/bin/bash
# TODO: Split this into local/prod setups (or more likely, use Docker and pre-set things like the
# PATH in the Docker image).
CYAN='\033[0;36m'
NC='\033[0m'

touch ~/.bash_profile
NEW_PATH="export PATH=~/.local/bin:\$PATH"
if grep -Fxq "$NEW_PATH" ~/.bash_profile; then
:
else
echo "Adding Haskell global bin to PATH."
echo "$NEW_PATH" >> ~/.bash_profile
fi

echo -e "${CYAN}Logging commands to ~/.vest-hs.${NC}"
mkdir -p ~/.vest-hs
rm -f ~/.vest-hs/*.log
echo "Cleaning up existing stack work."
stack clean --full > ~/.vest-hs/stack-clean.log 2>&1
echo "Updating Cabal package list."
cabal update > ~/.vest-hs/cabal-update.log 2>&1
echo "Installing Intero."
stack build intero > ~/.vest-hs/intero-build.log 2>&1
echo "Installing IDE-related modules."
stack build hlint > ~/.vest-hs/hlint-build.log 2>&1
stack install phoityne-vscode haskell-dap > ~/.vest-hs/phoityne-install.log 2>&1
stack install hindent > ~/.vest-hs/hindent-install.log 2>&1
echo "Installing external dependencies."
brew install git-crypt node diffutils redis rabbitmq libpq postgres \
  > ~/.vest-hs/brew-install.log 2>&1
echo "Creating initial Postgres cluster."
pg_ctl init -D /usr/local/var/postgres > ~/.vest-hs/postgres-init.log 2>&1
echo "Configuring local Redis to use keyspace events."
sed -i -e 's/notify-keyspace-events ""/notify-keyspace-events "Kg"/g' /usr/local/etc/redis.conf
echo -e "${CYAN}Please follow the remaining instructions in SETUP.${NC}"
