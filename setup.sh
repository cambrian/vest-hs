#!/bin/bash
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
stack build phoityne-vscode > ~/.vest-hs/phoityne-build.log 2>&1
stack install hindent > ~/.vest-hs/hindent-install.log 2>&1
echo "Installing testing dependencies."
stack install tasty-discover > ~/.vest-hs/tasty-install.log 2>&1
echo "Installing external dependencies."
brew install diffutils redis rabbitmq libpq postgres > ~/.vest-hs/brew-install.log 2>&1
echo "Configuring Redis to use keyspace events."
sed -i -e 's/notify-keyspace-events ""/notify-keyspace-events "Kg"/g' /usr/local/etc/redis.conf
echo -e "${CYAN}1. Run [source ~/.bash_profile] to apply your PATH."
echo -e "2. Run [createuser --superuser USERNAME] for a local DB user."
echo -e "3. Run [createdb --owner=USERNAME DBNAME] to create a DB."
echo -e "4. Run ./dev.sh to build all the modules for Haskero."
echo -e "5. Restart the entire VS Code application.${NC}"
