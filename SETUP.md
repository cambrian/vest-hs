# Setup

1. Run `setup.sh` if you have not already.
2. Run `source ~/.bash_profile` to augment your PATH.
3. Run `./dev.sh` to build all the modules for Haskero.
4. Run `./deps.sh` to build any volatile dependencies.
5. Restart the entire VS Code application.

## Database

1. Run `createuser --superuser USERNAME` to create a local Postgres user `USERNAME`.
2. Run `createdb --owner=USERNAME DBNAME` to create a local database `DBNAME`.

## Encryption
By default
