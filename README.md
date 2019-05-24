**Note:** This read-only repository comprises a web microservices framework written in Haskell. It
was originally written to support a financial derivatives marketplace for Proof-of-Stake (PoS)
blockchains, although the business logic for the marketplace was left incomplete when the project
had to pivot.

In the hope that portions of the _framework_ might be useful to future projects, it is now open
source. Besides a highly type-safe way of declaring a service interface, this framework has varying
levels of support for DB persistence, caching layers, RPC transport layers, sane config
management, and inter-service authentication.

Unfortunately, it is less documented than we'd like it to be, given the fate of the project. The
best places to start digging are the partially-complete services in the `services` directory, and we
are more than happy to answer questions sent our way!

---

# vest-hs

Monorepo for all Haskell packages.

## FAQ
- **Where do dependencies go?** Per-package dependencies go in per-package YAML files, but if you
need to add `extra-deps` (non-LTS versions of dependencies), those go in the top-level `stack.yaml`.

- **What do I name my packages?** Something short like `tezos`. Only add a `vest-` prefix to the
folder and package name if there is a compilation conflict. _If you are creating a service, it must
go in the services package. This is very much by design._

- **Do our packages have useful version numbers?** Maybe if we open-source them in the future. For
now, usage of our packages should not be specified with version constraints (in theory, all of our
packages should stay in sync with one another, even if they use different versions of their own
dependencies).

## Updating Types
1. Run `stack exec ts-types > typescript/bridge-client/src/generated/types.ts` from the root of this
   repository. If this script errors or (more likely) did not compile in the first place, add
   TypeScript derivations where necessary in the codebase.
2. Run `stack exec ts-callers typescript/bridge-client/templates/callers.ede > typescript/bridge-client/src/generated/callers.ts`
   from the root of this repository.
3. The `bridge-client` should now fail to compile. Follow the instructions in `index.ts` to
   successfully integrate the new types.
