# vest-hs

Monorepo for all Haskell projects.

## FAQ
- **Where do dependencies go?** Per-project dependencies go in per-project YAML files, but if you
need to add `extra-deps` (non-LTS version of dependencies), those go in the top-level `stack.yaml`.

- **What do I name my sub-module?** Something short like `tezos`. Only add a `vest-` prefix to the
folder and module name if there is a compilation conflict. _If you are creating a service, it must
go in the services package. This is very much by design._

- **Do our modules have useful version numbers?** Maybe if we open-source them in the future. For
now, usage of our modules should not be specified with version constraints (in theory, all of our
modules should stay in sync with one another, even if they use different versions of their own
dependencies).

## Updating Types
1. Run `stack exec ts-types > typescript/bridge-client/src/generated/types.ts` from the root of this
   repository. If this script errors or (more likely) did not compile in the first place, add
   TypeScript derivations where necessary in the codebase.
2. Run `stack exec ts-callers typescript/bridge-client/templates/callers.ede > typescript/bridge-client/src/generated/callers.ts`
   from the root of this repository.
3. The `bridge-client` should now fail to compile. Follow the instructions in `index.ts` to
   successfully integrate the new types.
