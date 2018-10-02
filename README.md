# vest-hs

Mono-repo for all Haskell projects.

## FAQ
- **Where do dependencies go?** Per-project dependencies go in per-project YAML files, but if you
need to add `extra-deps` (non-LTS version of dependencies), those go in the top-level `stack.yaml`.

- **What do I name my sub-module?** Something short like `bridge`. Only add a `vest-` prefix to the
folder and module name if there is a compilation conflict.

- **Do our modules have useful version numbers?** Maybe if we open-source them in the future. For
now, usage of our modules should not be specified with version constraints (in theory, all of our
modules should stay in sync with one another, even if they use different versions of their own
dependencies).

## Updating Types
1. Run `stack exec ts-types bridge-client/foreign/src/generated/types.ts` from the root of this
   repository. If this script errors or (more likely) did not compile in the first place, add
   TypeScript derivations where necessary in the codebase.
2. Run `stack exec ts-callers bridge-client/foreign/templates/callers.ede bridge-client/foreign/src/generated/callers.ts`
   from the root of this repository.
3. You will now have mis-matched version strings in `bridge-client/foreign/src`. Follow the
   instructions in `call.ts` and `generated/callers.ts` to successfully integrate the new types.
