# depend

depend is a lightweight dependency management tool for ethereum smart contract projects.

It is written in less than 1000 lines of go. Static binaries are available for all platforms.

The following are priorities:

- ease of review:
    - the source of all dependencies is available in the same working tree as the parent project
    - the sources can be fetched even if `depend` is not installed
    - the provenance of each dependency can be easily verified
    - projects can be built manually with direct calls to `solc`

- devex:
    - instant startup
    - parralelised builds
    - incremental builds
    - multiple compiler versions
    - compatible with all major test frameworks (`hevm`, `brownie`, `hardhat`)

## Implementation

Dependencies are stored under `dirs.lib` at a filename containing their name and version tag.

TODO: storage of git deps, submodule, subtree, or copy/paste?

Builds are output as a single merged solidity json file, by default the full output is selected.

## Configuration

Configuration is stored in a `dep.yml` file at the project root. The following keys are admitted:

```yaml
dirs:
  src: src
  lib: lib
  out: out
  test: src/test
  plan: plan
```

## Commands

### `depend install <url> [name] [--no-merge]`

`depend install` adds the package at `<url>` to `dirs.lib/name-<version>` in the parent project.

`url` can be:
  - the path to a git repository followed by `@` and a version specifier (git commit or tag).
  - a blockchain address specifier.

If `<url>` is a git repo, then it will be added under `dirs.lib` as a subtree. By default the
contents of `dirs.lib` in `<url>` will be merged with `dirs.lib` in the parent. Any subtrees in
`<url>/<dirs.lib>` will be added as subtrees in the parent. Any conflicts during merge will result
in a hard error.

If `url` is a blockchain address in the format defined
[here](https://ethereum-magicians.org/t/chain-specific-addresses/6449), then `depend` will attempt
to discover and verify the source of the contract at that address. If it is able to do so, then it
will store the source under `dirs.lib/<chain>/<address>`. If it cannot, then it will fail with an
error.

If `url` is an ethpm url, then that package will be fetched and vendored under `dirs.lib`.

### `depend uninstall <path>`

Removes `path` rom `dirs.lib`.

Additionally removes anything under `dirs.lib` that is not directly referenced by an import in a build plan.

### `depend plan`

Builds one compiler input for each file in `dirs.src` and `dirs.test`, and
outputs them into `dirs.plan`.

### `depend build`

Calls `solc` in parallel for each compiler input and merges them into one
output json. Inputs that have not changed since the last call will not be
rebuilt. New inputs are output under `dirs.plan`
