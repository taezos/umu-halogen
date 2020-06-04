# umu-halogen

```shell
umu-halogen: Generate Halogen Project

Usage: umu-halogen COMMAND
  Use umu-halogen to generate a scaffold for a halogen project

Available options:
  -h,--help                Show this help text
  -v,--version             Show version

Available commands:
  init                     Initialize scaffold
  component                Generate a component
  route                    Generate a route
```

### Installation with cabal
First, clone the repository:
``` shell
git repo https://github.com/taezos/umu-halogen.git
```
Then run these commands inside the project:
```shell
cabal new-build
cabal new-install exe:umu-halogen # installs to ~/.cabal/bin
```

### Installation with Nix
First, clone the repository:
``` shell
git repo https://github.com/taezos/umu-halogen.git
```
Then run these commands inside the project:
``` shell
nix-env -f default.nix -i
```
