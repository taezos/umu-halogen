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
```

### Installation with cabal
First, clone the repository, then run these commands inside the project:
```shell
cabal new-build
cabal new-install exe:umu-halogen # installs to ~/.cabal/bin
```

### Installation with Nix

```shell
{ mkDerivation, fetchzip, stdenv, base, classy-prelude, transformers, mtl
, text, bytestring, optparse-applicative
, ansi-terminal, file-embed, turtle, microlens, casing
}:
mkDerivation rec {
  pname = "umu-halogen";
  version = <insert version here. Found in release tags>;
  src = fetchzip {
    url = "https://github.com/taezos/umu-halogen/archive/v${version}.tar.gz"
    sha256 = <Obtained by running nix-prefetch-url --unpack https://github.com/taezos/umu-halogen/archive/v<version here>.tar.gz in the command line>
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base classy-prelude transformers mtl
    text bytestring optparse-applicative
    ansi-terminal file-embed turtle microlens casing
  ];
  license = stdenv.lib.licenses.asl20;
}
```
Then in your default nix file do an import

```shell
{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;
  inherit (haskellPackages) callPackage;

in
  {
    ...

    umu-halogen = callPackage ./umu-halogen.nix {};

    ...
  }
```

