{ nixpkgs ? import ./nix/pinned.nix {} }:
let
  inherit ( nixpkgs ) pkgs;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = import ./project.nix;
  };

  haskellDeps = ps: with ps; [
    haskellPackages.umu-halogen
    haskellPackages.purescript-ast
    haskellPackages.purescript-cst
  ];

  ghc = nixpkgs.haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
    haskellPackages.umu-halogen
    haskellPackages.purescript-ast
    haskellPackages.purescript-cst
    haskellPackages.cabal-install
    haskellPackages.ghcid
  ];

in
pkgs.stdenv.mkDerivation {
  name = "umu-halogen";
  buildInputs = nixPackages;
}
