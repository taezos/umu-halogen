{ nixpkgs ? import ./nix/pinned.nix {} }:
let
  inherit ( nixpkgs ) pkgs;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = import ./project.nix;
  };

in
haskellPackages.umu-halogen
