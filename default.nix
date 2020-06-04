let
  nixpkgs = import ./nixpkgs.nix;
in
{ umu-halogen = nixpkgs.umu-halogen; }
