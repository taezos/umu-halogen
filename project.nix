self: super:
{
  umu-halogen = super.callPackage ./umu-halogen/default.nix {};
  purescript-ast = super.callPackage ./purescript-ast/default.nix {};
  purescript-cst = super.callPackage ./purescript-cst/default.nix {};

  project-packages = [
    self.umu-halogen
    self.purescript-ast
    self.purescript-cst
  ];
}
