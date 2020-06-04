let
  nixpkgSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/1ae28ebfdb7e7d48f003881967db67c156724912.tar.gz";
    sha256 = "1qqzxaf0nfsg8zdgr5kghih4pr46q5acbnpzihnsdiiykz4xvs3f";
  };

  overlay = self: super: {
    project-pkg-set = self.haskell.packages.ghc883.override {
      overrides = hself: hsuper: {
        umu-halogen = hself.callPackage ./umu-halogen/default.nix {};
        purescript-ast = hself.callPackage ./purescript-ast/default.nix {};
        purescript-cst = hself.callPackage ./purescript-cst/default.nix {};

        project-packages = [
          hself.umu-halogen
          hself.purescript-ast
          hself.purescript-cst
        ];
      };
    };

    umu-halogen = self.buildEnv {
      name = "umu-halogen";
      paths = self.project-pkg-set.project-packages;
      extraOutputsToInstall = [ "dev" "out" ];
    };
  };
in
import nixpkgSrc {
  overlays = [ overlay ];
}
