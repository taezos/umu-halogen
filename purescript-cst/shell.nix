{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, base-compat, bytestring
      , containers, dlist, filepath, happy, hpack, purescript-ast
      , scientific, semigroups, stdenv, tasty, tasty-golden
      , tasty-quickcheck, text
      }:
      mkDerivation {
        pname = "purescript-cst";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          array base containers dlist purescript-ast scientific semigroups
          text
        ];
        libraryToolDepends = [ happy hpack ];
        testHaskellDepends = [
          array base base-compat bytestring containers dlist filepath
          purescript-ast scientific semigroups tasty tasty-golden
          tasty-quickcheck text
        ];
        testToolDepends = [ happy ];
        prePatch = "hpack";
        homepage = "http://www.purescript.org/";
        description = "PureScript Programming Language Concrete Syntax Tree";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
