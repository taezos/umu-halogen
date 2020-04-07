{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, ansi-terminal, base, bytestring
      , classy-prelude, file-embed, microlens, microlens-th, mtl
      , optparse-applicative, stdenv, template-haskell, text
      , transformers, turtle
      }:
      mkDerivation {
        pname = "umu-halogen";
        version = "0.1.0.6";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          ansi-terminal base bytestring classy-prelude file-embed microlens
          microlens-th mtl optparse-applicative template-haskell text
          transformers turtle
        ];
        executableHaskellDepends = [ base classy-prelude ];
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
