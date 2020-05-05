{ nixpkgs ? import ./nix/pinned.nix {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, ansi-terminal, base, bytestring
      , classy-prelude, file-embed, microlens, mtl
      , optparse-applicative, stdenv, template-haskell, text
      , transformers, turtle, filepath
      }:
      mkDerivation {
        pname = "umu-halogen";
        version = "0.2.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          ansi-terminal base bytestring classy-prelude file-embed microlens
          mtl optparse-applicative template-haskell text
          transformers turtle filepath
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
