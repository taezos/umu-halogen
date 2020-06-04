{ mkDerivation, ansi-terminal, base, bytestring, casing, directory
, file-embed, filepath, lens, mtl, optparse-applicative, parsec
, purescript-ast, purescript-cst, relude, stdenv
, template-haskell, text, transformers, turtle
}:
mkDerivation {
  pname = "umu-halogen";
  version = "0.2.0.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring casing directory file-embed filepath
    lens mtl optparse-applicative parsec purescript-ast
    purescript-cst relude template-haskell text transformers turtle
  ];
  executableHaskellDepends = [ base relude ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
