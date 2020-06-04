{ mkDerivation, aeson, base, base-compat, bytestring, containers
, deepseq, filepath, hpack, microlens, mtl, protolude, scientific
, serialise, stdenv, text, vector
}:
mkDerivation {
  pname = "purescript-ast";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base-compat bytestring containers deepseq filepath
    microlens mtl protolude scientific serialise text vector
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "http://www.purescript.org/";
  description = "PureScript Programming Language Abstract Syntax Tree";
  license = stdenv.lib.licenses.bsd3;
}
