{ mkDerivation, array, base, base-compat, bytestring, containers
, dlist, filepath, happy, hpack, purescript-ast, scientific
, semigroups, stdenv, tasty, tasty-golden, tasty-quickcheck, text
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
}
