{ mkDerivation, base, containers, stdenv }:
mkDerivation {
  pname = "haScales";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers ];
  description = "A music theory library";
  license = stdenv.lib.licenses.bsd3;
}
