{ mkDerivation, base, cairo, containers, filepath, gitrev, haScales
, lens, mtl, optparse-applicative, parsec, stdenv, text
}:
mkDerivation {
  pname = "rechord";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base cairo containers filepath gitrev haScales lens mtl
    optparse-applicative parsec text
  ];
  description = "Generate pretty chord sheets from ChordPro-like text files";
  license = stdenv.lib.licenses.gpl3;
}
