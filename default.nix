{ mkDerivation, base, cairo, containers, directory, filepath
, haScales, lens, mtl, parsec, stdenv, text, time
}:
mkDerivation {
  pname = "rechord";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base cairo containers directory filepath haScales lens mtl parsec
    text time
  ];
  description = "Generate pretty chord sheets from ChordPro-like text files";
  license = stdenv.lib.licenses.gpl3;
}
