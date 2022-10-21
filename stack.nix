{ghc}:
with (import <nixpkgs> {});

let
  nixpkgs1809 = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/18.09.tar.gz) {
    config = config // { allowBroken = true; };
  };

in
  nixpkgs1809.haskell.lib.buildStackProject {
    ghc = nixpkgs1809.haskell.compiler.ghc802;
    name = "rechord";
    buildInputs = [
      nixpkgs1809.pkgconfig
      nixpkgs1809.cairo
      nixpkgs1809.gnome3.gtk
      nixpkgs1809.pango
      nixpkgs1809.git
      nixpkgs1809.zlib
    ];
  }
