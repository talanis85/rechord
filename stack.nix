{ghc}:
with (import <nixpkgs> {});

let
  pkgs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/22.05.tar.gz) {
    config = config // { allowBroken = true; };
  };

in
  pkgs.haskell.lib.buildStackProject {
    ghc = pkgs.haskell.compiler.ghc884;
    name = "rechord";
    nativeBuildInputs = [ lilypond ];
    buildInputs = [
      pkgs.pkgconfig
      pkgs.cairo
      pkgs.gtk3
      pkgs.pango
      pkgs.git
      pkgs.zlib
      pkgs.xorg.xorgproto
      pkgs.xorg.libX11
    ];
  }
