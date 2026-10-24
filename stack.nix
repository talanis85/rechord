{ghc}:
with (import <nixpkgs> {});

let
  # pkgs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/refs/tags/22.05.tar.gz) {
  pkgs = import (builtins.fetchTarball {
    name = "nixpkgs-22.05";
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.05.tar.gz";
    sha256 = "0d643wp3l77hv2pmg2fi7vyxn4rwy0iyr8djcw1h5x72315ck9ik";
  }) {
    config = {
      allowBroken = true;
    };
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
      pkgs.haskellPackages.haskell-language-server
    ];
  }
