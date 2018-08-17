{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "rechord";
  buildInputs = [ pkgconfig cairo zlib ];
}
