{ mkDerivation, aeson, base, base-unicode-symbols, brick
, bytestring, cassava, containers, data-default, exceptions, foldl
, free-vl, gitlib, gitlib-libgit2, hashable, lens, monad-control
, mtl, optparse-applicative, QuickCheck, scientific, split, stdenv
, tagged, text, time, turtle, unordered-containers, utf8-string
, vector, vty, wreq, youtrack
}:
mkDerivation {
  pname = "corr";
  version = "0.0.7";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base-unicode-symbols brick bytestring cassava containers
    data-default exceptions foldl free-vl gitlib gitlib-libgit2
    hashable lens monad-control mtl optparse-applicative QuickCheck
    scientific split tagged text time turtle unordered-containers
    utf8-string vector vty wreq youtrack
  ];
  description = "Correlate JetBrains Youtrack issues and git branches";
  license = stdenv.lib.licenses.gpl3;
}
