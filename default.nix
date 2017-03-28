# Install with:
#  nix-env --no-build-output --cores 4 -iE 'f: (import ./. {})'

with import <nixpkgs> {};
let
  default_compiler = "ghc7103";
  youtrack-src-github =
    pkgs.fetchgit {
      url    = https://github.com/deepfire/youtrack;
      rev    = "39eebdc7540183da75f97021a18418e9b67fbb77";
      sha256 = "0lc22d68zgpk38q67gm013pzi3d9n2imp7y2kfvsjcslxigygci9";
    };
  youtrack-src-local = ../youtrack;
  youtrack = hpkgs: src:
             with hpkgs;
             hpkgs.mkDerivation {
               pname = "youtrack";
               version = "0.0.6";
               src = src;
               libraryHaskellDepends = [
                 aeson base base-unicode-symbols bytestring HsOpenSSL http-client
                 http-client-openssl lens mtl parsers QuickCheck safe scientific split text time trifecta
                 unordered-containers utf8-string vector wreq
               ];
               homepage = "https://github.com/deepfire/youtrack";
               description = "Access a Jetbrains YouTrack instance";
               license = stdenv.lib.licenses.gpl3;
             };
in

{ haskellPackages ? pkgs.haskell.packages.${default_compiler}
, youtrack-from-github ? true }:
let
  youtrack-src    = if youtrack-from-github
		    then youtrack-src-github
		    else youtrack-src-local;
  hpkgs           = haskellPackages.override (oldArgs: {
    overrides = with haskell.lib; new: old:
    let parent = (oldArgs.overrides or (_: _: {})) new old;
    in parent // {
 #     brick = old.brick_0_5;
    };
  });
in
with hpkgs;
mkDerivation {
  pname        = "youtrack-tools";
  version      = "0.0.7";
  src          = ./.;
  isLibrary    = false;
  isExecutable = true;
  # haskell-src-meta -> configuration-tools, configifier
  buildDepends = # (if haskellPackages.ghc != pkgs.haskell.packages.ghc801.ghc then [ cabal-install ] else []) ++
                 [ base base-unicode-symbols brick bytestring
		   cabal-install
		   cassava configurator-export containers
		   data-default
		   exceptions
		   free-vl
		   ghc-core gitlib gitlib-libgit2
		   http-client http-client-openssl
		   linear
		   newtype
		   parsers pkgconfig profunctors
		   QuickCheck
		   singletons servant-client split
		   time trifecta turtle
		   vector
		   wreq
		   ###
		   (youtrack hpkgs youtrack-src)
		 ];
  description  = "Correlate JetBrains Youtrack issues and git branches.";
  license      = stdenv.lib.licenses.gpl3;
}
