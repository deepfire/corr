# Install with:
#  nix-env --no-build-output --cores 4 -iE 'f: (import ./. {})'

{ nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc802"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
, localYT     ? false
}:
let
  overcabal = pkgs.haskell.lib.overrideCabal;
  hubsrc    =      repo: rev: sha256:       pkgs.fetchgit { url = "https://github.com/" + repo; rev = rev; sha256 = sha256; };
  overc     = old:                    args: overcabal old (oldAttrs: (oldAttrs // args));
  overhub   = old: repo: rev: sha256: args: overc old ({ src = hubsrc repo rev sha256; }       // args);
  overhage  = old: version:   sha256: args: overc old ({ version = version; sha256 = sha256; } // args);

  youtrack-src-github = pkgs.fetchgit {
    url    = https://github.com/deepfire/youtrack;
    rev    = "f0e43e05b867c82218d386f829af096977c740b0";
    sha256 = "0zajwkxxg41vayga8l5m4f8xw2xwfa8igg2jsk6rlgkrxxkraxdf";
  };
  youtrack-src-local  = ../youtrack;

  ghc       = ghcOrig.override (oldArgs: {
    overrides = with haskell.lib; new: old:
    let parent = (oldArgs.overrides or (_: _: {})) new old;
    in with new; parent // {
      youtrack = new.mkDerivation {
        pname = "youtrack";
        version = "0.0.7";
        src = if !localYT then youtrack-src-github else youtrack-src-local;
        libraryHaskellDepends = [
          aeson base base-unicode-symbols bytestring HsOpenSSL http-client
          http-client-openssl lens mtl parsers QuickCheck safe scientific split text time trifecta
          unordered-containers utf8-string vector wreq
        ];
        homepage = "https://github.com/deepfire/youtrack";
        description = "Access a Jetbrains YouTrack instance";
        license = stdenv.lib.licenses.gpl3;
      };
    };
  });

  ###
  nakeDrv = ghc.callPackage (import ./.) {};
  drv = (haskell.lib.addBuildTools nakeDrv
         [ pkgs.cabal-install
           pkgs.stack
           ghc.intero
         ]);
in if pkgs.lib.inNixShell then drv.env else drv
