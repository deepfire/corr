{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc801"
, youtrack-from-github ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./.; # your default.nix

  haskell             = pkgs.haskell;
  haskellPackagesOrig = haskell.packages.${compiler};

  haskellPackages     = # import ../nixpkgs-haskellpackages-overrides/ghc801.nix pkgs
                        haskellPackagesOrig;

  drv = haskellPackages.callPackage f {
    haskellPackages = haskellPackages;
    youtrack-from-github = youtrack-from-github;
  };

in

  if pkgs.lib.inNixShell then drv.env else drv
