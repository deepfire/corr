{ mkDerivation, stdenv, src ? ./.
, base, base-unicode-symbols, brick, bytestring
, cabal-install
, cassava, configurator-export, containers
, data-default
, exceptions
, free-vl
, ghc-core, gitlib, gitlib-libgit2
, http-client, http-client-openssl
, linear
, newtype
, parsers, pkgconfig, profunctors
, QuickCheck
, singletons, servant-client, split
, time, trifecta, turtle
, vector
, wreq
##########
, youtrack
}:
mkDerivation {
  pname        = "corr";
  version      = "0.0.7";
  src          = ./.;
  isLibrary    = false;
  isExecutable = true;
  buildDepends = [ base base-unicode-symbols brick bytestring
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
		   youtrack
		 ];
  description  = "Correlate JetBrains Youtrack project and a git repository.";
  license      = stdenv.lib.licenses.gpl3;
}
