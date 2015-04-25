with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, network, tls, crypto-random, data-default, base
	     , pop3-client, stdenv
             }:
             mkDerivation {
	       pname = "hpop";
	       version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
	       buildDepends = [ network tls crypto-random data-default base pop3-client ];
	       description = "pop3s protocol";
	       license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
