{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, crypto-random
      , data-default, directory, filepath, network, old-locale, stdenv
      , time, tls, transformers, x509, x509-store
      }:
      mkDerivation {
        pname = "pop3s-client";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [
          base bytestring containers crypto-random data-default directory
          filepath network old-locale time tls transformers x509 x509-store
        ];
        description = "pop3s protocol";
        license = stdenv.lib.licenses.mit;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
