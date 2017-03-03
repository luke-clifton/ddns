{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, dns, errors, iproute, mtl
      , network, persistent, persistent-sqlite, persistent-template
      , sqlite, stdenv, transformers, utf8-string
      }:
      mkDerivation {
        pname = "ddns";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring dns errors iproute mtl network persistent
          persistent-sqlite persistent-template sqlite transformers
          utf8-string
        ];
        description = "Dynamic DNS server";
        license = stdenv.lib.licenses.bsd2;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
