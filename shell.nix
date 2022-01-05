{ nixpkgs ? import (builtins.fetchTarball
"https://github.com/NixOS/nixpkgs/archive/b3f68bbeebee00d6e338e2f0c3a56c60fd2d5de9.tar.gz") {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, array, base, bytestring, bytestring-mmap
      , bzlib, criterion, deepseq, filepath, ghc-prim, hexml, hexpat
      , hspec, lib, mtl, mutable-containers, time, vector, weigh, xml, cabal-install
      }:
      mkDerivation {
        pname = "xeno";
        version = "0.4.2";
        src = ./.;
        enableSeparateDataOutput = true;
        libraryToolDepends = [ cabal-install ];
        libraryHaskellDepends = [
          array base bytestring deepseq mtl mutable-containers vector
        ];
        testHaskellDepends = [ base bytestring hexml hspec ];
        benchmarkHaskellDepends = [
          base bytestring bytestring-mmap bzlib criterion deepseq filepath
          ghc-prim hexml hexpat time weigh xml
        ];
        homepage = "https://github.com/ocramz/xeno";
        description = "A fast event-based XML parser in pure Haskell";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
