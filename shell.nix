{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, attoparsec, base, base64-bytestring
      , binary, bytestring, bytestring-builder, case-insensitive, clock
      , connection, containers, criterion, cryptonite, data-default-class
      , entropy, HUnit, network, QuickCheck, random, SHA, stdenv
      , streaming-commons, test-framework, test-framework-hunit
      , test-framework-quickcheck2, text, tls, tls-session-manager
      }:
      mkDerivation {
        pname = "websockets";
        version = "0.12.7.2";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          async attoparsec base base64-bytestring binary bytestring
          bytestring-builder case-insensitive clock connection containers
          cryptonite data-default-class entropy network random SHA
          streaming-commons text tls tls-session-manager
        ];
        testHaskellDepends = [
          async attoparsec base base64-bytestring binary bytestring
          bytestring-builder case-insensitive clock containers entropy HUnit
          network QuickCheck random SHA streaming-commons test-framework
          test-framework-hunit test-framework-quickcheck2 text
        ];
        benchmarkHaskellDepends = [
          async attoparsec base base64-bytestring binary bytestring
          bytestring-builder case-insensitive clock containers criterion
          entropy network random SHA text
        ];
        doCheck = false;
        homepage = "http://jaspervdj.be/websockets";
        description = "A sensible and clean way to write WebSocket-capable servers in Haskell";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
