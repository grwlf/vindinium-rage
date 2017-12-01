{ nixpkgs ? import ../nixpkgs {}, compiler ? "ghc802", build ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  "mersenne-random-pure64" =
    { mkDerivation, base, random, time }:
     mkDerivation {
       pname = "mersenne-random-pure64";
       version = "0.2.2.0";
       sha256 = "ef1400ddc1ddafb0b98d4331a58bdbe3b5209a81579e17f85f180731a96d75d1";
       libraryHaskellDepends = [ base random time ];
       homepage = "http://code.haskell.org/~dons/code/mersenne-random-pure64/";
       description = "Generate high quality pseudorandom numbers purely using a Mersenne Twister";
       doCheck = false;
     };

  "heredocs" = haskellPackages.callPackage
     ({ mkDerivation, base, bytestring, doctest, parsec
     , template-haskell, text, stdenv
     }:
     mkDerivation {
       pname = "heredocs";
       version = "0.1.4";
       sha256 = "3f879b0e2f34d98f670e6a210f1bc61d9c4a9505c147c7ec93576f54fe69c56f";
       libraryHaskellDepends = [
         base bytestring doctest parsec template-haskell text
       ];
       testHaskellDepends = [ base bytestring doctest text ];
       description = "heredocument";
       license = stdenv.lib.licenses.bsd3;
       doCheck = false;
     }) {};

  "http-client" = haskellPackages.callPackage
    ({ mkDerivation, array, async, base, base64-bytestring
     , blaze-builder, bytestring, case-insensitive, containers, cookie
     , deepseq, directory, exceptions, filepath, ghc-prim, hspec
     , http-types, mime-types, monad-control, network, network-uri
     , random, streaming-commons, text, time, transformers, zlib
     , stdenv
     }:
     mkDerivation {
       pname = "http-client";
       version = "0.5.6.1";
       sha256 = "2c304337b88ea48cf4b1c4e4ac12ec48c5f3a241f3ab44a57965c1d9d06a2bed";
       libraryHaskellDepends = [
         array base base64-bytestring blaze-builder bytestring
         case-insensitive containers cookie deepseq exceptions filepath
         ghc-prim http-types mime-types network network-uri random
         streaming-commons text time transformers
       ];
       testHaskellDepends = [
         async base base64-bytestring blaze-builder bytestring
         case-insensitive containers deepseq directory hspec http-types
         monad-control network network-uri streaming-commons text time
         transformers zlib
       ];
       doCheck = false;
       homepage = "https://github.com/snoyberg/http-client";
       description = "An HTTP client engine";
       license = stdenv.lib.licenses.mit;
     }) {};

  "thrift" = haskellPackages.callPackage
    ({ mkDerivation, attoparsec, base, base64-bytestring, binary
     , bytestring, containers, ghc-prim, hashable, hspec, HTTP, network
     , network-uri, QuickCheck, split, text, unordered-containers
     , stdenv, vector
     }:
     mkDerivation {
       pname = "thrift";
       version = "0.10.0";
       sha256 = "01vxik64gnsnm0y9mh82dv48f711231dhc4kksdmgs7f352fc1k7";
       libraryHaskellDepends = [
         attoparsec base base64-bytestring binary bytestring containers
         ghc-prim hashable HTTP network network-uri QuickCheck split text
         unordered-containers vector
       ];
       testHaskellDepends = [
         base bytestring hspec QuickCheck unordered-containers
       ];
       homepage = "http://thrift.apache.org";
       description = "Haskell bindings for the Apache Thrift RPC system";
       license = "unknown";
       hydraPlatforms = stdenv.lib.platforms.none;
       patchPhase = ''
        sed -i 's/0.10.12.2/0.12.0.1/g' thrift.cabal
       '';
       doCheck = false;
     }) {};

  distSourceFilter = name: type: let baseName = baseNameOf (toString name); in ! (
    (type == "directory" && (baseName == "dist" || baseName == ".git")) ||
    false
    );

  "tensorflow-proto" = haskellPackages.callPackage
    ({ stdenv, mkDerivation, base, Cabal, proto-lens, proto-lens-protoc, protobufc, protobuf }:
     mkDerivation {
       pname = "tensorflow-proto";
       version = "0.1.0.0";
       sha256 = "06k87dvpsf8pnbb1qq5gjxpjc2sra95y1bwmsnpmlg1qn0ppi5mn";
       setupHaskellDepends = [ base Cabal proto-lens-protoc protobufc protobuf ];
       libraryHaskellDepends = [ base proto-lens proto-lens-protoc protobufc protobuf ];
       buildDepends = [protobufc protobuf];
       homepage = "https://github.com/tensorflow/haskell#readme";
       description = "TensorFlow protocol buffers";
       license = stdenv.lib.licenses.asl20;
     }) {};

  vindinium = haskellPackages.callPackage
      ({ mkDerivation, aeson, base, binary, bytestring, containers
      , deepseq, directory, filepath, hashable, heap
      , http-types, lens, mtl, optparse-applicative, pqueue, pretty-show
      , process, psqueues, random, stdenv, stm, text, time
      , transformers, unix, cabal-install, haskdogs, hasktags
      , flippers, tasty, tasty-quickcheck, tasty-hunit, QuickCheck
      }:
      mkDerivation {
        pname = "vindinium-bot";
        version = "0.1.0.0";
        src = builtins.filterSource distSourceFilter ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base binary bytestring containers deepseq directory filepath
          hashable heap http-client http-types lens
          mtl optparse-applicative pqueue
          pretty-show process psqueues random stm text time
          transformers unix cabal-install flippers
          tasty tasty-quickcheck tasty-hunit QuickCheck
          thrift
        ];
        executableHaskellDepends = [
          aeson base binary bytestring containers lens mtl
          optparse-applicative text unix haskdogs hasktags
          thrift
        ];
        license = stdenv.lib.licenses.mit;

        shellHook=''
          if test -f /etc/myprofile ; then
            . /etc/myprofile
          fi

          if test -f profile.sh ; then
            . profile.sh
          fi
        '';
      }) {};

in

  if pkgs.lib.inNixShell && !build then vindinium.env else vindinium
