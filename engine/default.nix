{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801", build ? false }:

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

  distSourceFilter = name: type: let baseName = baseNameOf (toString name); in ! (
    # Filter out Subversion and CVS directories.
    (type == "directory" && (baseName == "dist" || baseName == ".git")) ||
    false
    );

  vindinium = haskellPackages.callPackage
      ({ mkDerivation, aeson, base, binary, bytestring, containers
      , deepseq, Diff, directory, filepath, hashable, heap
      , http-types, lens, mersenne-random-pure64
      , MonadRandom, mtl, optparse-applicative, pqueue, pretty-show
      , process, psqueues, random, rdtsc, stdenv, stm, text, time
      , transformers, unix, cabal-install, haskdogs, hasktags
      , gitrev
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
          mersenne-random-pure64 MonadRandom mtl optparse-applicative pqueue
          pretty-show process psqueues random rdtsc stm text time
          transformers unix cabal-install heredocs gitrev
        ];
        executableHaskellDepends = [
          aeson base binary bytestring containers Diff lens mtl
          optparse-applicative rdtsc text unix haskdogs hasktags
        ];
        license = stdenv.lib.licenses.mit;

        shellHook=''
          if test -f /etc/myprofile ; then
            . /etc/myprofile
          fi
        '';
      }) {};

in

  if pkgs.lib.inNixShell && !build then vindinium.env else vindinium
