{ nixpkgs ? import ../nixpkgs {}, compiler ? "ghc802" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  vindinium = import ../engine { inherit nixpkgs compiler; build=true; };

  distSourceFilter = name: type: let baseName = baseNameOf (toString name); in ! (
    (type == "directory" && (baseName == "dist" || baseName == ".git")) ||
    false
    );

  bot = haskellPackages.callPackage
      ({ mkDerivation, aeson, base, binary, bytestring, containers
      , deepseq, directory, filepath, hashable, heap
      , lens, mtl, optparse-applicative, pqueue, pretty-show
      , process, psqueues, random, stdenv, stm, text, time
      , transformers, unix, cabal-install, haskdogs, hasktags
      , flippers, tasty, tasty-quickcheck, tasty-hunit, QuickCheck
      }:
      mkDerivation {
        pname = "bot-zipzip";
        version = "0.1.0.0";
        src = builtins.filterSource distSourceFilter ./.;
        isLibrary = false;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base binary bytestring containers deepseq directory filepath
          hashable heap lens mtl optparse-applicative pqueue
          pretty-show process psqueues random stm text time transformers unix
          cabal-install vindinium flippers tasty tasty-quickcheck tasty-hunit QuickCheck
        ];
        executableHaskellDepends = [
          aeson base binary bytestring containers lens mtl
          optparse-applicative text unix haskdogs hasktags
        ];
        license = stdenv.lib.licenses.mit;

        doHaddock = false;

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

  if pkgs.lib.inNixShell then bot.env else bot
