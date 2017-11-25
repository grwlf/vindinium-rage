{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  vindinium = import ../engine { inherit nixpkgs compiler; build=true; };

  bot = haskellPackages.callPackage
      ({ mkDerivation, aeson, base, binary, bytestring, containers
      , deepseq, directory, filepath, hashable, heap
      , lens
      , MonadRandom, mtl, optparse-applicative, pqueue, pretty-show
      , process, psqueues, random, rdtsc, stdenv, stm, text, time
      , transformers, unix, cabal-install, haskdogs, hasktags
      }:
      mkDerivation {
        pname = "vindinium-bot";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson base binary bytestring containers deepseq directory filepath
          hashable heap lens
          MonadRandom mtl optparse-applicative pqueue
          pretty-show process psqueues random rdtsc stm text time
          transformers unix cabal-install vindinium
        ];
        executableHaskellDepends = [
          aeson base binary bytestring containers lens mtl
          optparse-applicative rdtsc text unix haskdogs hasktags
        ];
        license = stdenv.lib.licenses.mit;

        doHaddock = false;

        shellHook=''
          if test -f /etc/myprofile ; then
            . /etc/myprofile
          fi
        '';
      }) {};

in

  if pkgs.lib.inNixShell then bot.env else bot
