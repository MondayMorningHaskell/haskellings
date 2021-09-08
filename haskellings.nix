{ mkDerivation, aeson, ansi-terminal, base, containers, directory
, extra, filepath, fsnotify, hspec, HUnit, lib, process, tasty, tasty-hunit
, time, yaml
}:
mkDerivation {
  pname = "haskellings";
  version = "0.9.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  doCheck = false;
  libraryHaskellDepends = [
    aeson ansi-terminal base containers directory extra filepath fsnotify
    process tasty tasty-hunit time yaml
  ];
  executableHaskellDepends = [ base containers extra ];
  testHaskellDepends = [
    base containers directory filepath hspec HUnit tasty tasty-hunit time
  ];
  homepage = "https://github.com/MondayMorningHaskell/haskellings#readme";
  license = lib.licenses.bsd3;
}
