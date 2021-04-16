{ mkDerivation, aeson, ansi-terminal, base, containers, directory
, extra, fsnotify, hspec, HUnit, lib, process, tasty, tasty-hunit
, time, yaml
}:
mkDerivation {
  pname = "haskellings";
  version = "0.8.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  doCheck = false;
  libraryHaskellDepends = [
    aeson ansi-terminal base containers directory extra fsnotify
    process tasty tasty-hunit time yaml
  ];
  executableHaskellDepends = [ base containers extra ];
  testHaskellDepends = [
    base containers directory hspec HUnit tasty tasty-hunit time
  ];
  homepage = "https://github.com/MondayMorningHaskell/haskellings#readme";
  license = lib.licenses.bsd3;
}
