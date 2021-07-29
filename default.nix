{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/7e9b0dff974c89e070da1ad85713ff3c20b0ca97.tar.gz") {}, compiler ? "ghc8104" }:
pkgs.pkgs.haskell.packages.${compiler}.callPackage ./haskellings.nix { }
