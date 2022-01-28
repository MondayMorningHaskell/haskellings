{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/6c4b9f1a2fd761e2d384ef86cff0d208ca27fdca.tar.gz") {}, compiler ? "ghc8107" }:
pkgs.pkgs.haskell.packages.${compiler}.callPackage ./haskellings.nix { }
