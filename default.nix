{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/34f85de51bbc74595e63b22ee089adbb31f7c7a2.tar.gz") {}, compiler ? "ghc884" }:
pkgs.pkgs.haskell.packages.${compiler}.callPackage ./haskellings.nix { }
