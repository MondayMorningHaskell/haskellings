{ pkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:
pkgs.pkgs.haskell.packages.${compiler}.callPackage ./haskellings.nix { }
