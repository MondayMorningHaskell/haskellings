{
  description = "haskellings";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, ... }:
  utils.lib.eachDefaultSystem (system:
  let
    inherit (lib) attrValues;
    compiler = "ghc884";
    lib = nixpkgs.lib;
    pkgs = import nixpkgs { inherit system; };
    package = import ./default.nix { inherit pkgs compiler; };
  in {
    defaultPackage = package;
    devShell = import ./shell.nix { inherit pkgs; }; });
  }
