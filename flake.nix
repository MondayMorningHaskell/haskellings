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
        package = with pkgs; with pkgs.haskell.lib; with pkgs.haskellPackages; 
          callPackage (callCabal2nix "haskellings" ./.) { };
          # in pkgs.haskell.lib.dontCheck pkg { };
      in {
        defaultPackage = package;
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ ghc package ];
        };
      });
}
