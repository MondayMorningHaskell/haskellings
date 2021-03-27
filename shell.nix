{ pkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:

pkgs.mkShell {
  buildInputs = with pkgs; [ 
    haskell.compiler.ghc884
    (import ./default.nix { inherit pkgs compiler; })
  ];
}
