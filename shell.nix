
{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    haskell.compiler.ghc884
    stack
  ];

  shellHook = ''
    stack build --test
    PATH=$PATH:./.stack-work/dist/x86_64-linux-nix/Cabal-3.0.1.0/build/haskellings
  '';
}
