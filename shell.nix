{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/34f85de51bbc74595e63b22ee089adbb31f7c7a2.tar.gz") {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    haskell.compiler.ghc884
    stack
  ];

  shellHook = ''
    __GHCPATH=$(echo $(whereis ghc) | sed 's/^....//')
    echo "ghc_path: $__GHCPATH" > config.yaml 

    stack build
    PATH=$PATH:./.stack-work/dist/x86_64-linux-nix/Cabal-3.0.1.0/build/haskellings
  '';
}
