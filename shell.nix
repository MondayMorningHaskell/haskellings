{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/6c4b9f1a2fd761e2d384ef86cff0d208ca27fdca.tar.gz") {}, compiler ? "ghc8107" }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    haskell.compiler.ghc8107
    which
    (import ./default.nix { inherit pkgs compiler; })
  ];

  shellHook = ''
    __GHCPATH=$(echo $(which ghc))
    echo "ghc_path: $__GHCPATH" > config.yaml
  '';
}
