{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/7e9b0dff974c89e070da1ad85713ff3c20b0ca97.tar.gz") {}, compiler ? "ghc8104" }:

pkgs.mkShell {
  buildInputs = with pkgs; [ 
    haskell.compiler.ghc8104
    (import ./default.nix { inherit pkgs compiler; })
  ];

  shellHook = ''
    __GHCPATH=$(echo $(whereis ghc) | sed 's/^....//')
    echo "ghc_path: $__GHCPATH" > config.yaml 
  '';
}
