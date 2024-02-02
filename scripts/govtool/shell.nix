let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/976fa3369d722e76f37c77493d99829540d43845.tar.gz";
  }) {};
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    awscli
    docker
    git
    gnumake
    rsync
    terraform
  ];
}
