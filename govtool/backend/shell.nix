{ pkgs ? import <nixpkgs> {} }:
let
  project = import ./default.nix { inherit pkgs; };
in
project.overrideAttrs (attrs: {
  buildInputs = attrs.buildInputs ++ (with pkgs; [
    awscli
    docker
    git
    gnumake
  ]);

  shellHook = ''
    ln -s ${project}/libexec/yarn-nix-example/node_modules node_modules
  '';
})
