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
})
