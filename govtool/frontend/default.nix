{ pkgs ? import <nixpkgs> {} }:
let
  project = pkgs.mkYarnPackage {
    name = "govtool-frontend";
    src = ./.;
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
  };
in
project
