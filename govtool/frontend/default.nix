{ pkgs ? import <nixpkgs> {} }:
let
  project = pkgs.mkYarnPackage {
    name = "govtool-frontend";
    src = ./.;
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
    nodejs = pkgs.nodejs_18;
  };
in
project.overrideAttrs (attrs: {
  shellHook = ''
    function warn() { tput setaf $2; echo "$1"; tput sgr0; }

    tput bold
    warn "Welcome to GovTool!" 4
    warn "This is a frontend development shell." 4
    warn "Read the ${./README.md} to get more info about this module." 8
    rm -rf ./node_modules
    ln -s ${project.out}/libexec/voltaire-voting-app/node_modules ./node_modules
  '';
})
