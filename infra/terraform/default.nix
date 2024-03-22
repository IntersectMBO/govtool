{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  packages = with pkgs; [ awscli2 terraform ];
  shellHook = ''
    function warn() { tput setaf $2; echo "$1"; tput sgr0; }

    tput bold
    warn "Welcome to GovTool!" 4
    warn "This is an infrastructure shell." 4
    warn "Read the infra/terraform/README.md to get more info about the deployment processes." 8
  '';
}
