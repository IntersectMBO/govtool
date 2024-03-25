{ pkgs ? import <nixpkgs> { } }:
let
  # This is the version of the Haskell compiler we reccommend using.
  ghcPackages = pkgs.haskell.packages.ghc927;

  additionalTools = drv:
    pkgs.haskell.lib.addBuildTools drv (with ghcPackages; [
      cabal-install
      haskell-language-server
      lzma
      ormolu
      pkgs.postgresql
      zlib
    ]);

  project = ghcPackages.developPackage {
    root = ./.;
    modifier = additionalTools;
    overrides = self: super: {
      openapi3 = pkgs.haskell.lib.dontCheck super.openapi3;
    };
  };
in project.overrideAttrs (oldAttrs: {
  shellHook = ''
    function warn() { tput setaf $2; echo "$1"; tput sgr0; }

    tput bold
    warn "Welcome to GovTool!" 4
    warn "This is a backend development shell." 4
    warn "Read the govtool/backend/README.md to get more info about this module." 8
  '' + (oldAttrs.shellHook or "");
})
