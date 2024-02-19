# TODO: Remove the sources file and use the nixpkgs version provided from the
# flakes lock file instead when the flakes feature is present and enabled in the
# root of the project.
{ pkgs ? (import ./sources.nix).pkgs }:
let
  # This is the version of the Haskell compiler we reccommend using.
  ghcPackages = pkgs.haskell.packages.ghc927;

  additionalTools = drv: pkgs.haskell.lib.addBuildTools drv (with ghcPackages;
    [
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
    overrides = self: super: { openapi3 = pkgs.haskell.lib.dontCheck super.openapi3; };
  };
in
project
