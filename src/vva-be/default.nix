{ pkgs ? (import ./sources.nix).pkgs
, ghcVersion ? "ghc927"
}:
let
  additionalTools = drv: pkgs.haskell.lib.addBuildTools drv (with pkgs.haskell.packages."${ghcVersion}";
    [
      cabal-install
      haskell-language-server
      lzma
      ormolu
      pkgs.postgresql
      zlib
    ]);

  project = pkgs.haskell.packages."${ghcVersion}".developPackage {
    root = ./.;
    modifier = additionalTools;
    overrides = self: super: { openapi3 = pkgs.haskell.lib.dontCheck super.openapi3; };
    source-overrides = { resource-pool = "0.4.0.0"; };
  };
in
project
