{ pkgs ? import <nixpkgs> { } }:
let
  inherit (pkgs.lib.trivial) pipe;
  inherit (pkgs) haskell;
  inherit (haskell) lib;

  # This is the version of the Haskell compiler we recommend using.
  ghcPackages = haskell.packages.ghc927;

  appendLibraries = drv: lib.addExtraLibraries drv (with pkgs; [ lzma zlib ]);

  appendTools = drv:
    lib.addBuildTools drv (with ghcPackages; [
      cabal-install
      haskell-language-server
      hlint
      stylish-haskell
    ]);

  useBroken = drv: pipe drv [ lib.markBroken lib.dontCheck ];

  modifier = drv: pipe drv [ appendLibraries appendTools ];

  project = ghcPackages.developPackage {
    root = ./.;
    modifier = modifier;
    overrides = self: super: { openapi3 = useBroken super.openapi3; };
  };
in project.overrideAttrs (oldAttrs: {
  shellHook = ''
    function warn() { tput setaf $2; echo "$1"; tput sgr0; }

    tput bold
    warn "Welcome to GovTool!" 4
    warn "This is a backend development shell." 4
    warn "Read the ${./README.md} to get more info about this module." 8
  '' + (oldAttrs.shellHook or "");
})
