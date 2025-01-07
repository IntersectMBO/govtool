{
  description = "GovTool and utilities monorepo.";
  inputs = {
    default_nixpkgs.url = "github:nixos/nixpkgs/c9ece0059f42e0ab53ac870104ca4049df41b133";
    node_nixpkgs.url = "github:nixos/nixpkgs/23aa9e2f958310c69a94d21f6523240841a02d58";
    flake-utils.url = "github:numtide/flake-utils";
    nix-inclusive.url = "github:input-output-hk/nix-inclusive";
  };

  outputs = { self, default_nixpkgs, node_nixpkgs, flake-utils, nix-inclusive, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        defaultPkgs = import default_nixpkgs { inherit system; config.allowBroken = true; };
        nodePkgs = import node_nixpkgs { inherit system; config.allowUnfree = true; };
        frontend = nodePkgs.callPackage ./govtool/frontend { pkgs = nodePkgs; incl = nix-inclusive.lib.inclusive; };
      in
      {
        packages.scripts = defaultPkgs.callPackage ./scripts/govtool { pkgs = nodePkgs; };
        packages.infra = defaultPkgs.callPackage ./infra/terraform { pkgs = nodePkgs; };
        packages.backend = defaultPkgs.callPackage ./govtool/backend { pkgs = defaultPkgs; incl = nix-inclusive.lib.inclusive; };
        packages.frontend = frontend;
        packages.webserver = defaultPkgs.callPackage frontend.webserver {
          staticSiteRoot = frontend.staticSite.overrideAttrs (finalAttrs: prevAttrs: {
            VITE_BASE_URL = "/api";
          });
        };

        # Example of how to change VITE variables
        #packages.frontendOverride = frontend.overrideAttrs (finalAttrs: prevAttrs: {
        #  VITE_BASE_URL = "https://example.com:8443";
        #});

        devShells = {
          default = defaultPkgs.mkShell { buildInputs = [ defaultPkgs.pre-commit ]; };
          frontend = frontend.devShell;
          # shell with js dependencies only if yarn.lock is broken and needs fixed
          js = defaultPkgs.mkShell {
            buildInputs = [ nodePkgs.nodejs_20 nodePkgs.yarn ];
          };
        };
      });
}
