{
  description = "GovTool and utilities monorepo.";

  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/release-23.11";
  inputs.nixpkgs-govtool-backend.url = "github:nixos/nixpkgs/c9ece0059f42e0ab53ac870104ca4049df41b133";

  outputs = inputs: let
    govtoolBackendPkgs = system:
      import inputs.nixpkgs-govtool-backend {
        inherit system;
        config.allowBroken = true;
      };
  in
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      perSystem = {
        config,
        pkgs,
        system,
        ...
      }: {
        packages.govtool-backend = pkgs.callPackage govtool/backend/default.nix {pkgs = govtoolBackendPkgs system;};
      };
    };

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = true;
  };
}
