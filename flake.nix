{
  description = "GovTool and utilities monorepo.";

  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/release-23.11";
  inputs.nixpkgs-vva-be.url = "github:nixos/nixpkgs/c9ece0059f42e0ab53ac870104ca4049df41b133";

  outputs = inputs: let
    vvaBePkgs = system:
      import inputs.nixpkgs-vva-be {
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
        packages.vva-be = pkgs.callPackage ./src/vva-be {pkgs = vvaBePkgs system;};
      };
    };

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = true;
  };
}
