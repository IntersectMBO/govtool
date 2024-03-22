{
  description = "GovTool and utilities monorepo.";

  inputs.nixpkgs.url =
    "github:nixos/nixpkgs/c9ece0059f42e0ab53ac870104ca4049df41b133";
  inputs.node_nixpkgs.url =
    "github:nixos/nixpkgs/9957cd48326fe8dbd52fdc50dd2502307f188b0d";

  outputs = { self, nixpkgs, node_nixpkgs }: {
    packages = let
      supportedSystems =
        [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in builtins.listToAttrs (map (system:
      let
        pkgs = import nixpkgs { inherit system; config.allowBroken = true; };
        npkgs = import node_nixpkgs { inherit system; };
      in {
        name = system;
        value = {
          scripts = pkgs.callPackage ./scripts/govtool { inherit pkgs; };
          infra = pkgs.callPackage ./infra/terraform { inherit pkgs; };
          backend = pkgs.callPackage ./govtool/backend { inherit pkgs; };
          frontend = pkgs.callPackage ./govtool/frontend { pkgs = npkgs; };
        };
      }) supportedSystems);
  };
}
