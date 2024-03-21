{
  description = "GovTool and utilities monorepo.";

  inputs.nixpkgs.url =
    "github:nixos/nixpkgs/c9ece0059f42e0ab53ac870104ca4049df41b133";

  outputs = { self, nixpkgs }: {
    packages = let
      supportedSystems =
        [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in builtins.listToAttrs (map (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ ];
          config.allowBroken = true;
        };
      in {
        name = system;
        value = {
          scripts = pkgs.callPackage ./scripts/govtool { inherit pkgs; };
          backend = pkgs.callPackage ./govtool/backend { inherit pkgs; };
        };
      }) supportedSystems);
  };
}
