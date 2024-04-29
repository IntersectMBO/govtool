{ pkgs ? import <nixpkgs> {}
, VITE_BASE_URL ? "http://localhost"
, VITE_IS_DEV ? "true"
, VITE_GTM_ID ? ""
, VITE_SENTRY_DSN ? ""
, CARDANO_NETWORK ? "sanchonet"
}:
let
  VITE_NETWORK_FLAG = if CARDANO_NETWORK == "mainnet" then "1" else "0";
  nodeModules = pkgs.mkYarnPackage {
    name = "govtool-node-modules";
    src = ./.;
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
    nodejs = pkgs.nodejs_18;
  };
  staticSite = pkgs.stdenv.mkDerivation {
    name = "govtool-website";
    src = ./.;
    buildInputs = [pkgs.yarn nodeModules];
    inherit VITE_BASE_URL VITE_IS_DEV VITE_GTM_ID VITE_SENTRY_DSN VITE_NETWORK_FLAG;
    buildPhase = ''
      ln -s ${nodeModules}/libexec/voltaire-voting-app/node_modules node_modules
      yarn build
    '';
    installPhase = ''
      mv dist $out
    '';
  };
  devShell = pkgs.mkShell {
    shellHook = ''
      function warn() { tput setaf $2; echo "$1"; tput sgr0; }

      tput bold
      warn "Welcome to GovTool!" 4
      warn "This is a frontend development shell." 4
      warn "Read the ${./README.md} to get more info about this module." 8
      rm -rf ./node_modules
      ln -s ${nodeModules.out}/libexec/voltaire-voting-app/node_modules ./node_modules
    '';
  };
in { inherit nodeModules devShell staticSite; }
