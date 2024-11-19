{ pkgs ? import <nixpkgs> {}
, incl
, VITE_BASE_URL ? "http://localhost"
, VITE_IS_DEV ? "true"
, VITE_GTM_ID ? ""
, VITE_SENTRY_DSN ? ""
, VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED ? ""
, VITE_PDF_API_URL ? ""
, CARDANO_NETWORK ? "sanchonet"
}:
let
  VITE_NETWORK_FLAG = if CARDANO_NETWORK == "mainnet" then "1" else "0";
  frontendSrc = incl ./. [
    ./package.json
    ./yarn.lock
    ./src
    ./public
    ./patches
    ./vite.config.ts
    ./index.html
  ];

  nodeModules = pkgs.mkYarnPackage {
    name = "govtool-node-modules";
    src = frontendSrc;
    packageJSON = ./package.json;
    yarnLock = ./yarn.lock;
    nodejs = pkgs.nodejs_18;
  };
  staticSite = pkgs.stdenv.mkDerivation {
    name = "govtool-website";
    src = frontendSrc;
    buildInputs = [(pkgs.yarn.override { nodejs = pkgs.nodejs_18;}) nodeModules];
    inherit VITE_BASE_URL VITE_IS_DEV VITE_GTM_ID VITE_SENTRY_DSN VITE_NETWORK_FLAG VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED VITE_PDF_API_URL;
    buildPhase = ''
      cp -R ${nodeModules}/libexec/@govtool/frontend/node_modules node_modules

      # Yarn links a vite transitive dependency version to
      # `node_modules/.bin` rather than the dev declared vite version which then breaks
      # the build if we don't do this.
      chmod +w node_modules/.bin
      ln -sf ../vite/bin/vite.js node_modules/.bin/vite

      yarn build
    '';
    installPhase = ''
      mv dist $out
    '';
  };
  webserver = { staticSiteRoot ? staticSite, backendUrl ? "http://localhost:9999" }: let
    nginxConfig = pkgs.writeText "govtool-nginx.conf" ''
      daemon off;
      pid /tmp/govtool-nginx.pid;
      events {
      }
      error_log /dev/stdout info;
      http {
        access_log /dev/stdout combined;
        server {
          listen 8081;
          include ${pkgs.nginx}/conf/mime.types;
          root ${staticSiteRoot}/;
          index index.html;
          try_files $uri $uri /index.html;
          location /api/ {
            proxy_pass ${backendUrl}/;
          }
        }
      }
    '';
  in pkgs.writeScriptBin "govtool-webserver" ''
    echo "Starting nginx from site root ${staticSiteRoot}... at http://localhost:8081"
    ${pkgs.nginx}/bin/nginx -c ${nginxConfig} -e /dev/stderr
  '';
  devShell = pkgs.mkShell {
    buildInputs = [pkgs.nodejs_18 pkgs.yarn];
    shellHook = ''
      function warn() { tput setaf $2; echo "$1"; tput sgr0; }

      tput bold
      warn "Welcome to GovTool!" 4
      warn "This is a frontend development shell." 4
      warn "Read the ${./README.md} to get more info about this module." 8
      rm -rf ./node_modules
      ln -s ${nodeModules.out}/libexec/@govtool/frontend/node_modules ./node_modules
    '';
  };
in staticSite // { inherit nodeModules devShell staticSite webserver; }
