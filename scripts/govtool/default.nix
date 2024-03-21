{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  buildInputs = with pkgs; [ awscli curl docker git gnumake openssh rsync ];
  shellHook = ''
    function warn() { tput setaf $2; echo "$1"; tput sgr0; }

    tput bold
    warn "Welcome to GovTool!" 4
    warn "This is a deployment shell." 4
    warn "Read the scripts/govtool/README.md to get more info about the deployment processes." 8
    case "''${ENVIRONMENT}" in
      "dev")
        export DOMAIN="dev-''${CARDANO_NETWORK}.govtool.byron.network"
        warn "Your configuration is set to deploy to DEV environment, you are safe." 2
        ;;
      "test")
        export DOMAIN="test-''${CARDANO_NETWORK}.govtool.byron.network"
        warn "Your configuration is set to deploy to TEST environment, be careful…" 5
        ;;
      "staging")
        export DOMAIN="staging.govtool.byron.network"
        warn "Your configuration is set to deploy to STAGING environment, be careful…" 5
        ;;
      "beta")
        export DOMAIN="sanchogov.tools"
        warn "Your configuration is set to deploy to BETA environment, BE CAREFUL!" 1
        ;;
      *)
        warn "Your configuration is not set, deployment will not work…" 1
        ;;
    esac
  '';
}
