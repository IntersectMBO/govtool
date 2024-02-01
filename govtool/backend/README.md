# GovTool Backend

This is a backend application of GovTool project.

## Prerequisites
In order to run `backend` your host machine will need access to the `cardano-db-sync` postgres database. To have this database running locally you'll need:
* `cardano-node`
* `cardano-db-sync`
* PostgreSQL database

You will need your `cardano-node` and `cardano-db-sync` to be compatible with Sancho testnet. Until these features will be merged to the master branch the new Sancho compatible versions are available as releases on [github](https://github.com/IntersectMBO/cardano-db-sync/releases). You will also need a correct `cardano-node` version. The release notes for `cardano-db-sync` usualy specify that.

[`cardano-db-sync` documentation](https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/building-running.md)

[`cardano-db-sync` documentation (with docker)](https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/docker.md)

[`cardano-node` documentation](https://github.com/IntersectMBO/cardano-node/blob/master/README.rst)

[`sancho` testnet config files](https://sancho.network/tutorials/start-node/)

### Using Nix

Due to problems with openapi3 package it's hard to build this project with plain `ghc` and `cabal-install`. Until the prolem is solved we reccomend using `nix` - this problem is fixed when you build your project from inside of the nix shell.

1. Get [Nix](https://nixos.org/download).

2. Enter `govtool/backend` directory:

    ```sh
    cd govtool/backend
    ```

3. Allow broken nix packages

    ```sh
    export NIXPKGS_ALLOW_BROKEN=1
    ```
    This is due to `openapi3` package being marked as broken.

4. Run `nix-shell`

    ```sh
    nix-shell
    ```

5. Update cabal & build project
    ```sh
    cabal update
    cabal build all
    ```
6. Create a config file. You can use `example-config.json` as a template.

7. Run project
    ```sh
    cabal run vva-be -- --config <YOUR CONFIG FILE> start-app
    ```
