# GovTool Backend

This is a backend application of GovTool project.

## Prerequisites

In order to run `backend` your host machine will need access to the `cardano-db-sync` postgres database. To have this database running locally you'll need:

- `cardano-node`
- `cardano-db-sync`
- PostgreSQL database

You will need your `cardano-node` and `cardano-db-sync` to be compatible with Sancho testnet. Until these features will be merged to the master branch the new Sancho compatible versions are available as releases on [github](https://github.com/IntersectMBO/cardano-db-sync/releases). You will also need a correct `cardano-node` version. The release notes for `cardano-db-sync` usualy specify that.

[`cardano-db-sync` documentation](https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/building-running.md)

[`cardano-db-sync` documentation (with docker)](https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/docker.md)

[`cardano-node` documentation](https://github.com/IntersectMBO/cardano-node/blob/master/README.rst)

[`sancho` testnet config files](https://sancho.network/tutorials/start-node/)

You can utilize the [docker-compose.node+dbsync.yml](../../scripts/govtool/docker-compose.node+dbsync.yml) file to setup the required services.

### Using Nix and Direnv

Due to problems with openapi3 package it's hard to build this project with plain `ghc` and `cabal-install`. Until the prolem is solved we reccomend using `nix` - this problem is fixed when you build your project from inside of the nix shell.

1.  Get [Nix](https://nixos.org/download).

2.  Get [direnv](https://direnv.net/).

3.  Set GHC version to 9.10.1:

    ```sh
        ghcup install ghc 9.10.1

        ghcup set ghc 9.10.1
    ```

4.  Install cabal

        ```sh
            ghcup install cabal
            ghcup set cabal
        ```

5.  Enter `govtool/backend` directory:

    ```sh
    cd govtool/backend
    ```

6.  Allow direnv to setup your environment:

    ```sh
    direnv allow
    ```

7.  Update cabal & build project
    ```sh
    cabal update
    cabal build all
    ```
8.  Create a config file. You can use `example-config.json` as a template.

9.  Run project
    `sh
    cabal run vva-be -- --config <YOUR CONFIG FILE> start-app
    `
    > [!WARNING]
    > In the context of our ongoing project enhancements, it is assumed that the executable previously known as 'vva-be' should be now officially renamed to 'govtool-backend'. This change is necessary for aligning with the updated branding and functional scope of the application and it has to be implemented in the near future as a chore and refactoring ticket. Make sure that the documentation matches the actual name of the executable.

## Development

### Linter

In the development environment in Nix, the [`hlint`](https://github.com/ndmitchell/hlint) tool is readily available to verify Haskell files.

By using `hlint`, developers can ensure that their code adheres to best practices and follows appropriate guidelines. By incorporating `hlint` into the development process, developers can catch potential errors and make necessary improvements early on, ultimately leading to more efficient and robust software development.

### Formatter

To easily format Haskell code, ensuring consistency and readability across the codebase the [`stylish-haskell`](https://github.com/haskell/stylish-haskell) formatter has been introduced into the nix configuration.

Developers can streamline the process of formatting their code, reducing the time and effort required for manual formatting.

### HLS

Developers can use the IDE integrations for Language Server Protocol (LSP) by utilising the [Haskell-Language-Server](https://github.com/haskell/haskell-language-server) that include support for GHC 9.2.7. Using such integration, developers can ensure a seamless and efficient experience with Haskell code in their IDE.
