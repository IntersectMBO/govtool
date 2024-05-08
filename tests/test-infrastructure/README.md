GovTool Test Infrastructure
====================

Compose files and scripts to deploy and test environment of govtool.
Additionally, it deploys services required to perform integration test on the environment

## Compose files and services
1. [basic-services](./docker-compose-basic-services.yml) : postgres and gateway
2. [cardano](./docker-compose-cardano.yml) : node, dbsync and kuber
3. [govtool](./docker-compose-govtool.yml) : govtool-frontend and govtool-backend
4. [govaction-loader](./docker-compose-govaction-loader.yml) : govaction-loader frontend and badkcne
5. [test](./docker-compose-test.yml) : lighthouse-server and metadata-api

##  Setting up the services


####  a. Update .env file and DNS records

- Create `.env` file by copying `.env.example` and update it.
- Make sure that  DNS is pointed to the right server. Following are the domains used.
  - lighthouse-{BASE_DOMAIN}
  - kuber-{BASE_DOMAIN}
  - metadata-{BASE_DOMAIN}
  - governance-{BASE_DOMAIN}

### b. Prepare the machine.
  - Buy a virtual server
  - Install `docker` and enable `docker compose` plugin.
  - execute `docker swarm init` command.

### c. One time setup on the machine.
 - Generate secrets and configurations required by the services
  `./gen-configs.sh`
 - Mark the nodes with labels to specify where the services should be run. In case of single node
   docker swarm, all labels can be set to single node.
  `./deploy.sh prepare`

### d. Build images and deploy the stacks.
 - `./build-images.sh`
 - `./deploy.sh stack all`

