# Run GovTool Locally

If you are interested in contributing or exploring GovTool in depth, you can run the GovTool services on your own machine.

## Docker Compose (recommended)

The quickest way to run Core GovTool locally is the Docker Compose setup in the GovTool repository: [docker/README.md](https://github.com/IntersectMBO/govtool/blob/develop/docker/README.md). It starts:

* the frontend on port `80`
* the backend on port `9999`
* the metadata validation service on port `3000`
* the outcomes pillar backend on port `3001`

You will need Docker with Docker Compose, and access to a DB-Sync PostgreSQL instance (see [Core GovTool](./core-govtool.md) for the DB-Sync prerequisites).

## Kubernetes (Minikube)

You can also run GovTool in a lightweight Minikube cluster, running each of the associated services on localhost. A community example is available in [aaboyle878/govtool-k8-manifest](https://github.com/aaboyle878/govtool-k8-manifest) (last updated August 2025). The rest of this section describes that setup.

The Helm charts and Argo CD configuration used for the hosted GovTool deployments are in [IntersectMBO/govtool-argo](https://github.com/IntersectMBO/govtool-argo).
