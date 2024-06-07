# GovTool deployment stack

Welcome to the GovTool deployment stack documentation. This guide provides an
overview of the deployment process, including the setup and management of
various environments, the use of Docker Compose for service orchestration, Nix
for tooling, and detailed steps for deploying the GovTool stack.

This README aims to provide clear and concise instructions for the deployment of
the GovTool stack. Should you have any questions or require further
clarification, please consult the development team.

## Environments

The GovTool project supports four distinct environments, each tailored to
specific stages of the development and deployment lifecycle:

- **dev**: Designed for developers, this environment facilitates development and
  testing of new features.
- **test**: Dedicated to QA testers, enabling thorough testing and validation of
  software before it moves to the next stage.
- **staging**: Acts as a pre-production environment, allowing for final checks
  and adjustments.
- **beta**: Serves as the current production environment, where the software is
  available to end-users.

## Docker Compose files

Docker Compose is utilized to manage per-environment services setups
effectively. This includes configurations for:

- Services such as Cardano Node, Cardano DB Sync, GovTool backend, GovTool
  frontend, the metadata validation service and an analytics dashboard.
- Network configuration with Traefik to handle requests efficiently.
- Monitoring solutions with Prometheus and Grafana to ensure optimal performance
  and availability.

Each environment has its own Docker Compose file, generated from the template,
enabling tailored setups that meet specific requirements.

## Context

Deployment context, including environment variables, is managed through `.env`
files. For an example configuration, refer to the `.env.example` file.

## Deployment process

The deployment process is automated using a system of Makefiles, which include
several targets to streamline the deployment steps:

### [Deployment `Makefile`](./Makefile)

This is the main Makefile that provides full support for the deployment and
maintenance of the stack. It includes backend and frontend modules and uses
particular Makefiles for additional purposes.

#### `prepare-config`

Prepares the configuration files required for the application. This involves
generating or fetching files, then placing them in the appropriate directory
structure. Key components include:

- `cardano-node` Cardano Node configuration files, with modifications for
  Prometheus.
- `dbsync-secrets` for database credentials used by Cardano DB Sync.
- `backend-config.json` for backend configurations.
- `prometheus.yml` and `grafana-provisioning` for monitoring setup.
- `nginx` configuration for basic authentication where necessary.

#### `upload-config`

Uploads the generated configuration to the target server.

#### `deploy-stack`

Updates the target server with the latest images and (re)launches the Docker
Compose stack.

#### `destroy-cardano-node-and-dbsync`

Removes the Cardano Node and Cardano DB Sync services, purges their volumes, and
removes PostgreSQL database files. This step ensures that the Cardano network's
state has been fully removed so the blockchain will be synchronized from the
beginning.

#### `toggle-maintenance`

This step turns on the "maintenance page" that blocks users from usage of the
application and provides an explanation of that in form of the simple page with
error message.

### Helpers, variables and targets in [`common.mk`](./common.mk) file

This Makefile defines some common variables and helpers, as well as highly
reused targets, such as:

#### `check-env-defined`

Verifies the given environment value. When there is no definition for such
environment in Terraform configuration it stops the execution.

#### `docker-login`

Ensures that the user is logged into the correct Docker account, providing
access to necessary Docker resources.

### Notifications in [`info.mk`](./info.mk) file

This file is responsible for giving the proper notifications and feedback about
the current execution:

#### `info`

Displays deployment parameters for review and verification.

#### `notify`

Sends a notification to stakeholders about the deployment status via Slack.

### Building backend images steps in [`backend.mk`](./backend.mk)

This file is responsible for generating images for backend and for pushing them
to the ECR repository.

#### `build-backend`

Builds the backend image locally and a base image for the backend when changes
are detected in the Cabal file.

#### `push-backend`

Pushes the backend and backend-base images to the Docker repository.

### Building frontend images steps in [`frontend.mk`](./frontend.mk)

This file is responsible for generating images for frontend and for pushing them
to the ECR repository.

#### `build-frontend`

Handles the construction of the frontend image.

#### `push-frontend`

Pushes the frontend image to the Docker repository.

### Building status service images steps in [`status-service.mk`](./status-service.mk)

This file is responsible for generating images for status service and for
pushing them to the ECR repository.

#### `build-status-service`

Handles the construction of the status service image.

#### `push-status-service`

Pushes the status service image to the Docker repository.

### Building metadata validation service images steps in [`metadata-validation.mk`](./metadata-validation.mk)

This file is responsible for generating images for metadata validation service
and for pushing them to the ECR repository.

#### `build-metadata-validation`

Handles the construction of the metadata validation service image.

#### `push-metadata-validation`

Pushes the metadata validation service image to the Docker repository.

### Building analytics dashboard service images steps in [`analytics-dashboard.mk`](./analytics-dashboard.mk)

This file is responsible for generating images for analytics dashboard service
and for pushing them to the ECR repository.

#### `build-analytics-dashboard`

Handles the construction of the analytics dashboard service image.

#### `push-analytics-dashboard`

Pushes the analytics dashboard service image to the Docker repository.
