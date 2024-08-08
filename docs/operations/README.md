# Overview

The application is setup with the following tools:

- Terraform - for creating infrastructure in AWS for each environment
- Docker - to build and run application components
- Docker Compose - to connect the application components together and deploy them as a stack
- make - to simplify operations tasks
- Prometheus - to gather metrics from application host and Docker containers
- Grafana - to visualize metrics gathered from Prometheus and handle alerting

# Environments

The application is hosted on AWS, there are several application environments, each of them is described with Terraform in `src/terraform/main.tf` file. Terraform is executed manually, in order to add/modify/delete an environment, modify the code and run `terraform plan` to see the changes and `terraform apply` to execute them.

Each environment consists of:

- VPC network (with subnets, route tables, IGW)
- Security Groups
- EC2 instance
- Elastic IPs associated with EC2 instance
- Route 53 record (only for environments using `govtool.byron.network` domain)

For each environment, the frontend is hosted at root and the backend is at `/api`.

## List of public environments

### beta

A beta environment connected to `preview` Cardano network.

Available at https://preview.gov.tools/. The DNS record for this domain is created manually.

# Deployment

Deployment is performed via GitHub Actions workflow (`.github/workflows/build-and-deploy.yml`).

The workflow performs the following steps:

- check if the environment is defined in Terraform (to avoid deployment attempt to inexistant environment)
- build of frontend app
- build of backend app
- generate configuration files and upload them (over SSH) to the target environment
- setup the application compoments with Docker Compose on the target environment

The workflow can be triggered directly from GitHub Actions panel. When ruuning the workflow, you need to specify:

- Cardano network to be used
- environment name
- optionally skip the build process (frontend and backend) - useful when there are plain configuration changes that do not require the application to be rebuild

# Monitoring

Monitoring is achieved with Prometheus and Grafana, which are deployed together with each environment. Grafana is available at `/grafana`, the `admin` password is defined in a GitHub Actions secret `GRAFANA_ADMIN_PASSWORD`.

Each Grafana instance is managed as code and provisioned with YAML files located at `src/config/grafana-provisioning`. This includes a default datasource, dashboard and alerting. The alerts are configured to be sent to Slack via Slack bot.

The Slack bot OAuth token and recipient are defined with GitHub Actions secrets `GRAFANA_SLACK_OAUTH_TOKEN` and `GRAFANA_SLACK_RECIPIENT`, respectively.

### Frontend

Deploying new versions of the application is done using Github actions

1. Open [`repository`](https://github.com/IntersectMBO/govtool)
2. Select "Actions" tab
3. From left menu choose "Build and deploy app"
4. From the droping options - "Run workflow", select the branch, Cardano network and type of environment for your deployment
5. Press "Run workflow"
6. Wait for the final effect. It's done.
