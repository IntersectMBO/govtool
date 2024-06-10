# Terraform Infrastructure for GovTool

This repository contains Terraform configurations for managing cloud resources
for the GovTool project, focusing on AWS.

## Architecture Overview

The infrastructure is designed to support multiple environments and components
across the development lifecycle.

### Components

- **IAM Groups and Policies**: Manage access and permissions for CI/CD
  processes.
- **DNS Management**: Route 53 is used to handle DNS configurations for various
  services.
- **ECR Repositories**: Host Docker images for backend, frontend, and services.
- **EC2 Instances**: Support development, testing, staging, and beta
  environments.

## Managed Resources

This project configures and manages several AWS resources essential for the
operation and deployment of the GovTool application across various environments.

### IAM Configuration

- **IAM Group (`CICD`)**: Manages CI/CD permissions.
- **IAM Policy (`CICD_ECR`)**: Attached to the `CICD` group to manage ECR
  access.

### DNS Management

- **Route 53 Zone (`govtool.byron.network`)**: Handles DNS configurations and
  routing for the application.

### Container Registry

- **ECR Repositories**:
  - `backend`: Stores backend service images.
  - `backend-base`: Stores base images for backend development.
  - `frontend`: Stores frontend service images.
  - `status-service`: Stores images for the status monitoring service.
  - `metadata-validation`: Stores images for metadata validation service.
  - `analytics-dashboard`: Stores images for analytics dashboard.

### Compute Resources

- **EC2 Instances**:
  - `dev`: Instance for development.
  - `test`: Instance for testing.
  - `staging`: Instance for staging.
  - `beta`: Instance for beta testing.

## Configuration Files

- `main.tf`: Defines the provider, resources, and modules for infrastructure
  management.
- `versions.tf`: Specifies the required versions for Terraform and providers,
  ensuring compatibility.

## Managing Infrastructure

### Prerequisites

- AWS CLI
- Configured AWS credentials
- Terraform CLI (version specified in `versions.tf`)

### Initialization

Initialize the Terraform environment to begin managing the infrastructure:

```bash
terraform init
terraform plan
terraform apply
```

### Adding or Modifying Components

When adding new resources or modifying existing configurations, ensure to follow
the modular structure and update the respective environment settings as
required.

## Continuous Integration and Deployment

IAM configurations and ECR policies are tailored to support CI/CD pipelines,
allowing automated build and deployment processes.

## Outputs

Terraform outputs are configured to provide URLs and domain names for the
deployed services, facilitating easy access and management.

## Contributions

Contributions are welcome. Please ensure to maintain the modular architecture
and follow best practices for cloud resource management.
