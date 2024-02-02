# Deployment

## AWS Setup

Requirements:

- aws-cli v2+
- Terraform v1.5.6+
- AWS IAM account CLI keys

### Using NIX configuration

Change directory to `src` and type `nix-shell`. The required tools will be
downloaded and installed in their required versions.

### Bootstrap

AWS is configured with Terraform, but the account needs to be bootstrapped beforehand.

This has to be done only once per AWS account.

1. Configure AWS CLI with `aws configure` (if you are using a configuration profile, use `aws --profile MYPROFILE configure` and then `export AWS_PROFILE=MYPROFILE`).
1. Execute `./src/terraform/bootstrap-aws-account.sh` script.

### Infrastructure setup

1. Change directory to `src/terraform`.
1. Run `terraform init` to initialize Terraform.
1. Run `terraform plan` to view changes that would be performed to infrastructure.
1. Take note of the outputs - they contain ECR repo URLs and app domains.

Note: the Terraform code configures the EC2 instance using `src/terraform/modules/vva-ec2/user_data.sh`. This script is only executed on instance creation.

## Application deployment

1. Set environment variables in your shell (see `src/.env.example` file for list of variables and sample values).
1. Prepare configuration files for target environment:
    1. Run `make prepare-config` - this will create `config/target` directory on your machine with necessary files.
    1. Run `make upload-config instance=$INSTANCE cardano_network=$CARDANO_NETWORK env=$ENVIRONMENT` - this will upload the `config/target` directory to appropriate EC2 instance.
1. Build the application Docker images:
    1. Run `make build-backend`.
    1. Run `make build-frontend instance=$INSTANCE cardano_network=$CARDANO_NETWORK env=$ENVIRONMENT`.
1. Push the images to ECR:
    1. Run `make docker-login`.
    1. Run `make push-backend`.
    1. Run `make push-frontend instance=$INSTANCE cardano_network=$CARDANO_NETWORK env=$ENVIRONMENT`.
1. Deploy the application: run `make deploy-stack instance=$INSTANCE cardano_network=$CARDANO_NETWORK env=$ENVIRONMENT`.

On first deploy, the last command will take a long while. On subsequent deployments it should perform faster.

Alternatively you can type `make all instance=$INSTANCE cardano_network=$CARDANO_NETWORK env=$ENVIRONMENT` to deploy all at once.

View the app at `https://${ENVIRONMENT}-${INSTANCE}.govtool.byron.network`.
Keep in mind that after initial deployment on a new environment, it will take some time for the Cardano node to get in sync.
