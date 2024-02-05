provider "aws" {
  region = "eu-west-1"
}

resource "aws_iam_group" "cicd" {
  name = "CICD"
  path = "/"
}

resource "aws_route53_zone" "govtool" {
  name = "govtool.byron.network."
}

module "govtool-ecr-backend" {
  source    = "./modules/ecr"
  repo_name = "backend"
}

module "govtool-ecr-frontend" {
  source    = "./modules/ecr"
  repo_name = "frontend"
}

resource "aws_iam_policy" "cicd_ecr" {
  name = "CICD_ECR"
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Action = [
          "ecr:*"
        ]
        Effect = "Allow"
        Resource = [
          module.govtool-ecr-backend.repo_arn,
          module.govtool-ecr-forntend.repo_arn
        ]
      },
      {
        Action = "ecr:GetAuthorizationToken"
        Effect = "Allow"
        Resource = [
          "*"
        ]
      }
    ]
  })
}

resource "aws_iam_group_policy_attachment" "cicd" {
  group      = aws_iam_group.cicd.name
  policy_arn = aws_iam_policy.cicd_ecr.arn
}

# duplicate the following block in order to prepare a new environment
# make sure that app_env/cardano_network variable pair is unique

module "govtool-dev-sanchonet" {
  source          = "./modules/govtool-ec2"
  app_env         = "dev"
  cardano_network = "sanchonet"
  instance_type   = "t3.large"
  dns_zone_id     = aws_route53_zone.govtool.id
}

module "govtool-test-sanchonet" {
  source          = "./modules/govtool-ec2"
  app_env         = "test"
  cardano_network = "sanchonet"
  instance_type   = "t3.large"
  dns_zone_id     = aws_route53_zone.govtool.id
}

module "govtool-staging-sanchonet" {
  source           = "./modules/govtool-ec2"
  app_env          = "staging"
  cardano_network  = "sanchonet"
  instance_type    = "t3.large"
  dns_zone_id      = aws_route53_zone.govtool.id
  custom_subdomain = "staging"
}

module "govtool-beta-sanchonet" {
  source           = "./modules/govtool-ec2"
  app_env          = "beta"
  cardano_network  = "sanchonet"
  instance_type    = "t3.large"
}

output "govtool-ecr-backend-url" {
  value = module.govtool-ecr-backend.repo_url
}

output "govtool-ecr-frontend-url" {
  value = module.govtool-ecr-frontend.repo_url
}

output "govtool-dev-sanchonet-frontend-domain" {
  value = module.govtool-dev-sanchonet.frontend_domain
}

output "govtool-test-sanchonet-frontend-domain" {
  value = module.govtool-test-sanchonet.frontend_domain
}

output "govtool-staging-sanchonet-frontend-domain" {
  value = module.govtool-staging-sanchonet.frontend_domain
}

output "govtool-beta-sanchonet-frontend-domain" {
  value = module.govtool-beta-sanchonet.frontend_domain
}
