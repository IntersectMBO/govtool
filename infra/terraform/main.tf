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

module "vva-ecr-be" {
  source    = "./modules/ecr"
  repo_name = "backend"
}

module "vva-ecr-fe" {
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
          module.vva-ecr-be.repo_arn,
          module.vva-ecr-fe.repo_arn
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

module "vva-dev-sanchonet" {
  source          = "./modules/vva-ec2"
  app_env         = "dev"
  cardano_network = "sanchonet"
  instance_type   = "t3.large"
  dns_zone_id     = aws_route53_zone.govtool.id
}

module "vva-test-sanchonet" {
  source          = "./modules/vva-ec2"
  app_env         = "test"
  cardano_network = "sanchonet"
  instance_type   = "t3.large"
  dns_zone_id     = aws_route53_zone.govtool.id
}

module "vva-staging-sanchonet" {
  source           = "./modules/vva-ec2"
  app_env          = "staging"
  cardano_network  = "sanchonet"
  instance_type    = "t3.large"
  dns_zone_id      = aws_route53_zone.govtool.id
  custom_subdomain = "staging"
}

module "vva-beta-sanchonet" {
  source           = "./modules/vva-ec2"
  app_env          = "beta"
  cardano_network  = "sanchonet"
  instance_type    = "t3.large"
}

output "vva-ecr-be-url" {
  value = module.vva-ecr-be.repo_url
}

output "vva-ecr-fe-url" {
  value = module.vva-ecr-fe.repo_url
}

output "vva-dev-sanchonet-frontend-domain" {
  value = module.vva-dev-sanchonet.frontend_domain
}

output "vva-test-sanchonet-frontend-domain" {
  value = module.vva-test-sanchonet.frontend_domain
}

output "vva-staging-sanchonet-frontend-domain" {
  value = module.vva-staging-sanchonet.frontend_domain
}

output "vva-beta-sanchonet-frontend-domain" {
  value = module.vva-beta-sanchonet.frontend_domain
}
