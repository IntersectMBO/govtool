terraform {
  backend "s3" {
    bucket         = "govtool-terraform-state"
    key            = "terraform.tfstate"
    region         = "eu-west-1"
    dynamodb_table = "govtool-terraform-locks"
    encrypt        = true
  }
  required_version = ">= 1.5.3"
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}
