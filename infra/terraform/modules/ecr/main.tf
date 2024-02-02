resource "aws_ecr_repository" "ecr_repo" {
  name = var.repo_name

  encryption_configuration {
    encryption_type = "KMS"
  }

  tags = {
    app_name = var.app_name
  }
}
