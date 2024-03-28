output "repo_arn" {
  value = aws_ecr_repository.ecr_repo.arn
}

output "repo_url" {
  value = aws_ecr_repository.ecr_repo.repository_url
}
