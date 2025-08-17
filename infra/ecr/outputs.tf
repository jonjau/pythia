output "repository_url" {
  value       = aws_ecr_repository.repository.repository_url
  description = "The URL of the ECR repository"
}

output "repository_name" {
  value       = aws_ecr_repository.repository.name
  description = "The name of the ECR repository"
}