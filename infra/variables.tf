variable "ecr_repository_name" {
  description = "Name of the ECR repo"
  type        = string
}

variable "tag_prefix" {
  description = "Prefix for AWS resource tags"
  type        = string
}

variable "environment" {
  description = "Environment name"
  type        = string

  validation {
    condition     = contains(["dev", "staging", "prod"], var.environment)
    error_message = "Environment must be dev, staging, or prod."
  }
}