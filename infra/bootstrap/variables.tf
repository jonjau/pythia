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

variable "aws_account_id" {
  description = "AWS Account ID"
  type        = string
}

variable "tfstate_s3_bucket_prefix" {
  description = "Prefix for Terraform state S3 backend bucket"
  type        = string
}
