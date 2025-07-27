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

variable "az_count" {
  description = "Number of Availability Zones to use in the Region"
  type        = number
  default     = 2
}

variable "container_port" {
  description = "Port on which the container listens"
  type        = number
  default     = 3000
}

variable "image_uri" {
  description = "ECR image URI with digest"
  type        = string
}

variable "app_container_name" {
  description = "Name of the application container"
  type        = string
  default     = "pythia-app"
}