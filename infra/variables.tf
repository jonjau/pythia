variable "aws_region" {
  description = "Which AWS Region to deploy resources to"
  type        = string
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

variable "tags" {
  type        = map(string)
  description = "Tags to apply to resources"
  default     = {}
}
