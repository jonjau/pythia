provider "aws" {
  region = "us-west-2"
}

provider "cloudflare" {
  api_token = var.cloudflare_api_token
}