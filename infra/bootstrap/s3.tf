resource "aws_s3_bucket" "tfstate_s3_bucket" {
  bucket_prefix = var.tfstate_s3_bucket_prefix

  lifecycle {
    prevent_destroy = true
  }

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

resource "aws_s3_bucket_versioning" "tfstate_s3_bucket_versioning" {
  bucket = aws_s3_bucket.tfstate_s3_bucket.id
  versioning_configuration {
    status = "Enabled"
  }
}
