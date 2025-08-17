resource "aws_iam_role_policy_attachment" "role_policy_ecr" {
  role       = "GithubActionsTerraformRole"
  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2ContainerRegistryFullAccess"
}

resource "aws_iam_role_policy_attachment" "role_policy_s3" {
  role       = "GithubActionsTerraformRole"
  policy_arn = "arn:aws:iam::aws:policy/AmazonS3FullAccess"
}

resource "aws_iam_role_policy_attachment" "role_policy_iam" {
  role       = "GithubActionsTerraformRole"
  policy_arn = "arn:aws:iam::aws:policy/IAMFullAccess"
}

resource "aws_iam_role_policy_attachment" "role_policy_ecs" {
  role       = "GithubActionsTerraformRole"
  policy_arn = "arn:aws:iam::aws:policy/AmazonECS_FullAccess"
}

resource "aws_iam_role_policy_attachment" "role_policy_ec2" {
  role       = "GithubActionsTerraformRole"
  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2FullAccess"
}

resource "aws_iam_role_policy_attachment" "role_policy_cloudwatch" {
  role       = "GithubActionsTerraformRole"
  policy_arn = "arn:aws:iam::aws:policy/CloudWatchLogsFullAccess"
}