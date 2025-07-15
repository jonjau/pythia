import {
  to = aws_iam_openid_connect_provider.github
  id = "arn:aws:iam::${var.aws_account_id}:oidc-provider/token.actions.githubusercontent.com"
}

resource "aws_iam_openid_connect_provider" "github" {
  url            = "https://token.actions.githubusercontent.com"
  client_id_list = ["sts.amazonaws.com"]
  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

import {
  to = aws_iam_role.role
  id = "GithubActionsTerraformRole"
}

resource "aws_iam_role" "role" {
  name        = "GithubActionsTerraformRole"
  description = "Role to be assumed by GitHub Actions running in the jonjau/pythia repository to apply Terraform changes"
  assume_role_policy = jsonencode({
    "Version" : "2012-10-17",
    "Statement" : [
      {
        "Effect" : "Allow",
        "Principal" : {
          "Federated" : "arn:aws:iam::${var.aws_account_id}:oidc-provider/token.actions.githubusercontent.com"
        },
        "Action" : "sts:AssumeRoleWithWebIdentity",
        "Condition" : {
          "StringEquals" : {
            "token.actions.githubusercontent.com:aud" : "sts.amazonaws.com"
          },
          "StringLike" : {
            "token.actions.githubusercontent.com:sub" : "repo:jonjau/pythia:*"
          }
        }
      }
    ]
  })

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

import {
  to = aws_iam_policy.policy
  id = "arn:aws:iam::${var.aws_account_id}:policy/GithubActionsTerraformIAMFullAccessPolicy"
}

resource "aws_iam_policy" "policy" {
  name        = "GithubActionsTerraformIAMFullAccessPolicy"
  description = "IAM Full Access to the GithubActionsTerraformRole"
  policy = jsonencode({
    "Version" : "2012-10-17",
    "Statement" : [
      {
        "Sid" : "VisualEditor0",
        "Effect" : "Allow",
        "Action" : "iam:*",
        "Resource" : [
          "arn:aws:iam::${var.aws_account_id}:instance-profile/*",
          "arn:aws:iam::${var.aws_account_id}:sms-mfa/*",
          "arn:aws:iam::${var.aws_account_id}:role/GithubActionsTerraformRole",
          "arn:aws:iam::${var.aws_account_id}:policy/*",
          "arn:aws:iam::${var.aws_account_id}:access-report/*",
          "arn:aws:iam::${var.aws_account_id}:user/*",
          "arn:aws:iam::${var.aws_account_id}:oidc-provider/*",
          "arn:aws:iam::${var.aws_account_id}:server-certificate/*",
          "arn:aws:iam::${var.aws_account_id}:mfa/*",
          "arn:aws:iam::${var.aws_account_id}:saml-provider/*",
          "arn:aws:iam::${var.aws_account_id}:group/*"
        ]
      }
    ]
  })

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

import {
  to = aws_iam_role_policy_attachment.role_policy
  id = "GithubActionsTerraformRole/${aws_iam_policy.policy.arn}"
}

resource "aws_iam_role_policy_attachment" "role_policy" {
  role       = "GithubActionsTerraformRole"
  policy_arn = aws_iam_policy.policy.arn
}
