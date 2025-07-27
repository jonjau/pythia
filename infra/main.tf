resource "aws_ecr_repository" "repository" {
  name                 = var.ecr_repository_name
  image_tag_mutability = "IMMUTABLE"
  image_scanning_configuration {
    scan_on_push = true
  }

  encryption_configuration {
    encryption_type = "KMS"
  }

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

resource "aws_ecr_lifecycle_policy" "name" {
  repository = aws_ecr_repository.repository.name
  policy = jsonencode({
    "rules" : [
      {
        "rulePriority" : 1,
        "description" : "Keep last 3 latest images",
        "selection" : {
          "tagStatus" : "tagged",
          "tagPrefixList" : ["latest"],
          "countType" : "imageCountMoreThan",
          "countNumber" : 3
        },
        "action" : {
          "type" : "expire"
        }
      },
    ]
  })
}

resource "aws_ecr_registry_scanning_configuration" "default" {
  scan_type = "BASIC"
}

resource "aws_iam_role" "ecs_task_execution" {
  name = "ecs-task-execution-role"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Principal = {
        Service = "ecs-tasks.amazonaws.com"
      }
      Action = "sts:AssumeRole"
    }]
  })
}

resource "aws_iam_role_policy_attachment" "ecs_execution_attach" {
  role       = aws_iam_role.ecs_task_execution.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AmazonECSTaskExecutionRolePolicy"
}

data "aws_iam_policy_document" "task_assume_role_policy" {
  statement {
    actions = ["sts:AssumeRole"]

    principals {
      type        = "Service"
      identifiers = ["ecs-tasks.amazonaws.com"]
    }
  }
}

resource "aws_iam_role" "ecs_task_iam_role" {
  name               = "ecs-task-iam-role-0"
  description        = "Role for ECS tasks to access AWS services"
  assume_role_policy = data.aws_iam_policy_document.task_assume_role_policy.json

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

resource "aws_iam_policy" "dynamodb_access" {
  name        = "DynamoAccess"
  description = "Policy to allow access to DynamoDB for ECS tasks"
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Action = [
        "dynamodb:GetItem",
        "dynamodb:PutItem",
        "dynamodb:UpdateItem",
        "dynamodb:DeleteItem",
        "dynamodb:Scan",
        "dynamodb:Query"
      ]
      Resource = "*"
    }]
  })

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

resource "aws_iam_role_policy_attachment" "ecs_task_dynamodb_access" {
  role       = aws_iam_role.ecs_task_iam_role.name
  policy_arn = aws_iam_policy.dynamodb_access.arn
}

resource "aws_ecs_cluster" "cluster" {
  name = "ecs-cluster-0"

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

## A list of all AZs available in the region configured in the AWS credentials
data "aws_availability_zones" "available" {}

resource "aws_vpc" "vpc" {
  cidr_block = "10.0.0.0/24"

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

resource "aws_internet_gateway" "default" {
  vpc_id = aws_vpc.vpc.id

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

resource "aws_route_table" "private" {
  count  = var.az_count
  vpc_id = aws_vpc.vpc.id

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

resource "aws_subnet" "public" {
  count  = var.az_count
  vpc_id = aws_vpc.vpc.id
  // Get /28 subnet per AZ from the /24 VPC CIDR block
  cidr_block        = cidrsubnet(aws_vpc.vpc.cidr_block, 4, count.index)
  availability_zone = data.aws_availability_zones.available.names[count.index]

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

## Route Table with egress route to the internet
resource "aws_route_table" "public" {
  vpc_id = aws_vpc.vpc.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.default.id
  }

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

## Associate Route Table with Public Subnets
resource "aws_route_table_association" "public" {
  count          = var.az_count
  subnet_id      = aws_subnet.public[count.index].id
  route_table_id = aws_route_table.public.id
}

## Make our Route Table the main Route Table
resource "aws_main_route_table_association" "public_main" {
  vpc_id         = aws_vpc.vpc.id
  route_table_id = aws_route_table.public.id
}

resource "aws_subnet" "private" {
  count  = var.az_count
  vpc_id = aws_vpc.vpc.id
  // Get /28 subnet per AZ from the /24 VPC CIDR block, add var.az_count to the index to avoid overlap with public subnets
  cidr_block        = cidrsubnet(aws_vpc.vpc.cidr_block, 4, var.az_count + count.index)
  availability_zone = data.aws_availability_zones.available.names[count.index]

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

resource "aws_route_table_association" "private" {
  count          = var.az_count
  subnet_id      = aws_subnet.private[count.index].id
  route_table_id = aws_route_table.private[count.index].id
}

resource "aws_cloudwatch_log_group" "log_group" {
  name              = "/ecs/${var.app_container_name}"
  retention_in_days = 5

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

resource "aws_ecs_task_definition" "service" {
  family                   = "ecs-task-definition-0"
  network_mode             = "awsvpc"
  requires_compatibilities = ["FARGATE"]
  cpu                      = 256
  memory                   = 512
  execution_role_arn       = aws_iam_role.ecs_task_execution.arn
  task_role_arn            = aws_iam_role.ecs_task_iam_role.arn
  container_definitions = jsonencode([
    {
      name  = var.app_container_name
      image = var.image_uri

      essential = true
      portMappings = [
        {
          containerPort = var.container_port
        }
      ]

      environment = [
        {
          name  = "AWS_REGION"
          value = "us-west-2"
        },
        {
          name  = "DYNAMODB_ENDPOINT"
          value = "https://dynamodb.us-west-2.amazonaws.com"
        }
      ]

      logConfiguration = {
        logDriver = "awslogs",
        options = {
          "awslogs-group"         = aws_cloudwatch_log_group.log_group.name,
          "awslogs-region"        = "us-west-2",
          "awslogs-stream-prefix" = var.app_container_name
        }
      }
    }
  ])

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

resource "aws_security_group" "ecs_service" {
  name        = "security-group-0"
  description = "Security group for ECS task running on Fargate"
  vpc_id      = aws_vpc.vpc.id

  ingress {
    from_port   = var.container_port
    to_port     = var.container_port
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

resource "aws_ecs_service" "service" {
  name            = "ecs-service-0"
  cluster         = aws_ecs_cluster.cluster.id
  launch_type     = "FARGATE"
  task_definition = aws_ecs_task_definition.service.arn
  desired_count   = 0

  network_configuration {
    # subnets = aws_subnet.private.*.id
    # assign_public_ip = false

    assign_public_ip = true
    subnets          = aws_subnet.public.*.id
    security_groups  = [aws_security_group.ecs_service.id]
  }

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}


# resource "aws_ecs_service" "service" {
#   name                               = "ecs-service-0"
#   iam_role                           = aws_iam_role.ecs_service_role.arn
#   cluster                            = aws_ecs_cluster.default.id
#   task_definition                    = aws_ecs_task_definition.default.arn
#   launch_type = "FARGATE"

#   load_balancer {
#     target_group_arn = aws_alb_target_group.service_target_group.arn
#     container_name   = var.service_name
#     container_port   = var.container_port
#   }

#   ## Spread tasks evenly accross all Availability Zones for High Availability
#   ordered_placement_strategy {
#     type  = "spread"
#     field = "attribute:ecs.availability-zone"
#   }

#   ## Make use of all available space on the Container Instances
#   ordered_placement_strategy {
#     type  = "binpack"
#     field = "memory"
#   }

#   ## Do not update desired count again to avoid a reset to this number on every deployment
#   lifecycle {
#     ignore_changes = [desired_count]
#   }
# }
