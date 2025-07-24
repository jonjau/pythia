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
  name = "ecsTaskExecutionRole"

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


# resource "aws_iam_role_policy" "dynamodb_access" {
#   name = "DynamoAccess"
#   role = aws_iam_role.ecs_task_execution.id

#   policy = jsonencode({
#     Version = "2012-10-17"
#     Statement = [{
#       Effect = "Allow"
#       Action = [
#         "dynamodb:GetItem",
#         "dynamodb:PutItem",
#         "dynamodb:UpdateItem",
#         "dynamodb:DeleteItem",
#         "dynamodb:Scan",
#         "dynamodb:Query"
#       ]
#       Resource = "*"
#     }]
#   })
# }


resource "aws_ecs_cluster" "cluster" {
  name = "ecs-cluster0"

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}

# ECS Task Definition
# resource "aws_ecs_task_definition" "app" {
#   family                   = "simple-task"
#   cpu                      = "256"
#   memory                   = "512"
#   network_mode             = "awsvpc"
#   requires_compatibilities = ["FARGATE"]
#   execution_role_arn       = aws_iam_role.task_exec_role.arn

#   container_definitions = jsonencode([
#     {
#       name      = "web"
#       image     = "123456789012.dkr.ecr.ap-southeast-2.amazonaws.com/my-app:latest" # <- Replace with your image
#       portMappings = [{
#         containerPort = 80
#         protocol      = "tcp"
#       }]
#     }
#   ])
# }

resource "aws_ecs_task_definition" "service" {
  family                   = "ecs-task-definition0"
  network_mode             = "awsvpc"
  requires_compatibilities = ["FARGATE"]
  cpu                      = 256
  memory                   = 512
  container_definitions = jsonencode([
    {
      name  = "pythia-app"
      image = "${aws_ecr_repository.repository.repository_url}:latest"

      essential = true
      portMappings = [
        {
          containerPort = 3000
        }
      ]
    }
  ])

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
