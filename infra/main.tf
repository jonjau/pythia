resource "aws_iam_role" "ecs_task_execution" {
  name = "ecs-task-execution-role-0"

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

  tags = var.tags
}

resource "aws_iam_policy" "dynamodb_access" {
  name        = "DynamoAccess"
  description = "Policy to allow access to DynamoDB for ECS tasks"
  policy = jsonencode({
    Version = "2012-10-17"
    Statement = [{
      Effect = "Allow"
      Action = [
        "dynamodb:CreateTable",
        "dynamodb:DescribeTable",
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

  tags = var.tags
}

resource "aws_iam_role_policy_attachment" "ecs_task_dynamodb_access" {
  role       = aws_iam_role.ecs_task_iam_role.name
  policy_arn = aws_iam_policy.dynamodb_access.arn
}

resource "aws_ecs_cluster" "cluster" {
  name = "ecs-cluster-0"

  tags = var.tags
}

// A list of all AZs available in the region configured in the AWS credentials
data "aws_availability_zones" "available" {}

resource "aws_vpc" "vpc" {
  cidr_block = "10.0.0.0/24"

  tags = var.tags
}

resource "aws_vpc_endpoint" "dynamodb" {
  vpc_id            = aws_vpc.vpc.id
  service_name      = "com.amazonaws.${var.aws_region}.dynamodb"
  vpc_endpoint_type = "Gateway"
  route_table_ids   = aws_route_table.private[*].id
}

resource "aws_internet_gateway" "default" {
  vpc_id = aws_vpc.vpc.id

  tags = var.tags
}

resource "aws_route_table" "private" {
  count  = var.az_count
  vpc_id = aws_vpc.vpc.id

  // fck-nat will manage routes for private subnets

  tags = var.tags
}

resource "aws_subnet" "public" {
  count  = var.az_count
  vpc_id = aws_vpc.vpc.id
  // Get /28 subnet per AZ from the /24 VPC CIDR block
  cidr_block        = cidrsubnet(aws_vpc.vpc.cidr_block, 4, count.index)
  availability_zone = data.aws_availability_zones.available.names[count.index]

  tags = var.tags
}

// Route Table with egress route to the internet
resource "aws_route_table" "public" {
  vpc_id = aws_vpc.vpc.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.default.id
  }

  tags = var.tags
}

// Associate Route Table with Public Subnets
resource "aws_route_table_association" "public" {
  count          = var.az_count
  subnet_id      = aws_subnet.public[count.index].id
  route_table_id = aws_route_table.public.id
}

resource "aws_subnet" "private" {
  count  = var.az_count
  vpc_id = aws_vpc.vpc.id
  // Get /28 subnet per AZ from the /24 VPC CIDR block, add var.az_count to the index to avoid overlap with public subnets
  cidr_block        = cidrsubnet(aws_vpc.vpc.cidr_block, 4, var.az_count + count.index)
  availability_zone = data.aws_availability_zones.available.names[count.index]

  tags = var.tags
}

resource "aws_route_table_association" "private" {
  count          = var.az_count
  subnet_id      = aws_subnet.private[count.index].id
  route_table_id = aws_route_table.private[count.index].id
}

resource "aws_cloudwatch_log_group" "log_group" {
  name              = "/ecs/${var.app_container_name}"
  retention_in_days = 14

  tags = var.tags
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
        { name = "RUST_LOG", value = "debug" },
        { name = "PYTHIA_RUN_MODE", value = "remote" },
        { name = "AWS_REGION", value = "us-west-2" }
      ]

      logConfiguration = {
        logDriver = "awslogs",
        options = {
          "awslogs-group"         = aws_cloudwatch_log_group.log_group.name,
          "awslogs-region"        = var.aws_region,
          "awslogs-stream-prefix" = var.app_container_name
        }
      }
    }
  ])

  tags = var.tags
}

resource "aws_security_group" "alb" {
  name        = "alb-sg-0"
  description = "Allow HTTP access to ALB"
  vpc_id      = aws_vpc.vpc.id

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = var.tags
}

resource "aws_lb" "main" {
  name               = "ecs-alb"
  internal           = false
  load_balancer_type = "application"
  security_groups    = [aws_security_group.alb.id]
  subnets            = aws_subnet.public[*].id

  tags = var.tags
}

resource "aws_lb_target_group" "ecs" {
  name        = "ecs-target-group-0"
  port        = var.container_port
  protocol    = "HTTP"
  target_type = "ip" # required for Fargate
  vpc_id      = aws_vpc.vpc.id

  health_check {
    path                = "/"
    protocol            = "HTTP"
    matcher             = "200"
    interval            = 30
    timeout             = 5
    healthy_threshold   = 2
    unhealthy_threshold = 2
  }

  tags = var.tags
}

// Listen for HTTP traffic on port 80.
// When a request comes in, forward it to the ECS target group behind the load balancer.
resource "aws_lb_listener" "http" {
  load_balancer_arn = aws_lb.main.arn
  port              = 80
  protocol          = "HTTP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.ecs.arn
  }

  tags = var.tags
}

resource "aws_security_group" "ecs_service" {
  name        = "ecs-sg-0"
  description = "Security group for ECS task running on Fargate"
  vpc_id      = aws_vpc.vpc.id

  ingress {
    from_port   = var.container_port
    to_port     = var.container_port
    protocol    = "tcp"
    cidr_blocks = [aws_vpc.vpc.cidr_block]
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = var.tags
}


resource "aws_ecs_service" "service" {
  name            = "ecs-service-0"
  cluster         = aws_ecs_cluster.cluster.id
  launch_type     = "FARGATE"
  task_definition = aws_ecs_task_definition.service.arn
  desired_count   = 1

  network_configuration {
    assign_public_ip = false
    subnets          = aws_subnet.private[*].id
    security_groups  = [aws_security_group.ecs_service.id]
  }

  load_balancer {
    target_group_arn = aws_lb_target_group.ecs.arn
    container_name   = var.app_container_name
    container_port   = var.container_port
  }

  tags = var.tags
}
