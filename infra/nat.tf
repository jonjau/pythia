module "fck_nat" {
  source  = "RaJiska/fck-nat/aws"
  version = "1.3.0" # or latest stable version

  name      = "nat0"
  vpc_id    = aws_vpc.vpc.id
  subnet_id = aws_subnet.public[0].id

  update_route_tables = true

  route_tables_ids = {
    for i in range(var.az_count) :
    "private-${i}" => aws_route_table.private[i].id
  }

  tags = {
    "${var.tag_prefix}:Owner"       = "devops"
    "${var.tag_prefix}:Environment" = var.environment
  }
}
