resource "aws_vpc" "vpc" {
  cidr_block           = "172.30.0.0/16"
  enable_dns_hostnames = "true"

  tags = {
    Name            = "${var.app_name}_${var.app_env}_${var.cardano_network}"
    app_name        = var.app_name
    app_env         = var.app_env
    cardano_network = var.cardano_network
  }
}

resource "aws_internet_gateway" "igw" {
  vpc_id = aws_vpc.vpc.id

  tags = {
    Name            = "${var.app_name}_${var.app_env}_${var.cardano_network}"
    app_name        = var.app_name
    app_env         = var.app_env
    cardano_network = var.cardano_network
  }
}

resource "aws_route_table" "rt" {
  vpc_id = aws_vpc.vpc.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.igw.id
  }

  tags = {
    Name            = "${var.app_name}_${var.app_env}_${var.cardano_network}"
    app_name        = "${var.app_name}"
    app_env         = "${var.app_env}"
    cardano_network = var.cardano_network
  }
}

resource "aws_subnet" "sub01" {
  vpc_id            = aws_vpc.vpc.id
  cidr_block        = "172.30.0.0/20"
  availability_zone = "eu-west-1a"

  tags = {
    Name            = "${var.app_name}_${var.app_env}_${var.cardano_network}"
    app_name        = var.app_name
    app_env         = var.app_env
    cardano_network = var.cardano_network
  }
}

resource "aws_route_table_association" "rta" {
  subnet_id      = aws_subnet.sub01.id
  route_table_id = aws_route_table.rt.id
}

resource "aws_security_group" "out_sg" {
  name        = "out"
  description = "Allow outbound traffic"

  vpc_id = aws_vpc.vpc.id

  egress {
    from_port        = 0
    to_port          = 0
    protocol         = "-1"
    cidr_blocks      = ["0.0.0.0/0"]
    ipv6_cidr_blocks = ["::/0"]
  }

  tags = {
    Name            = "${var.app_name}_${var.app_env}_${var.cardano_network}"
    app_name        = var.app_name
    app_env         = var.app_env
    cardano_network = var.cardano_network
  }
}

resource "aws_security_group" "ssh_sg" {
  name        = "ssh"
  description = "Allow inbound traffic on port 22"

  vpc_id = aws_vpc.vpc.id

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    description = "BinarApps Office"
    cidr_blocks = ["185.48.176.106/32"]
  }

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    description = "BinarApps a.guderski"
    cidr_blocks = ["85.89.169.166/32"]
  }

  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    description = "public internet, replace with github actions ip in the future"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name            = "${var.app_name}_${var.app_env}_${var.cardano_network}"
    app_name        = var.app_name
    app_env         = var.app_env
    cardano_network = var.cardano_network
  }
}

resource "aws_security_group" "web_sg" {
  name        = "web"
  description = "Allow inbound traffic on ports 80 and 443"

  vpc_id = aws_vpc.vpc.id

  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  tags = {
    Name            = "${var.app_name}_${var.app_env}_${var.cardano_network}"
    app_name        = var.app_name
    app_env         = var.app_env
    cardano_network = var.cardano_network
  }
}

resource "aws_instance" "vva" {
  ami           = var.ami
  instance_type = var.instance_type

  root_block_device {
    volume_size = var.volume_size
    volume_type = var.volume_type
  }

  user_data = file("${path.module}/user_data.sh")
  # user_data_replace_on_change = true

  credit_specification {
    cpu_credits = "unlimited"
  }

  subnet_id = aws_subnet.sub01.id
  vpc_security_group_ids = [
    aws_security_group.out_sg.id,
    aws_security_group.ssh_sg.id,
    aws_security_group.web_sg.id
  ]

  tags = {
    Name            = "${var.app_name}_${var.app_env}_${var.cardano_network}"
    app_name        = var.app_name
    app_env         = var.app_env
    cardano_network = var.cardano_network
  }
}

resource "aws_eip" "vva" {
  instance = aws_instance.vva.id

  tags = {
    Name            = "${var.app_name}_${var.app_env}_${var.cardano_network}"
    app_name        = var.app_name
    app_env         = var.app_env
    cardano_network = var.cardano_network
  }
}

resource "aws_route53_record" "frontend" {
  count   = var.app_env == "beta" ? 0 : 1
  zone_id = var.dns_zone_id
  name    = "${var.custom_subdomain != "" ? "${var.custom_subdomain}" : "${var.app_env}-${var.cardano_network}"}"
  type    = "A"
  ttl     = 180
  records = [aws_eip.vva.public_ip]
}
