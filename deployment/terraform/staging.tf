terraform {
  backend "s3" {
    bucket = "serokell-tezos-agora-tfstate"
    dynamodb_table = "serokell-tezos-agora-tfstate-lock"
    encrypt = true
    key    = "agora/terraform.tfstate"
    region = "eu-west-2"
    profile = "tezos-default"
  }
}

provider  "aws" {
  version = "~> 2.15"
  region = "eu-west-2"
  profile = "tezos-${terraform.workspace}"
}

# Grab the latest NixOS 19.03 AMI available in our region
data "aws_ami" "nixos" {
  most_recent = true

  filter {
    name = "name"
    values = ["nixos-19.03.*"]
  }

  filter {
    name = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["080433136561"] # NixOS
}

# The staging server
resource "aws_instance" "staging" {
  key_name = "${aws_key_pair.mkaito.key_name}"

  # Networking
  availability_zone = "eu-west-2a"
  subnet_id = "${aws_subnet.tezos-public.id}"
  associate_public_ip_address = true
  vpc_security_group_ids = [
    "${aws_security_group.egress_all.id}",
    "${aws_security_group.tezos_node.id}",
    "${aws_security_group.http.id}",
    "${aws_security_group.ssh.id}",
  ]

  # Instance parameters
  instance_type = "t3a.medium"
  monitoring = true

  # Disk type, size, and contents
  ami = "${data.aws_ami.nixos.id}"
  root_block_device {
    volume_type = "gp2"
    volume_size = "500"
  }
}

# Allow ALL egress traffic
resource "aws_security_group" "egress_all" {
  name = "egress_all"
  description = "Allow inbound and outbound traffic on 9732 and 8732"
  vpc_id = "${aws_vpc.default.id}"

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

# Allow traffic for the tezos node
resource "aws_security_group" "tezos_node" {
  name = "tezos_node"
  description = "Allow inbound and outbound traffic on the RPC interface"
  vpc_id = "${aws_vpc.default.id}"

  ingress {
    from_port = 8732
    to_port = 8732
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

# Allow SSH traffic
resource "aws_security_group" "ssh" {
  name = "ssh"
  description = "Allow inbound and outbound traffic on 9732 and 8732"
  vpc_id = "${aws_vpc.default.id}"

  ingress {
    from_port = 22
    to_port = 22
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

# Allow Frontend traffic
resource "aws_security_group" "http" {
  name = "http"
  description = "Allow inbound and outbound traffic for the Agora frontend"
  vpc_id = "${aws_vpc.default.id}"

  ingress {
    from_port = 80
    to_port = 80
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ingress {
    from_port = 443
    to_port = 443
    protocol = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

# Network resources
resource "aws_vpc" "default" {
  cidr_block = "10.0.0.0/16"
  enable_dns_hostnames = true
}

resource "aws_internet_gateway" "default" {
  vpc_id = "${aws_vpc.default.id}"
}

resource "aws_eip" "staging" {
  instance = "${aws_instance.staging.id}"
  vpc = true
}

# Public subnet
resource "aws_subnet" "tezos-public" {
  vpc_id = "${aws_vpc.default.id}"

  cidr_block = "10.0.0.0/24"
  availability_zone = "eu-west-2a"
}

resource "aws_route_table" "tezos-public" {
  vpc_id = "${aws_vpc.default.id}"

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = "${aws_internet_gateway.default.id}"
  }
}

resource "aws_route_table_association" "eu-west-1a-public" {
  subnet_id = "${aws_subnet.tezos-public.id}"
  route_table_id = "${aws_route_table.tezos-public.id}"
}

## Extra resources
resource "aws_key_pair" "mkaito" {
  key_name   = "mkaito"
  public_key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDOySJ7SAmh+3CBK4fagwkY9PsF6xF+9msMRoQN6JalpauQANALVsVDjC3heFH6Lc/tjLrhQ46oVO3xMFGVKxNe81gaWhvWPxytfH5V8FP52GWEo5HwwMd+VoEyJIYYbj10jwkuzutr9fF0qlp0nhR1IaTKnxJFxV8tUkpiC3a9Qf4yrNy7Ft6DMwyiZSh/mEx+S4LuMqayb93do7+ddlSAyb70NQrLv7H2IRA+qkAzPhZe80o3FqKRvXayH5GSSuYLFfEPFgy0guKAA7P2ICjddLJ+l8BAdTlF8ADY1Z97DvCAgG6CT4cnRzv+cSM+Uvd+ZTxBY6Z+U27kO2LB7UBhVLzrWHSRbv5KWaruFzhOD3E64y3+7XzUg0DpoeS2QVahYc3iF4FvpVfLLPX3F4aev/83Z05G6nEn8lDb1XPAV0KRwo0gB4cCknC6MurnIzxgAeElin9DL5KgVMgVr5jIgBhx01Z9VEVNs5UcMDrA2mXHenY0uAnNk+iWeKZdzxxet50gQuebJ5Q3jHCADS6WZZsBdjxTDiLNvBVo1OiaZ4/tubzVZdrmCkPZDyPUO04Gz7rqXdVFiqzCJgVbcv2gX1qe8UthlRmdblX+l2fY4gvAOGNchVG1cMmvuA5i27td0PqDh6I7kQPvqKQ3QkCI012hwW9ca5S3HGtQDgqSZQ== cardno:000607309598"
}

## DNS
resource "aws_route53_zone" "agora" {
  name = "${terraform.workspace == "default" ? "agora.tezos.serokell.org" : "agora.serokell.io"}"
}

resource "aws_route53_record" "staging" {
  zone_id = "${terraform.workspace == "default" ? aws_route53_zone.agora.zone_id : "Z8JOLZKLR08Z"}"
  name    = "${terraform.workspace == "default" ? "staging.agora.tezos.serokell.org" : "www.tezosagora.org"}"
  type    = "A"
  ttl     = "300"
  records = ["${aws_eip.staging.public_ip}"]
}

## Bucket for TF state storage
resource "aws_s3_bucket" "tfstate" {
  bucket = "serokell-tezos-agora-tfstate${terraform.workspace == "production" ? "production" : ""}"
  acl    = "private"

  versioning {
    enabled = true
  }

  lifecycle {
    prevent_destroy = true
  }
}

## DynamoDB for TF locking and state
resource "aws_dynamodb_table" "tfstatelock" {
  name = "serokell-tezos-agora-tfstate-lock"
  hash_key = "LockID"
  read_capacity = 20
  write_capacity = 20

  attribute {
    name = "LockID"
    type = "S"
  }
}

## Bucket for Node data storage
resource "aws_s3_bucket" "nodedata" {
  bucket = "serokell-tezos-agora-node-data${terraform.workspace == "production" ? "production" : ""}"
  acl    = "public-read"

  lifecycle {
    prevent_destroy = true
  }
}

## Outputs for scripting
output "node_ip" {
  value = "${aws_instance.staging.public_ip}"
}

output "subdomain_ns" {
  value = [ "${aws_route53_zone.agora.name_servers}" ]
}
