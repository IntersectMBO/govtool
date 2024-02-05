variable "ami" {
  default = "ami-01dd271720c1ba44f"
}
variable "app_env" {}
variable "app_name" {
  default = "govtool"
}
variable "cardano_network" {}
variable "custom_subdomain" {
  default = ""
}
variable "dns_zone_id" {
  default = ""
}
variable "instance_type" {
  default = "t3.micro"
}
variable "region" {
  default = "eu-west-1"
}
variable "volume_size" {
  default = 400
}
variable "volume_type" {
  default = "gp3"
}
