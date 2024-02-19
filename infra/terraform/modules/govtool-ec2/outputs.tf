output "govtool_eip" {
  value = aws_eip.govtool.public_ip
}

output "frontend_domain" {
  value = var.app_env == "beta" ? aws_eip.govtool.public_ip : aws_route53_record.frontend[0].fqdn
}
