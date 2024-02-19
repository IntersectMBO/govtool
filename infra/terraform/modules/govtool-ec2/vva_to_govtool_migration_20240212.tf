# FIXME This Terraform file in question is designed to migrate resource and
# module names, serving as a transitional tool to ensure consistent naming
# conventions across the infrastructure. Once it is confirmed that all users
# have applied this migration, the file can be safely removed in the future to
# maintain clarity and efficiency in the codebase.

moved {
  from = aws_instance.vva
  to   = aws_instance.govtool
}

moved {
  from = aws_eip.vva
  to   = aws_eip.govtool
}
