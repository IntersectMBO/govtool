# FIXME This Terraform file in question is designed to migrate resource and
# module names, serving as a transitional tool to ensure consistent naming
# conventions across the infrastructure. Once it is confirmed that all users
# have applied this migration, the file can be safely removed in the future to
# maintain clarity and efficiency in the codebase.

moved {
  from = module.vva-ecr-be
  to   = module.govtool-ecr-backend
}

moved {
  from = module.vva-ecr-fe
  to   = module.govtool-ecr-frontend
}

moved {
  from = module.vva-dev-sanchonet
  to   = module.govtool-dev-sanchonet
}

moved {
  from = module.vva-test-sanchonet
  to   = module.govtool-test-sanchonet
}

moved {
  from = module.vva-staging-sanchonet
  to   = module.govtool-staging-sanchonet
}

moved {
  from = module.vva-beta-sanchonet
  to   = module.govtool-beta-sanchonet
}
