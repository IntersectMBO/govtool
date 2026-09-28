# Getting Started

## Prerequisites

* It is heavily advised that the repos for all the pillars of GovTool that you would like to deploy be forked into you own local copy. (Each section title links back to the parent repo, links can also be found in the README file of this repo)
* After forking the repo it is recommended to have a look at the Check and Build workflows as these are used by the respective upstream repos to build the images used in production. [Build Workflow Links](https://app.gitbook.com/o/Prbm1mtkwSsGWSvG1Bfd/s/qBJxNzoywC55SZXT5Tv2/~/changes/107/cardano-govtool/run-govtool-locally/quick-links)
* Each of the Upstream repos will use environment variables similar to those which are listed in the **env-vars.yaml** files for each respective pillar with the exception of the GovTool backend which will rely upon the **config.json** however for deployment purposes this will just need to be treated as a secret similar to how the env-vars.yaml files are
* As this was designed to be run locally using a Minikube cluster it is advised to have Docker installed and a Minikube Image downloaded locally [Guide Links](https://app.gitbook.com/o/Prbm1mtkwSsGWSvG1Bfd/s/qBJxNzoywC55SZXT5Tv2/~/changes/107/cardano-govtool/run-govtool-locally/quick-links)

## Pillar Naming Conventions

Throughout this Wiki section you may find multiple references to backend and frontend services for the sake of clarity the following will be true:

* pdf backend will reference the GovTool Proposal Pillar
* outcomes backend will reference the GovTool Outcomes Pillar
* backend will reference the Core GovTool Backend
* metadata will reference the Core GovTool Metadata Validation Backend
* frontend will reference the Core GovTool Frontend UI

