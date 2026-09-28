# Outcomes Pillar

## [Outcomes Pillar](https://github.com/IntersectMBO/govtool-outcomes-pillar)

The Governance Actions Outcomes Pillar backend when combined with the Core GovTool Frontend UI will enable viewing of the Governance Actions -> Outcomes section

## Prerequisites

* This service will also need to access a db-sync instance to successfully retrieve any Governance Actions that have been raised in the past
* A valid IPFS Gateway will also need to be passed as an env var to the service to ensure the metadata associated with the Governance Action can be displayed

## Notice

As the instance of [Cardano GovTool](https://gov.tools) uses a DB-Sync with which leverages some of the input options available in the config file everything may not display identically when running locally e.g. 3rd party providers may not have access to the pool stat table meaning SPO voting may not show/be accurate

## Setting Up the Outcomes Pillar

* Review [oc-env-vars.yaml](https://github.com/aaboyle878/govtool-k8-manifest/blob/6f297e580250882dcefcfbef4f4abcbf56a6ead4/govtool/mainnet/outcomes/oc-env-vars.yaml) and add your custom env vars
* Review [outcomes.yaml](https://github.com/aaboyle878/govtool-k8-manifest/blob/6f297e580250882dcefcfbef4f4abcbf56a6ead4/govtool/mainnet/outcomes/outcomes.yaml) ensuring to update the [Deployment Containers Image Spec](https://github.com/aaboyle878/govtool-k8-manifest/blob/6f297e580250882dcefcfbef4f4abcbf56a6ead4/govtool/mainnet/outcomes/outcomes.yaml#L34) with the associated image for the service (these can be custom or the images referenced in the current deployments) and [metadata -> namespace](https://github.com/aaboyle878/govtool-k8-manifest/blob/6f297e580250882dcefcfbef4f4abcbf56a6ead4/govtool/mainnet/outcomes/outcomes.yaml#L5) if not using the default govtool namespace
* Create the Kubernetes Secrets which will house the env vars using `kubectl apply` in your chosen namespace
* Use `kubectl apply` to launch the outcomes pillar in the same namespace as your secrets -- ([kubectl links](https://app.gitbook.com/o/Prbm1mtkwSsGWSvG1Bfd/s/qBJxNzoywC55SZXT5Tv2/~/changes/107/cardano-govtool/run-govtool-locally/quick-links))
