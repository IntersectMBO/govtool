# Proposal Pillar

## [Proposal Pillar](https://github.com/IntersectMBO/govtool-proposal-pillar)

The Proposal Pillar backend when combined with the Core GovTool Frontend UI will enable viewing of Budget Proposals and Governance Actions -> Proposals areas

## Prerequisites

* As the proposal pillar was designed for off-chain discussion it will require a Postgres DB to store any user discussion information

## Notice

* As stated above because this service covers off chain discussions when initially launched the DB will be empty and so there will be nothing to load when the respective pages are accessed from the frontend UI
* As this repo was designed for dev purposes only the db has not been assigned any persistent storage (if running long term this will need to be updated)

## Setting Up the Proposal Pillar

* Review the [pdf-db-connect.yaml](https://github.com/aaboyle878/govtool-k8-manifest/blob/6f297e580250882dcefcfbef4f4abcbf56a6ead4/govtool/mainnet/pdf/pdf-db-connect.yaml) and [pdf-env-vars.yaml](https://github.com/aaboyle878/govtool-k8-manifest/blob/6f297e580250882dcefcfbef4f4abcbf56a6ead4/govtool/mainnet/pdf/pdf-env-vars.yaml) files adding your custom env vars
* Review [pdf-db.yaml](https://github.com/aaboyle878/govtool-k8-manifest/blob/6f297e580250882dcefcfbef4f4abcbf56a6ead4/govtool/mainnet/pdf/pdf-db.yaml) and [pdf.yaml](https://github.com/aaboyle878/govtool-k8-manifest/blob/6f297e580250882dcefcfbef4f4abcbf56a6ead4/govtool/mainnet/pdf/pdf.yaml) ensuring to update the [Deployment Containers Image Spec](https://github.com/aaboyle878/govtool-k8-manifest/blob/6f297e580250882dcefcfbef4f4abcbf56a6ead4/govtool/mainnet/pdf/pdf.yaml#L34) with the associated image for the service (these can be custom or the images referenced in the current deployments) and [metadata -> namespace](https://github.com/aaboyle878/govtool-k8-manifest/blob/6f297e580250882dcefcfbef4f4abcbf56a6ead4/govtool/mainnet/pdf/pdf.yaml#L5) if not using the default govtool namespace
* Create the Kubernetes Secrets which will house the env vars using `kubectl apply` in your chosen namespace
* Use `kubectl apply` to first create the pdf db and then pdf service in the same namespace as your secrets -- ([kubectl links](https://app.gitbook.com/o/Prbm1mtkwSsGWSvG1Bfd/s/qBJxNzoywC55SZXT5Tv2/~/changes/107/cardano-govtool/run-govtool-locally/quick-links))
