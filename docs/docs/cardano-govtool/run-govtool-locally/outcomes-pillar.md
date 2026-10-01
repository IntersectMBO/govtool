# Outcomes Pillar

The Governance Actions Outcomes section is now part of Core GovTool:

* The **outcomes UI** is part of the GovTool frontend source, enabled with the `VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED` flag.
* The **outcomes API** is served by the GovTool backend under `/outcomes`. Point the frontend at it with `VITE_OUTCOMES_API_URL` (for example `http://localhost:9999/outcomes`).

The separate [Outcomes Pillar](https://github.com/IntersectMBO/govtool-outcomes-pillar) backend is no longer deployed, and you do not need to run it. Follow [Core GovTool](./core-govtool.md) to set up the frontend and backend.

## Prerequisites

* The backend needs a data source that can retrieve Governance Actions raised in the past, such as a db-sync instance (see [Core GovTool](./core-govtool.md)).
* A valid IPFS Gateway (`IPFS_GATEWAY`) should be set on the backend so that the metadata associated with the Governance Action can be displayed.
* The outcomes route that links a Governance Action to its proposal discussion uses `GOVTOOL_PDF_API_URL`. Without it, that route answers `503`.

## Notice

As the instance of [Cardano GovTool](https://gov.tools) uses a DB-Sync with which leverages some of the [insert options](https://github.com/IntersectMBO/cardano-db-sync/blob/13.6.0.4/doc/configuration.md#properties) available in the config file everything may not display identically when running locally e.g. 3rd party providers may not have access to the pool stat table meaning SPO voting may not show/be accurate
