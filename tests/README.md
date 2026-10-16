# GovTool Tests

This directory contains tests for the GovTool project.

## 📍 Navigation

- [Backend Tests](./govtool-backend/)
- [Frontend Tests](./govtool-frontend/playwright/)
- [Devnet](./devnet/)
- [Load Tests](./load-testing/)
- [Metadata API](./test-metadata-api/)

## Backend Tests
- Conducts basic tests on GovTool backend endpoints using Python.

## Frontend Tests
- Performs integration tests on the deployed GovTool platform using Playwright.

## Devnet
- Runs GovTool and both suites above against a local Cardano devnet, with no secrets and no public Cardano network (image pulls and external links aside).

## Load Tests
- Executes load tests on the GovTool API using Gatling.

## Metadata API
- A simple service to host JSON metadata during testing.