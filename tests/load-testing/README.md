# API Load Testing

API load testing using gatling.

## Table of Contents

- [Prerequisites](#prerequisites)
- [Manual Run](#manual-run)
- [Docker Build and Run](#docker-build-and-run)
- [Environment Variables](#environment-variables)

## Prerequisites

Before you start, ensure you have the following prerequisites installed:
- Java Development Kit (JDK) version 17.

## Setup Auth
```bash
cd auth_script
npm install
npm run generate-auth
```

## Manual Run

```bash
export API_URL=https://govtool.cardanoapi.io/api
export PDF_API_URL=https://z74f91f2d-zb0719f09-gtw.z937eb260.rustrocks.fr/api
export METADATA_API_URL=https://govtool.cardanoapi.io/metadata-validation
export PEAK_USERS=100
export RAMP_DURATION=40 # in seconds
export STRESS_DURATION=40 # in seconds
./mvnw gatling:test
```

## Run stress test with docker

```bash
docker build -t govtool/load-testing . # build  the image
docker run \
  -e RAMP_DURATION=40 \
  -e PEAK_USERS=100 \
  -e STRESS_DURATION=40 \
  -e API_URL='https://govtool.cardanoapi.io/api'\
  -e PDF_API_URL='https://z74f91f2d-zb0719f09-gtw.z937eb260.rustrocks.fr/api'\
  -e METADATA_API_URL='https://govtool.cardanoapi.io/metadata-validation'\
  govtool/load-testing
```

## Environment Variables
Explain the environment variables used in the project and their purpose.

- API_URL: The URL of the API being tested.
- PDF_API_URL: The URL of the PDF API being tested.
- METADATA_API_URL: The URL of the METADATA_API being tested.
- PEAK_USERS:  The number of users to be injected during the test for each scenario.
- RAMP_DURATION:The duration over which the user rate gradually increases.
- STRESS_DURATION: The duration over which the stress peak occurs.
