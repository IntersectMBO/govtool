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

## Manual Run

```bash
export TARGET_USER_RATE=<target_user_rate_value> RAMP_DURATION=<ramp_duration_value> PEAK_USERS=<peak_users_value> STRESS_DURATION=<stress_duration_value> API_URL=<api_url>; ./mvnw gatling:test
```

## Docker Build and Run

1. Build the Docker image:
```bash
docker build -t load-testing .
```
2. Run the Docker container:
```bash
docker run -e TARGET_USER_RATE=<target_user_rate> -e RAMP_DURATION=<ramp_duration> -e PEAK_USERS=<peak_users> -e STRESS_DURATION=<stress_duration> -e API_URL=<api_url> load-testing
```

## Environment Variables
Explain the environment variables used in the project and their purpose.

- TARGET_USER_RATE: The target rate of users per second during the test.
- RAMP_DURATION:The duration over which the user rate gradually increases.
- PEAK_USERS:  The number of users to be injected during the stress peak.
- STRESS_DURATION: The duration over which the stress peak occurs.
- API_URL: The URL of the API being tested.
