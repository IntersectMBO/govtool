# Test Metrics API

This api is intended to be called by Github Action whenever a new commit to vva app has been made and new metrics are obtained. The api expects a POST request with a JSON file of the metrics which will then update the database with the new metrics.

## Prerequisite

- **PostgresSql** Database

### Table Structure

Database tables are automatically created upon startup

#### `test_metrics` Table

| Column Name | Data Type | Constraints |
| ----------- | --------- | ----------- |
| id          | SERIAL    | PRIMARY KEY |
| outcome     | TEXT      | NOT NULL    |
| start_date  | BIGINT    | NOT NULL    |
| end_date    | BIGINT    | NOT NULL    |
| build_id    | TEXT      | NOT NULL    |
| test_name   | TEXT      | NOT NULL    |
| commit_hash | TEXT      | NOT NULL    |

#### `endpoint_metrics` Table

| Column Name   | Data Type | Constraints |
| ------------- | --------- | ----------- |
| id            | SERIAL    | PRIMARY KEY |
| build_id      | TEXT      | NOT NULL    |
| method        | TEXT      | NOT NULL    |
| endpoint      | TEXT      | NOT NULL    |
| path_param    | TEXT      |             |
| json          | text      |             |
| status_code   | INTEGER   | NOT NULL    |
| response_json | text      |             |
| response_time | BIGINT    | NOT NULL    |
| start_date    | BIGINT    | NOT NULL    |

### How to run

To run it in develop mode, you can use `npm run make` command in the terminal
To compile it, you can use `npm run build` command in the terminal
To run the compiled version, you can use `npm run start` command in the terminal

| Environment Variable | Description                                                                        |
| -------------------- | ---------------------------------------------------------------------------------- |
| API_SECRET_TOKEN     | A secret token that must be provided in each API call in the header `secret-token` |
| PGHOST               | Hostname for the database server                                                   |
| PGPORT               | Port number for the database server                                                |
| PGDATABASE           | Database name                                                                      |
| PGUSER               | Database username                                                                  |
| PGPASSWORD           | Database password                                                                  |
| PORT                 | Port number for the API                                                            |

### Posting metrics to the server

```
curl -X POST http://localhost:8080 \
  -d "@metrics.json" \
  -H "commitHash: abc" \
  -H "secret-token: token"
  -H 'Content-Type: application/json'
```

# Metrics API Reference

## Authentication

Each API request must include a secret token in the request headers for authentication.

- Headers:
  - `secret-token`: The secret token used to authenticate requests.

If the provided `secret-token` is missing or invalid, the server will return a `403 Forbidden` status code, and the request will not be processed further.

---

# Metrics API Reference

## Test Result Metrics

### **POST** `/metrics/test-results`

Stores test execution results into the database.

#### Request

- Headers:

  - `secret-token`: The secret token used to authenticate requests.

- Body (application/json):

  ```json
  {
    "outcome": "passed",
    "start_date": 1625072400000,
    "end_date": 1625076000000,
    "build_id": "build-12345",
    "test_name": "Unit Tests",
    "commit_hash": "abc123def456"
  }
  ```

  - `TestMetricsJson` object structure.

#### Response

- **200 OK**: Metrics have been successfully uploaded.
- **400 Bad Request**: Metrics upload not successful.
- **403 Forbidden**: Secret Token missing or invalid.

---

## API Endpoint Metrics

### **POST** `/metrics/api-endpoints`

Stores metrics related to API endpoint performance into the database.

#### Request

- Headers:

  - `secret-token`: The secret token used to authenticate requests.

- Body (application/json):

  ```json
  {
    "build_id": "build-67890",
    "method": "GET",
    "endpoint": "/users/{userId}",
    "path_param": "userId",
    "json": {},
    "status_code": 200,
    "response_json": { "name": "John Doe" },
    "response_time": 120,
    "start_date": 1625079600000
  }
  ```

  - Replace any placeholder `{userId}` in the endpoint with actual path parameters.
  - The `json` key should contain the JSON payload sent to the server or empty object if none.
  - `EndpointMetricsJson` object structure.

#### Response

- **200 OK**: Metrics have been successfully uploaded.
- **400 Bad Request**: Metrics upload not successful.
- **403 Forbidden**: Secret Token missing or invalid.

---


## Packaging
Project used Docker for packaging. Build docker image with
```
docker build -t voltaire-era/test-metrics-api .
```
