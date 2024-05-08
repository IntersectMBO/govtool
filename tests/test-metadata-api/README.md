Test metadata API
=================

Simple service to host json metadata during testing.

## Installation

```
git clone https://github.com/your/repository.git
yarn install
yarn start
```
#### Swagger UI

```
http://localhost:3000/docs
```

## Metadata Endpoints

### 1. Save File

- **Endpoint:** `PUT /data/{filename}`
- **Description:** Saves data to a file with the specified filename.

### 2. Get File

- **Endpoint:** `GET /data/{filename}`
- **Description:** Retrieves the content of the file with the specified filename.

### 3. Delete File

- **Endpoint:** `DELETE /data/{filename}`
- **Description:** Deletes the file with the specified filename.

## Locks Endpoint
### 1. Acquire Lock
- **Endpoint:** `POST /lock/{key}?expiry={expiry_secs}`
- **Description:** Acquire a lock for the specified key for given time. By default the lock is set for 180 secs.
- **Responses:**
   - `200 OK`: Lock acquired successfully.
   - `423 Locked`: Lock not available.

### 2. Release Lock

- **Endpoint:** `POST/unlock/{key}`
- **Description:** Release a lock for the specified key.
