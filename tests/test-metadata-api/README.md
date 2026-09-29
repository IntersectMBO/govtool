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

## IPFS Endpoints

A stand-in for Pinata and an IPFS gateway, so an isolated test environment
needs neither. The CID is computed from the bytes the way IPFS does for
single-block content with raw leaves (CIDv1, raw codec, sha2-256, base32,
`bafkrei...`), which is also what Pinata returns for GovTool metadata. Content
is stored under `IPFS_DIR` (default `$DATA_DIR/ipfs`); the limit is 512 KiB.

- `POST /ipfs` (or `PUT /ipfs`): pin the raw request body; `201 {"cid": "bafkrei..."}`.
- `GET /ipfs/{cid}`: the exact bytes pinned, as a path gateway serves them, with
  `Content-Type` sniffed (JSON, text or octet-stream). `404` when not pinned here.
- `DELETE /ipfs/{cid}`: unpin.
- `GET /ipfs`: health, `{"status": "ok"}`.

Use it as a gateway base url:

- frontend `VITE_IPFS_GATEWAY=http://<host>:3000/ipfs`
- metadata service `IPFS_PRIMARY_GATEWAY=http://<host>:3000` (it appends `/ipfs/<cid>`)
  with `METADATA_ALLOW_PRIVATE_ADDRESSES=true`, and `IPFS_GATEWAYS` set to the same
  url to drop the public fallbacks
- metadata-validation `IPFS_GATEWAY=http://<host>:3000/ipfs`
- db-sync: `"ipfs_gateway": ["http://<host>:3000/ipfs"]` in its config file
- backend pinning: `GOVTOOL_PINNING_PROVIDER=test` and
  `GOVTOOL_TEST_PINNING_URL=http://<host>:3000`

## Tests

```
yarn test
```
