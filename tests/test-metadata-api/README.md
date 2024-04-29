Test medatada API
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


## Available Endpoints

### Save File

- **Endpoint:** `PUT /data/{filename}`
- **Description:** Saves data to a file with the specified filename.

### Get File

- **Endpoint:** `GET /data/{filename}`
- **Description:** Retrieves the content of the file with the specified filename.

### Delete File

- **Endpoint:** `DELETE /data/{filename}`
- **Description:** Deletes the file with the specified filename.