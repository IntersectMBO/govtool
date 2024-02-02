# VVA Multi/Specific Loader Frontend

It contains simple forms for loading multiple/specific proposals in the sancho net.

### Depends On

- [gov-action-loader-be](../gov-action-loader-be/)
  
### Limitations
Gov action loader backend instance uses fixed set of wallet to perform transactions. This means that gov action loader can be used by only 1 user at a time. 

## Running the frontend

Copy `.env.example` and create `.env` file. Set the configuration variable.

```bash
yarn  # install package.
yarn  dev # start the frontend.
```
### [Developer Docs](./DEVELOPER.md)


### Packaging

Project comes with Dockerfile for building container image

```
docker build --VITE_DATA_LOADER_API='/' VITE_VVA_WEBAPP_URL='https://vva-be.cardanoapi.io'
```

**Note** these variables must be set on the build time and cannot be replaced in runtime.

-  `VITE_DATA_LOADER_API` build arg is used to specify the endpoint  url of `gov-action-loader-be`. This can be set to '/' if the `/api` is mapped to the backend using reverse-proxy.

-  `VITE_VVA_WEBAPP_URL` is used to direct user to gov-action detail page.
