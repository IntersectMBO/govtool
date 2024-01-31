/*
    This file is used to store all the configuration variables.
    In this case, we are storing the API url.
*/

const dataLoaderApi = import.meta.env.VITE_DATA_LOADER_API
const mockLoaderApi = import.meta.env.MOCK_LOADER_API
const vvaWebappUrl: string | undefined = import.meta.env.VITE_VVA_WEBAPP_URL

function removeTrailingSlash(url: string | undefined) {
  if (url && url.endsWith('/')) {
    return url.substring(0, url.length - 1)
  } else {
    return url
  }
}

export default {
  dataLoaderApi: removeTrailingSlash(dataLoaderApi),
  mockLoaderApi: removeTrailingSlash(mockLoaderApi),
  vvaWebappUrl: removeTrailingSlash(vvaWebappUrl),
}
