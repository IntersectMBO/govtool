import { AxiosError } from "axios";

/**
 * The backend answers a name or text search with 503 until it has fetched
 * every DRep's and governance action's document once, rather than return
 * results that leave out the ones it has not read yet.
 */
export const isSearchNotReady = (error: unknown): boolean =>
  error instanceof AxiosError &&
  error.response?.status === 503 &&
  (error.response.data as { errorType?: string } | undefined)?.errorType ===
    "ServiceUnavailableError";

/** Asked again every 5 s for about a minute before the page says so. */
export const SEARCH_NOT_READY_RETRIES = 12;
export const SEARCH_NOT_READY_RETRY_DELAY_MS = 5_000;
