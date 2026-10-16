/**
 * Metadata limits, set in code rather than the environment (D120): changing
 * one is a reviewed, versioned change.
 */

/**
 * The most bytes read from a metadata url. The same 2 MB the metadata service
 * uses, so a document the service accepts is never rejected here.
 */
export const METADATA_FETCH_LIMIT_BYTES = 2 * 1024 * 1024;
