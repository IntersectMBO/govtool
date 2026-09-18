import { ChainDataError } from '@govtool/data-providers/chain-data';

export function invalidInput(
  message: string,
  details?: Record<string, unknown>,
): ChainDataError {
  return new ChainDataError('INVALID_INPUT', message, { details });
}

export function notFound(
  message: string,
  details?: Record<string, unknown>,
): ChainDataError {
  return new ChainDataError('NOT_FOUND', message, { details });
}

export function internal(message: string, cause?: unknown): ChainDataError {
  return new ChainDataError('INTERNAL', message, { cause });
}

/**
 * Every route the legacy SQL cannot answer fails this way, and is listed as
 * `unsupported` in `system.getCapabilities()`. Throwing rather than returning
 * an empty page keeps "no data" and "cannot know" distinguishable.
 */
export function unsupported(route: string): ChainDataError {
  return new ChainDataError(
    'CAPABILITY_UNSUPPORTED',
    `db-sync provider does not implement ${route}`,
    { details: { route } },
  );
}

/**
 * Wraps a failure from the database driver. The original error goes on
 * `cause` for logs; the message is kept generic so a connection string or a
 * table name never reaches a response body.
 */
export function fromDatabaseError(error: unknown): ChainDataError {
  if (ChainDataError.is(error)) {
    return error;
  }
  return new ChainDataError('PROVIDER_UNAVAILABLE', 'db-sync query failed', {
    cause: error,
  });
}
