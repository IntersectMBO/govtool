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
 * A route this provider cannot serve. Every one is listed as `unsupported` in
 * `system.getCapabilities()`, with the reason recorded there, so a consumer
 * discovers the gap by asking rather than by failing.
 */
export function unsupported(route: string, reason?: string): ChainDataError {
  return new ChainDataError(
    'CAPABILITY_UNSUPPORTED',
    `blockfrost provider does not implement ${route}`,
    { details: reason === undefined ? { route } : { route, reason } },
  );
}
