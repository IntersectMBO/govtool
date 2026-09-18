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
 * Every route Koios cannot answer fails this way, and is listed as
 * `unsupported` in `system.getCapabilities()`. Throwing rather than returning
 * an empty page keeps "no data" and "cannot know" distinguishable.
 *
 * `reason` is carried because a Koios gap is almost never "the endpoint is
 * missing" — it is usually "the endpoint exists but omits the field the
 * contract needs", and a caller deciding whether to switch provider needs
 * that distinction.
 */
export function unsupported(route: string, reason: string): ChainDataError {
  return new ChainDataError(
    'CAPABILITY_UNSUPPORTED',
    `Koios provider does not implement ${route}: ${reason}`,
    { details: { route, reason } },
  );
}
