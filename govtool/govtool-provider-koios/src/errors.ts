import { ChainDataError } from '@govtool/data-providers/chain-data';

export const invalidInput = (message: string, details?: Record<string, unknown>) =>
  new ChainDataError('INVALID_INPUT', message, details ? { details } : {});

export const notFound = (message: string, details?: Record<string, unknown>) =>
  new ChainDataError('NOT_FOUND', message, details ? { details } : {});

/** Refuse rather than fabricate (SPEC.md §3.5): the value cannot be computed here. */
export const unsupported = (what: string, why?: string) =>
  new ChainDataError('CAPABILITY_UNSUPPORTED', `Koios provider does not support ${what}${why ? `: ${why}` : ''}`, {
    details: { what },
  });

export const internal = (message: string, details?: Record<string, unknown>, cause?: unknown) =>
  new ChainDataError('INTERNAL', message, {
    ...(details ? { details } : {}),
    ...(cause === undefined ? {} : { cause }),
  });

/** Koios has not caught up with something it will know shortly. */
export const staleData = (message: string, details?: Record<string, unknown>) =>
  new ChainDataError('STALE_DATA', message, { retryable: true, ...(details ? { details } : {}) });

/** Koios has the record but cannot serve the part asked for; another instance or a later try may. */
export const unavailable = (message: string, details?: Record<string, unknown>) =>
  new ChainDataError('PROVIDER_UNAVAILABLE', message, { retryable: true, ...(details ? { details } : {}) });
