import { ChainDataError } from '@govtool/data-providers/chain-data';

export const invalidInput = (message: string, details?: Record<string, unknown>) =>
  new ChainDataError('INVALID_INPUT', message, details ? { details } : {});

export const notFound = (message: string, details?: Record<string, unknown>) =>
  new ChainDataError('NOT_FOUND', message, details ? { details } : {});

/** Refuse rather than fabricate (SPEC.md §3.5): the value cannot be computed here. */
export const unsupported = (what: string) =>
  new ChainDataError('CAPABILITY_UNSUPPORTED', `Blockfrost provider does not support ${what}`, {
    details: { what },
  });

export const internal = (message: string, details?: Record<string, unknown>) =>
  new ChainDataError('INTERNAL', message, details ? { details } : {});

/**
 * The source has the resource but not the part asked for. Retryable: a
 * Blockfrost instance that has not filled it in yet may on a later read.
 */
export const unavailable = (message: string, details?: Record<string, unknown>) =>
  new ChainDataError('PROVIDER_UNAVAILABLE', message, { retryable: true, ...(details ? { details } : {}) });
