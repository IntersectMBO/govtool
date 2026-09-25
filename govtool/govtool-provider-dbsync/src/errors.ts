import { ChainDataError } from '@govtool/data-providers/chain-data';

export const invalidInput = (message: string, details?: Record<string, unknown>) =>
  new ChainDataError('INVALID_INPUT', message, details ? { details } : {});

export const notFound = (message: string, details?: Record<string, unknown>) =>
  new ChainDataError('NOT_FOUND', message, details ? { details } : {});

/** Refuse rather than fabricate (SPEC.md §3.5): the value cannot be computed here. */
export const unsupported = (what: string) =>
  new ChainDataError('CAPABILITY_UNSUPPORTED', `db-sync provider does not support ${what}`, {
    details: { what },
  });

export const internal = (message: string, cause?: unknown) =>
  new ChainDataError('INTERNAL', message, cause === undefined ? {} : { cause });
