import { HttpException } from '@nestjs/common';
import { ChainDataError } from '@govtool/data-providers/chain-data';
import { PinningError } from '@govtool/data-providers/pinning';

/**
 * The single place the data layer's error vocabulary becomes HTTP.
 *
 * Status codes and `errorType` strings are the ones the legacy backend used,
 * so a client that branched on them keeps working.
 */
const CHAIN_DATA_STATUS: Record<ChainDataError['code'], number> = {
  INVALID_INPUT: 400,
  NOT_FOUND: 404,
  CAPABILITY_UNSUPPORTED: 501,
  PROVIDER_UNAVAILABLE: 503,
  PROVIDER_RATE_LIMITED: 429,
  PROVIDER_TIMEOUT: 504,
  STALE_DATA: 503,
  INTERNAL: 500,
};

const CHAIN_DATA_ERROR_TYPE: Record<ChainDataError['code'], string> = {
  INVALID_INPUT: 'ValidationError',
  NOT_FOUND: 'NotFoundError',
  CAPABILITY_UNSUPPORTED: 'NotImplementedError',
  PROVIDER_UNAVAILABLE: 'ProviderUnavailableError',
  PROVIDER_RATE_LIMITED: 'RateLimitedError',
  PROVIDER_TIMEOUT: 'ProviderTimeoutError',
  STALE_DATA: 'StaleDataError',
  INTERNAL: 'CriticalError',
};

const PINNING_STATUS: Record<PinningError['reason'], number> = {
  TOO_LARGE: 400,
  UNSUPPORTED_CONTENT_TYPE: 400,
  INVALID_METADATA: 400,
  QUOTA_EXCEEDED: 429,
  RATE_LIMITED: 429,
  BACKEND_UNAVAILABLE: 503,
  BACKEND_TIMEOUT: 504,
  BACKEND_ERROR: 503,
};

/**
 * `pin()` failures keep the legacy `errorType` names exactly, because the
 * frontend's upload dialog matches on them.
 */
const PINNING_ERROR_TYPE: Record<PinningError['reason'], string> = {
  TOO_LARGE: 'ValidationError',
  UNSUPPORTED_CONTENT_TYPE: 'ValidationError',
  INVALID_METADATA: 'ValidationError',
  QUOTA_EXCEEDED: 'RateLimitedError',
  RATE_LIMITED: 'RateLimitedError',
  BACKEND_UNAVAILABLE: 'PinataConenctionError',
  BACKEND_TIMEOUT: 'PinataConenctionError',
  BACKEND_ERROR: 'PinataAPIError',
};

export function toHttpException(error: unknown): HttpException {
  if (error instanceof HttpException) {
    return error;
  }

  if (ChainDataError.is(error)) {
    return new HttpException(
      {
        errorType: CHAIN_DATA_ERROR_TYPE[error.code],
        message: error.message,
        ...(error.details ? { details: error.details } : {}),
      },
      CHAIN_DATA_STATUS[error.code],
    );
  }

  if (PinningError.is(error)) {
    // The legacy upload endpoint nested Pinata's own status and body under
    // `pinataResponse`. A `PinningError` carries only a reason and a message
    // now, so the reason is all there is to report.
    return new HttpException(
      {
        errorType: PINNING_ERROR_TYPE[error.reason],
        message: error.message,
      },
      PINNING_STATUS[error.reason],
    );
  }

  return new HttpException(
    { errorType: 'CriticalError', message: String(error) },
    500,
  );
}

/** Runs `action`, converting any data-layer error into its HTTP form. */
export async function asHttp<T>(action: () => Promise<T>): Promise<T> {
  try {
    return await action();
  } catch (error) {
    throw toHttpException(error);
  }
}

/**
 * Narrow an optional contract member, or fail with the error a caller of an
 * unsupported route should see.
 *
 * Optional methods and namespaces on `ChainDataApiV1` are how a provider says
 * it cannot serve something: the member is absent, so calling it blindly is a
 * TypeError rather than a rejected promise. Every use of an optional member
 * goes through here, which is what makes the compiler enforce that a new
 * provider gap is handled rather than crashing a route.
 */
export function required<T>(member: T | undefined, route: string): T {
  if (member === undefined) {
    throw new ChainDataError(
      'CAPABILITY_UNSUPPORTED',
      `The configured provider does not serve ${route}.`,
      { retryable: false, details: { route } },
    );
  }
  return member;
}

/**
 * The same, for an optional METHOD: returns the owner with that method
 * narrowed to present, so it is still called as `owner.method(…)` and stays
 * bound to its object.
 */
export function withMethod<T extends object, K extends keyof T>(
  owner: T,
  key: K,
  route: string,
): T & { [P in K]-?: NonNullable<T[P]> } {
  required(owner[key], route);
  return owner as T & { [P in K]-?: NonNullable<T[P]> };
}
