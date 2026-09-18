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
  BACKEND_INVALID_RESPONSE: 503,
  UNSUPPORTED_OPERATION: 501,
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
  BACKEND_INVALID_RESPONSE: 'PinataDecodingError',
  UNSUPPORTED_OPERATION: 'NotImplementedError',
};

/** `details` is `Record<string, unknown>`; only primitives are safe to stringify. */
function scalarOr(value: unknown, fallback: string): string {
  if (typeof value === 'string') return value;
  if (typeof value === 'number' || typeof value === 'boolean') {
    return String(value);
  }
  return fallback;
}

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
    const body: Record<string, unknown> = {
      errorType: PINNING_ERROR_TYPE[error.reason],
      message: error.message,
    };
    // The legacy upload endpoint nested Pinata's own status and body here.
    if (error.details) {
      body.pinataResponse = {
        status: scalarOr(error.details.status, 'unknown'),
        body: scalarOr(error.details.body, ''),
      };
    }
    return new HttpException(body, PINNING_STATUS[error.reason]);
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
