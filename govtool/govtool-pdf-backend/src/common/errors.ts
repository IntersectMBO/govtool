// SPEC §3.6. Throw these from controllers and services; the global
// ApiExceptionFilter renders them as
// {"data": null, "error": {status, name, message, details}}.

export type ErrorName =
  | 'BadRequestError'
  | 'ValidationError'
  | 'ApplicationError'
  | 'UnauthorizedError'
  | 'ForbiddenError'
  | 'NotFoundError'
  | 'PayloadTooLargeError'
  | 'InternalServerError';

export interface ErrorBody {
  data: null;
  error: {
    status: number;
    name: ErrorName;
    message: string;
    details: unknown;
  };
}

export class ApiError extends Error {
  constructor(
    public readonly status: number,
    public readonly errorName: ErrorName,
    message: string,
    public readonly details: unknown = {},
  ) {
    super(message);
    this.name = 'ApiError';
  }

  toBody(): ErrorBody {
    return {
      data: null,
      error: {
        status: this.status,
        name: this.errorName,
        message: this.message,
        details: this.details,
      },
    };
  }
}

/**
 * Raw (non-enveloped) error for the proxy routes of SPEC §9:
 * `{"error": msg, "details": ...}` with any status.
 */
export class RawHttpError extends Error {
  constructor(
    public readonly status: number,
    public readonly body: unknown,
  ) {
    super(`HTTP ${status}`);
    this.name = 'RawHttpError';
  }
}

/** **BD** — Strapi's `ctx.badRequest(null, msg)`: the text goes in `details`. */
export const badRequestDetails = (msg: string) => new ApiError(400, 'BadRequestError', 'Bad Request', msg);
/** **B** */
export const badRequest = (msg: string) => new ApiError(400, 'BadRequestError', msg);
/** **V** */
export const validationError = (msg: string) => new ApiError(400, 'ValidationError', msg);
/** **A** */
export const applicationError = (msg: string) => new ApiError(400, 'ApplicationError', msg);
/** **U** */
export const unauthorized = (msg = 'Missing or invalid credentials') =>
  new ApiError(401, 'UnauthorizedError', msg);
/** **F** */
export const forbidden = (msg = 'Forbidden') => new ApiError(403, 'ForbiddenError', msg);
/** **N** */
export const notFound = (msg = 'Not Found') => new ApiError(404, 'NotFoundError', msg);
export const payloadTooLarge = () => new ApiError(413, 'PayloadTooLargeError', 'Payload Too Large');
export const internal = () => new ApiError(500, 'InternalServerError', 'Internal Server Error');

/** Prisma unique-constraint violation (P2002), without importing the runtime. */
export function isUniqueViolation(err: unknown): boolean {
  return typeof err === 'object' && err !== null && (err as { code?: unknown }).code === 'P2002';
}

/** Prisma "record to update/delete not found" (P2025). */
export function isRecordNotFound(err: unknown): boolean {
  return typeof err === 'object' && err !== null && (err as { code?: unknown }).code === 'P2025';
}
