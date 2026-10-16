import { ArgumentsHost, Catch, ExceptionFilter, HttpException, Logger } from '@nestjs/common';
import type { Response } from 'express';
import {
  ApiError,
  RawHttpError,
  badRequest,
  forbidden,
  internal,
  notFound,
  payloadTooLarge,
  unauthorized,
} from './errors';

/**
 * Maps anything thrown to the SPEC §3.6 body. Nothing internal is echoed
 * (Δ4): unknown errors are logged by class and stack only, never with the
 * request, so no token, cookie or signature reaches the log.
 */
export function toApiError(err: unknown, logger?: Logger): ApiError | RawHttpError {
  if (err instanceof ApiError || err instanceof RawHttpError) return err;
  if (err instanceof HttpException) {
    const status = err.getStatus();
    if (status === 404) return notFound();
    if (status === 413) return payloadTooLarge();
    if (status === 401) return unauthorized();
    if (status === 403) return forbidden();
    if (status === 400) return badRequest('Bad Request');
    if (status < 500) return new ApiError(status, 'BadRequestError', 'Bad Request');
  }
  const bp = bodyParserError(err);
  if (bp) return bp;
  if (logger) {
    const e = err instanceof Error ? err : new Error(String(err));
    logger.error(`${e.name}: ${e.message}`, e.stack);
  }
  return internal();
}

/** body-parser / raw-body failures, recognised by their `type`. */
export function bodyParserError(err: unknown): ApiError | null {
  if (typeof err !== 'object' || err === null) return null;
  const type = (err as { type?: unknown }).type;
  if (type === 'entity.parse.failed') return badRequest('Invalid JSON');
  if (type === 'entity.too.large') return payloadTooLarge();
  if (
    type === 'encoding.unsupported' ||
    type === 'charset.unsupported' ||
    type === 'request.size.invalid' ||
    type === 'request.aborted'
  ) {
    return badRequest('Bad Request');
  }
  return null;
}

export function sendError(res: Response, e: ApiError | RawHttpError): void {
  if (res.headersSent) return;
  if (e instanceof RawHttpError) {
    res.status(e.status).json(e.body);
  } else {
    res.status(e.status).json(e.toBody());
  }
}

@Catch()
export class ApiExceptionFilter implements ExceptionFilter {
  private readonly logger = new Logger('ApiExceptionFilter');

  catch(exception: unknown, host: ArgumentsHost): void {
    const res = host.switchToHttp().getResponse<Response>();
    sendError(res, toApiError(exception, this.logger));
  }
}
