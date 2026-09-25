import {
  ArgumentsHost,
  Catch,
  HttpException,
  HttpStatus,
  Logger,
} from '@nestjs/common';
import * as Sentry from '@sentry/nestjs';
import { SentryGlobalFilter } from '@sentry/nestjs/setup';
import type { Request } from 'express';

const MAX_LOGGED_RESPONSE_LENGTH = 1_000;
const FIRST_SERVER_ERROR_STATUS: number = HttpStatus.INTERNAL_SERVER_ERROR;

/**
  * Nest's base filter only logs non-HTTP exceptions, and Sentry treats every
 * HttpException as "expected". Without this filter, 5xx HttpExceptions (for
 * example upstream Pinata failures or critical data errors) would be sent to
 * the client and nowhere else.
 */
@Catch()
export class LoggingExceptionFilter extends SentryGlobalFilter {
  private readonly errorLogger = new Logger('ExceptionsHandler');

  catch(exception: unknown, host: ArgumentsHost): void {
    if (
      host.getType() === 'http' &&
      exception instanceof HttpException &&
      exception.getStatus() >= FIRST_SERVER_ERROR_STATUS
    ) {
      const request = host.switchToHttp().getRequest<Request>();

      this.errorLogger.error(
        `${request.method} ${request.path} responded ${exception.getStatus()}: ${describeResponse(exception)}`,
        exception.stack,
      );
      Sentry.captureException(exception);
    }

    super.catch(exception, host);
  }
}

function describeResponse(exception: HttpException): string {
  const response = exception.getResponse();
  const text =
    typeof response === 'string' ? response : JSON.stringify(response);

  return text.length > MAX_LOGGED_RESPONSE_LENGTH
    ? `${text.slice(0, MAX_LOGGED_RESPONSE_LENGTH)}…`
    : text;
}
