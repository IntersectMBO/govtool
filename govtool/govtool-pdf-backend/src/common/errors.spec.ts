import { NotFoundException, PayloadTooLargeException } from '@nestjs/common';
import { bodyParserError, toApiError } from './api-exception.filter';
import {
  ApiError,
  applicationError,
  badRequest,
  badRequestDetails,
  forbidden,
  internal,
  notFound,
  payloadTooLarge,
  unauthorized,
  validationError,
} from './errors';

const body = (e: ApiError) => e.toBody();

describe('error helpers (§3.6)', () => {
  it.each([
    [badRequestDetails('Proposal not found'), 400, 'BadRequestError', 'Bad Request', 'Proposal not found'],
    [badRequest('No Authorization'), 400, 'BadRequestError', 'No Authorization', {}],
    [validationError('Invalid key x'), 400, 'ValidationError', 'Invalid key x', {}],
    [applicationError('Verification failed'), 400, 'ApplicationError', 'Verification failed', {}],
    [unauthorized(), 401, 'UnauthorizedError', 'Missing or invalid credentials', {}],
    [unauthorized('Nope'), 401, 'UnauthorizedError', 'Nope', {}],
    [forbidden(), 403, 'ForbiddenError', 'Forbidden', {}],
    [forbidden("You can't access this entry"), 403, 'ForbiddenError', "You can't access this entry", {}],
    [notFound(), 404, 'NotFoundError', 'Not Found', {}],
    [payloadTooLarge(), 413, 'PayloadTooLargeError', 'Payload Too Large', {}],
    [internal(), 500, 'InternalServerError', 'Internal Server Error', {}],
  ])('%#', (e, status, name, message, details) => {
    expect(body(e)).toEqual({ data: null, error: { status, name, message, details } });
    expect(e.status).toBe(status);
  });
});

describe('toApiError', () => {
  it('maps Nest exceptions and body-parser errors', () => {
    expect(body(toApiError(new NotFoundException()) as ApiError)).toMatchObject({
      error: { name: 'NotFoundError' },
    });
    expect(body(toApiError(new PayloadTooLargeException()) as ApiError)).toMatchObject({
      error: { status: 413 },
    });
    expect(bodyParserError({ type: 'entity.parse.failed' })?.message).toBe('Invalid JSON');
    expect(bodyParserError({ type: 'entity.too.large' })?.status).toBe(413);
  });

  it('never echoes internal messages (Δ4)', () => {
    const e = toApiError(new Error('password=hunter2 at db')) as ApiError;
    expect(body(e)).toEqual({
      data: null,
      error: { status: 500, name: 'InternalServerError', message: 'Internal Server Error', details: {} },
    });
    // A Prisma-like error too.
    expect((toApiError(Object.assign(new Error('x'), { code: 'P2002' })) as ApiError).status).toBe(500);
  });
});
