import { HttpException, Logger } from '@nestjs/common';
import {
  PinningError,
  type PinningServiceV1,
} from '@govtool/data-providers/pinning';

import type { ConfigService } from '../config/config.service';
import { validateCip100Document } from './cip100-document';
import { IpfsService } from './ipfs.service';
import { UploadRateLimiter } from './upload-rate-limiter';

const rationale = JSON.stringify(
  {
    '@context': { CIP100: 'https://example.org/CIP-0100#' },
    hashAlgorithm: 'blake2b-256',
    body: { comment: 'I vote yes because…' },
  },
  null,
  2,
);

describe('validateCip100Document (#4171)', () => {
  it('accepts a CIP-100 JSON-LD document', () => {
    expect(validateCip100Document(rationale)).toBeNull();
  });

  it.each([
    ['plain text', 'aida-pentest-noop-do-not-keep'],
    ['a JSON array', '[1,2,3]'],
    ['JSON without @context', '{"hashAlgorithm":"blake2b-256","body":{}}'],
    [
      'a wrong hash algorithm',
      '{"@context":{},"hashAlgorithm":"sha256","body":{}}',
    ],
    ['a missing body', '{"@context":{},"hashAlgorithm":"blake2b-256"}'],
    [
      'non-array authors',
      '{"@context":{},"hashAlgorithm":"blake2b-256","body":{},"authors":"x"}',
    ],
  ])('rejects %s', (_label, content) => {
    expect(validateCip100Document(content)).not.toBeNull();
  });
});

describe('UploadRateLimiter', () => {
  it('limits each client and resets after the window', () => {
    let now = 0;
    const limiter = new UploadRateLimiter(
      {
        perClientLimit: 2,
        globalLimit: 100,
        windowSeconds: 60,
        maxTrackedClients: 10,
      },
      () => now,
    );
    expect(limiter.consume('a').allowed).toBe(true);
    expect(limiter.consume('a').allowed).toBe(true);
    expect(limiter.consume('a')).toEqual({
      allowed: false,
      retryAfterSeconds: 60,
    });
    expect(limiter.consume('b').allowed).toBe(true);
    now = 60_000;
    expect(limiter.consume('a').allowed).toBe(true);
  });

  it('caps total uploads across many clients', () => {
    const limiter = new UploadRateLimiter({
      perClientLimit: 5,
      globalLimit: 3,
      windowSeconds: 60,
      maxTrackedClients: 100,
    });
    const results = ['a', 'b', 'c', 'd'].map(
      (ip) => limiter.consume(ip).allowed,
    );
    expect(results).toEqual([true, true, true, false]);
  });

  it('fails closed when too many distinct clients are tracked', () => {
    const limiter = new UploadRateLimiter({
      perClientLimit: 5,
      globalLimit: 100,
      windowSeconds: 60,
      maxTrackedClients: 2,
    });
    expect(limiter.consume('a').allowed).toBe(true);
    expect(limiter.consume('b').allowed).toBe(true);
    expect(limiter.consume('c').allowed).toBe(false);
    expect(limiter.consume('a').allowed).toBe(true);
  });
});

describe('IpfsService.upload (#4171)', () => {
  let pinData: jest.Mock;

  const makeService = (perClientLimit = 10, pinning: unknown = { pinData }) =>
    new IpfsService(
      pinning as PinningServiceV1 | null,
      {
        get: () => ({
          ipfsUpload: {
            perClientLimit,
            globalLimit: 100,
            windowSeconds: 3600,
          },
        }),
      } as unknown as ConfigService,
    );

  const errorOf = async (promise: Promise<unknown>) => {
    try {
      await promise;
    } catch (error) {
      if (error instanceof HttpException) {
        return { status: error.getStatus(), body: error.getResponse() };
      }
      throw error;
    }
    throw new Error('expected rejection');
  };

  beforeEach(() => {
    jest.spyOn(Logger.prototype, 'warn').mockImplementation(() => undefined);
    jest.spyOn(Logger.prototype, 'error').mockImplementation(() => undefined);
    pinData = jest.fn().mockResolvedValue('bafy');
  });

  afterEach(() => jest.restoreAllMocks());

  it('pins valid CIP-100 documents as received', async () => {
    await expect(makeService().upload(rationale, '1.2.3.4')).resolves.toEqual({
      ipfsCid: 'bafy',
    });
    expect(pinData).toHaveBeenCalledWith(
      Buffer.from(rationale, 'utf8'),
      'govtool-legacy-upload',
    );
  });

  it('rejects arbitrary content without contacting the pinning service', async () => {
    const error = await errorOf(
      makeService().upload('aida-pentest-noop-do-not-keep', '1.2.3.4'),
    );
    expect(error.status).toBe(400);
    expect(error.body).toMatchObject({ errorType: 'ValidationError' });
    expect(pinData).not.toHaveBeenCalled();
  });

  it('returns 429 once a client exceeds its budget', async () => {
    const service = makeService(1);
    await service.upload(rationale, '1.2.3.4');
    const error = await errorOf(service.upload(rationale, '1.2.3.4'));
    expect(error.status).toBe(429);
    expect(error.body).toMatchObject({ errorType: 'RateLimitError' });
    expect(pinData).toHaveBeenCalledTimes(1);
  });

  it('answers 503 when no pinning service is configured', async () => {
    const error = await errorOf(
      makeService(10, null).upload(rationale, '1.2.3.4'),
    );
    expect(error.status).toBe(503);
    expect(error.body).toMatchObject({ errorType: 'IpfsUnconfiguredError' });
  });

  it('does not expose connection details to clients', async () => {
    pinData.mockRejectedValue(
      new PinningError(
        'BACKEND_UNAVAILABLE',
        'TypeError: fetch failed (connect ECONNREFUSED 10.0.0.7:443)',
      ),
    );
    const error = await errorOf(makeService().upload(rationale, '1.2.3.4'));
    expect(error.status).toBe(503);
    expect(error.body).toEqual({
      errorType: 'PinataConenctionError',
      message: 'Failed to connect to Pinata',
    });
  });
});
