import { HttpException, Logger } from '@nestjs/common';
import { IpfsService } from './ipfs.service';
import type { ConfigService } from '../config/config.service';
import {
  sanitizeUploadFileName,
  validateCip100Document,
} from './cip100-document';
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

describe('sanitizeUploadFileName', () => {
  it.each([
    [undefined, 'data.txt'],
    ['', 'data.txt'],
    ['voteContext.jsonld', 'voteContext.jsonld'],
    ['../../etc/passwd', 'etc_passwd'],
    ['<script>.html', 'script_.html'],
    ['a'.repeat(300), 'a'.repeat(100)],
  ])('%s -> %s', (input, expected) => {
    expect(sanitizeUploadFileName(input)).toBe(expected);
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
  let fetchMock: jest.SpyInstance;
  let service: IpfsService;

  const makeService = (perClientLimit = 10) =>
    new IpfsService({
      get: () => ({
        pinataApiJwt: 'jwt',
        ipfsUpload: { perClientLimit, globalLimit: 100, windowSeconds: 3600 },
      }),
    } as unknown as ConfigService);

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
    fetchMock = jest.spyOn(global, 'fetch').mockResolvedValue(
      new Response(JSON.stringify({ data: { cid: 'bafy' } }), {
        status: 200,
      }),
    );
    service = makeService();
  });

  afterEach(() => jest.restoreAllMocks());

  it('pins valid CIP-100 documents', async () => {
    await expect(
      service.upload(undefined, rationale, '1.2.3.4'),
    ).resolves.toEqual({
      ipfsCid: 'bafy',
    });
    expect(fetchMock).toHaveBeenCalledTimes(1);
  });

  it('rejects arbitrary content without contacting Pinata', async () => {
    const error = await errorOf(
      service.upload('poc.txt', 'aida-pentest-noop-do-not-keep', '1.2.3.4'),
    );
    expect(error.status).toBe(400);
    expect(fetchMock).not.toHaveBeenCalled();
  });

  it('returns 429 once a client exceeds its budget', async () => {
    service = makeService(1);
    await service.upload(undefined, rationale, '1.2.3.4');
    const error = await errorOf(
      service.upload(undefined, rationale, '1.2.3.4'),
    );
    expect(error.status).toBe(429);
    expect(error.body).toMatchObject({ errorType: 'RateLimitError' });
    expect(fetchMock).toHaveBeenCalledTimes(1);
  });

  it('does not expose the Pinata response body to clients', async () => {
    fetchMock.mockResolvedValue(
      new Response('{"error":"account details"}', { status: 401 }),
    );
    const error = await errorOf(
      service.upload(undefined, rationale, '1.2.3.4'),
    );
    expect(error.status).toBe(503);
    expect(JSON.stringify(error.body)).not.toContain('account details');
  });
});
