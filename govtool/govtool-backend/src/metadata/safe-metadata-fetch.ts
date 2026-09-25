import { lookup } from 'node:dns/promises';
import { request as httpRequest } from 'node:http';
import { request as httpsRequest } from 'node:https';
import type { LookupFunction } from 'node:net';
import * as ipaddr from 'ipaddr.js';
import { METADATA_FETCH_LIMIT_BYTES } from './config';
import { MetadataValidationStatus as Status } from './metadata-status.enum';

export class MetadataFetchError extends Error {
  constructor(readonly code: Status) {
    super(code);
  }
}

const TIMEOUT_MS = 10_000;

function blocked(address: string): boolean {
  return (
    ipaddr.isValid(address) && ipaddr.process(address).range() !== 'unicast'
  );
}

// Validate the actual DNS answers used by the socket, not a separate preflight lookup.
export const safeLookup: LookupFunction = (hostname, options, callback) => {
  lookup(hostname, options)
    .then((result) => {
      const addresses = Array.isArray(result) ? result : [result];
      if (addresses.some(({ address }) => blocked(address))) {
        callback(new MetadataFetchError(Status.URL_BLOCKED), '', 0);
      } else if (Array.isArray(result)) {
        callback(null, result);
      } else {
        callback(null, result.address, result.family);
      }
    })
    .catch((error: Error) => callback(error, '', 0));
};

export async function fetchMetadataText(
  url: string,
  headers: Record<string, string>,
): Promise<string> {
  let parsed: URL;
  try {
    parsed = new URL(url);
  } catch {
    throw new MetadataFetchError(Status.URL_NOT_FOUND);
  }
  const hostname = parsed.hostname.replace(/^\[|\]$/g, '').toLowerCase();
  if (
    !['http:', 'https:'].includes(parsed.protocol) ||
    hostname === 'localhost' ||
    hostname.endsWith('.localhost') ||
    blocked(hostname) ||
    parsed.username ||
    parsed.password
  ) {
    throw new MetadataFetchError(Status.URL_BLOCKED);
  }

  return new Promise((resolve, reject) => {
    const request = parsed.protocol === 'https:' ? httpsRequest : httpRequest;
    // Native requests do not follow redirects or read proxy environment variables.
    const req = request(parsed, { headers, lookup: safeLookup, agent: false });
    const timer = setTimeout(
      () => req.destroy(new Error('Metadata request timed out')),
      TIMEOUT_MS,
    );
    const fail = (status: Status) => {
      clearTimeout(timer);
      reject(new MetadataFetchError(status));
    };
    req.on('error', (error: NodeJS.ErrnoException) => {
      fail(
        error.code === Status.URL_BLOCKED
          ? Status.URL_BLOCKED
          : Status.URL_NOT_FOUND,
      );
    });
    req.on('response', (response) => {
      if (response.statusCode! < 200 || response.statusCode! >= 300) {
        fail(
          response.statusCode! >= 300 && response.statusCode! < 400
            ? Status.URL_BLOCKED
            : Status.URL_NOT_FOUND,
        );
        response.destroy();
        return;
      }
      // A declared length over the limit is refused before reading a byte.
      const declared = Number(response.headers?.['content-length']);
      if (Number.isFinite(declared) && declared > METADATA_FETCH_LIMIT_BYTES) {
        fail(Status.EXCEEDS_LIMIT);
        response.destroy();
        req.destroy();
        return;
      }
      let size = 0;
      const chunks: Buffer[] = [];
      response.on('data', (chunk: Buffer) => {
        size += chunk.length;
        if (size > METADATA_FETCH_LIMIT_BYTES) {
          fail(Status.EXCEEDS_LIMIT);
          response.destroy();
          req.destroy();
          return;
        }
        chunks.push(chunk);
      });
      response.on('error', () => fail(Status.URL_NOT_FOUND));
      response.on('aborted', () => fail(Status.URL_NOT_FOUND));
      response.on('end', () => {
        clearTimeout(timer);
        resolve(Buffer.concat(chunks).toString('utf8'));
      });
    });
    req.end();
  });
}
