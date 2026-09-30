import {
  PinningError,
  type Cid,
  type PinBackendHealth,
  type PinFailureReason,
  type PinningServiceV1,
} from '@govtool/data-providers/pinning';

import { rawBlockCid } from './cid';

export interface TestPinningOptions {
  /**
   * Root url of the test metadata service (tests/test-metadata-api), such as
   * http://test-metadata-api:3000. Required; the constructor refuses an
   * empty or non-http(s) one.
   */
  baseUrl: string;
  /** Byte cap enforced before any request leaves. Default: 512 KiB, as Pinata. */
  maxBytes?: number;
  /** Per-request timeout in milliseconds. */
  timeoutMs?: number;
  /** Injected for tests. Defaults to the global `fetch`. */
  fetch?: typeof fetch;
}

export const DEFAULT_MAX_BYTES = 512 * 1024;
export const DEFAULT_TIMEOUT_MS = 10_000;

/**
 * `PinningServiceV1` over the test metadata service's `/ipfs` routes, so an
 * isolated test environment pins and reads without Pinata or any public
 * gateway.
 *
 * The service names content by the same CID IPFS assigns a single raw block
 * (`bafkrei…`), which is what Pinata returns for GovTool metadata too; the CID
 * it answers is checked against the one computed here, so a pin can never
 * return a CID that does not name the bytes sent.
 */
export class TestPinningService implements PinningServiceV1 {
  private readonly baseUrl: string;
  private readonly maxBytes: number;
  private readonly timeoutMs: number;
  private readonly fetchImpl: typeof fetch;

  constructor(options: TestPinningOptions) {
    const baseUrl = (options.baseUrl ?? '').trim().replace(/\/+$/, '');
    let protocol: string;
    try {
      protocol = new URL(baseUrl).protocol;
    } catch {
      throw new Error('TestPinningService requires a valid baseUrl');
    }
    if (protocol !== 'http:' && protocol !== 'https:') {
      throw new Error('TestPinningService requires an http(s) baseUrl');
    }
    this.baseUrl = baseUrl;
    this.maxBytes = options.maxBytes ?? DEFAULT_MAX_BYTES;
    this.timeoutMs = options.timeoutMs ?? DEFAULT_TIMEOUT_MS;
    this.fetchImpl = options.fetch ?? fetch;
  }

  async pinData(data: Uint8Array, owner: string): Promise<Cid> {
    void owner;
    if (data.byteLength > this.maxBytes) {
      throw new PinningError(
        'TOO_LARGE',
        `content is ${data.byteLength} bytes; the limit is ${this.maxBytes}`,
      );
    }

    const response = await this.request(`${this.baseUrl}/ipfs`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/octet-stream' },
      body: new Blob([data]),
    });
    const responseText = await response.text();
    if (!response.ok) {
      throw new PinningError(
        reasonForStatus(response.status),
        `Test pinning service returned error status : ${response.status}`,
      );
    }

    const cid = extractCid(responseText);
    const expected = rawBlockCid(data);
    if (cid !== expected) {
      throw new PinningError(
        'BACKEND_ERROR',
        `Test pinning service answered ${cid} for content whose CID is ${expected}`,
      );
    }
    return cid;
  }

  /** Computed locally, exactly as the test service computes it. */
  getDataCid(data: Uint8Array): Promise<Cid> {
    return Promise.resolve(rawBlockCid(data));
  }

  async unpin(cid: Cid): Promise<void> {
    const response = await this.request(
      `${this.baseUrl}/ipfs/${encodeURIComponent(cid)}`,
      { method: 'DELETE' },
    );
    if (!response.ok) {
      throw new PinningError(
        reasonForStatus(response.status),
        `Test pinning service returned error status : ${response.status}`,
      );
    }
  }

  async fetch(cid: Cid): Promise<Uint8Array> {
    const response = await this.request(
      `${this.baseUrl}/ipfs/${encodeURIComponent(cid)}`,
      { method: 'GET' },
    );
    if (!response.ok) {
      throw new PinningError(
        reasonForStatus(response.status),
        `Test pinning service returned error status : ${response.status}`,
      );
    }
    return new Uint8Array(await response.arrayBuffer());
  }

  async getHealth(): Promise<PinBackendHealth> {
    try {
      const response = await this.request(`${this.baseUrl}/ipfs`, {
        method: 'GET',
      });
      return response.ok
        ? { status: 'healthy' }
        : {
            status: 'degraded',
            message: `Test pinning service health check returned ${response.status}`,
          };
    } catch (error) {
      return {
        status: 'unavailable',
        message: error instanceof Error ? error.message : String(error),
      };
    }
  }

  private async request(url: string, init: RequestInit): Promise<Response> {
    try {
      return await this.fetchImpl(url, {
        ...init,
        signal: AbortSignal.timeout(this.timeoutMs),
      });
    } catch (error) {
      const name = (error as { name?: unknown } | null)?.name;
      const timedOut = name === 'TimeoutError' || name === 'AbortError';
      throw new PinningError(
        timedOut ? 'BACKEND_TIMEOUT' : 'BACKEND_UNAVAILABLE',
        String(error),
        { cause: error },
      );
    }
  }
}

function reasonForStatus(status: number): PinFailureReason {
  if (status === 413) return 'TOO_LARGE';
  if (status === 429) return 'RATE_LIMITED';
  if (status === 504) return 'BACKEND_TIMEOUT';
  if (status === 502 || status === 503) return 'BACKEND_UNAVAILABLE';
  return 'BACKEND_ERROR';
}

function extractCid(responseText: string): Cid {
  let parsed: { cid?: unknown };
  try {
    parsed = JSON.parse(responseText) as { cid?: unknown };
  } catch {
    throw new PinningError(
      'BACKEND_ERROR',
      'Failed to decode test pinning service response',
    );
  }
  if (typeof parsed?.cid !== 'string' || parsed.cid === '') {
    throw new PinningError(
      'BACKEND_ERROR',
      'Failed to decode test pinning service response',
    );
  }
  return parsed.cid;
}

export function createTestPinning(
  options: TestPinningOptions,
): PinningServiceV1 {
  return new TestPinningService(options);
}
