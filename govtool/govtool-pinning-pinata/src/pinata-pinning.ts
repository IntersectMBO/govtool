import {
  PinningError,
  type Cid,
  type PinBackendHealth,
  type PinFailureReason,
  type PinningServiceV1,
} from '@govtool/data-providers/pinning';

import { rawBlockCid, SINGLE_BLOCK_MAX_BYTES } from './cid';

export interface PinataPinningOptions {
  /** Pinata API JWT. Required; the constructor refuses an empty one. */
  jwt: string;
  /** Pinata v3 upload endpoint. */
  uploadUrl?: string;
  /** Endpoint used by `getHealth()` to check the JWT is accepted. */
  authCheckUrl?: string;
  /** Gateway `fetch()` reads through. */
  gatewayUrl?: string;
  /** Pinata network the file is pinned to. GovTool metadata must be public. */
  network?: 'public' | 'private';
  /** Byte cap enforced before any request leaves. Default: 512 KiB, GovTool's long-standing limit. */
  maxBytes?: number;
  /** Per-request timeout in milliseconds. */
  timeoutMs?: number;
  /** Injected for tests. Defaults to the global `fetch`. */
  fetch?: typeof fetch;
}

export const DEFAULT_UPLOAD_URL = 'https://upload.pinata.cloud/v3/files';
export const DEFAULT_AUTH_CHECK_URL =
  'https://api.pinata.cloud/data/testAuthentication';
/** Pinata's own public gateway; ipfs.io was retired on 2026-09-21. */
export const DEFAULT_GATEWAY_URL = 'https://gateway.pinata.cloud';
export const DEFAULT_MAX_BYTES = 512 * 1024;
export const DEFAULT_TIMEOUT_MS = 60_000;
/** What the legacy upload endpoint named every file. */
export const DEFAULT_FILE_NAME = 'data.txt';

interface PinataUploadResponse {
  data?: { cid?: unknown };
}

/**
 * `PinningServiceV1` over Pinata's v3 upload API.
 *
 * `pinData()` is a faithful port of the legacy backend's IPFS upload: the same
 * endpoint, the same multipart shape (`network` + `file` named `data.txt`, as
 * `text/plain`), the same byte cap and the same error messages.
 *
 * `owner` is accepted and not sent: the legacy upload never sent one, and an
 * unverified call to Pinata's metadata fields would be worse than the gap.
 * `unpin()` is not possible through the upload API, because Pinata deletes by
 * its own file id rather than by CID, so it rejects with a message saying so.
 */
export class PinataPinningService implements PinningServiceV1 {
  private readonly jwt: string;
  private readonly uploadUrl: string;
  private readonly authCheckUrl: string;
  private readonly gatewayUrl: string;
  private readonly network: 'public' | 'private';
  private readonly maxBytes: number;
  private readonly timeoutMs: number;
  private readonly fetchImpl: typeof fetch;

  constructor(options: PinataPinningOptions) {
    if (!options.jwt || options.jwt.trim() === '') {
      throw new Error('PinataPinningService requires a non-empty jwt');
    }
    this.jwt = options.jwt;
    this.uploadUrl = options.uploadUrl ?? DEFAULT_UPLOAD_URL;
    this.authCheckUrl = options.authCheckUrl ?? DEFAULT_AUTH_CHECK_URL;
    this.gatewayUrl = (options.gatewayUrl ?? DEFAULT_GATEWAY_URL).replace(
      /\/+$/,
      '',
    );
    this.network = options.network ?? 'public';
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

    const formData = new FormData();
    formData.append('network', this.network);
    formData.append(
      'file',
      new Blob([data], { type: 'text/plain' }),
      DEFAULT_FILE_NAME,
    );

    const response = await this.request(this.uploadUrl, {
      method: 'POST',
      headers: { Authorization: `Bearer ${this.jwt}` },
      body: formData,
    });
    const responseText = await response.text();

    if (!response.ok) {
      throw new PinningError(
        reasonForStatus(response.status),
        `Pinata API returned error status : ${response.status}`,
      );
    }
    return extractCid(responseText);
  }

  /**
   * Computed locally: nothing is sent. Exact for content up to one IPFS block,
   * where the CID is the `raw` block's. Larger content would be chunked by the
   * backend in ways this service cannot reproduce, so it is refused rather
   * than answered with a CID that might not match.
   */
  getDataCid(data: Uint8Array): Promise<Cid> {
    if (data.byteLength > SINGLE_BLOCK_MAX_BYTES) {
      return Promise.reject(
        new PinningError(
          'TOO_LARGE',
          `a CID can be computed locally only for content up to ${SINGLE_BLOCK_MAX_BYTES} bytes; this is ${data.byteLength}`,
        ),
      );
    }
    return Promise.resolve(rawBlockCid(data));
  }

  unpin(cid: Cid): Promise<void> {
    return Promise.reject(
      new PinningError(
        'BACKEND_ERROR',
        `Pinata unpins by its own file id, not by CID, so ${cid} cannot be unpinned through this service`,
      ),
    );
  }

  async fetch(cid: Cid): Promise<Uint8Array> {
    const response = await this.request(
      `${this.gatewayUrl}/ipfs/${encodeURIComponent(cid)}`,
      { method: 'GET' },
    );
    if (!response.ok) {
      throw new PinningError(
        reasonForStatus(response.status),
        `Pinata gateway returned error status : ${response.status}`,
      );
    }
    return new Uint8Array(await response.arrayBuffer());
  }

  /**
   * Asks Pinata whether the JWT is accepted. `healthy` on 200, `degraded` on
   * any other status (the backend is up but this credential is not), and
   * `unavailable` when it cannot be reached.
   */
  async getHealth(): Promise<PinBackendHealth> {
    try {
      const response = await this.request(this.authCheckUrl, {
        method: 'GET',
        headers: { Authorization: `Bearer ${this.jwt}` },
      });
      return response.ok
        ? { status: 'healthy' }
        : {
            status: 'degraded',
            message: `Pinata authentication check returned ${response.status}`,
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
      // AbortSignal.timeout rejects with a DOMException, which is not always
      // an `instanceof Error` across realms, so the name is what is checked.
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
  let parsed: PinataUploadResponse;
  try {
    parsed = JSON.parse(responseText) as PinataUploadResponse;
  } catch {
    throw new PinningError(
      'BACKEND_ERROR',
      'Failed to decode Pinata API response',
    );
  }
  const cid = parsed.data?.cid;
  if (typeof cid !== 'string' || cid === '') {
    throw new PinningError(
      'BACKEND_ERROR',
      'Failed to decode Pinata API response',
    );
  }
  return cid;
}

export function createPinataPinning(
  options: PinataPinningOptions,
): PinningServiceV1 {
  return new PinataPinningService(options);
}
