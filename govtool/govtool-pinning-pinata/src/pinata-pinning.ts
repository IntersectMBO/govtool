import type { Hex } from '@govtool/data-providers/metadata';
import {
  PinningError,
  type Cid,
  type PinBackendHealth,
  type PinBackendId,
  type PinningPolicy,
  type PinningServiceV1,
  type PinRecord,
  type PinRequest,
} from '@govtool/data-providers/pinning';

import { blake2b256Hex } from './hash';

export interface PinataPinningOptions {
  /** Pinata API JWT. Required; the constructor refuses an empty one. */
  jwt: string;
  /** Pinata v3 upload endpoint. */
  uploadUrl?: string;
  /** Endpoint used by `getHealth()` to check the JWT is accepted. */
  authCheckUrl?: string;
  /** Pinata network the file is pinned to. GovTool metadata must be public. */
  network?: 'public' | 'private';
  /** Byte cap enforced before any request leaves. Default: 512 KiB, GovTool's long-standing limit. */
  maxBytes?: number;
  /** Content types the policy admits. Default: the contract's three. */
  allowedContentTypes?: string[];
  /** Gateway base urls reported on `PinRecord.gatewayUrls`, for display only. */
  gatewayBaseUrls?: string[];
  /** Injected for tests. Defaults to the global `fetch`. */
  fetch?: typeof fetch;
  /** Injected for tests. */
  now?: () => Date;
}

export const PINATA_BACKEND_ID: PinBackendId = 'pinata';
export const DEFAULT_UPLOAD_URL = 'https://upload.pinata.cloud/v3/files';
export const DEFAULT_AUTH_CHECK_URL =
  'https://api.pinata.cloud/data/testAuthentication';
export const DEFAULT_MAX_BYTES = 512 * 1024;
export const DEFAULT_ALLOWED_CONTENT_TYPES = [
  'application/ld+json',
  'application/json',
  'text/plain',
];
export const DEFAULT_GATEWAY_BASE_URLS = ['https://ipfs.io'];
/** What the legacy upload endpoint used when the caller named no file. */
export const DEFAULT_FILE_NAME = 'data.txt';

interface PinataUploadResponse {
  data?: { cid?: unknown };
}

/**
 * `PinningServiceV1` over Pinata's v3 upload API.
 *
 * `pin()` is a faithful port of the legacy backend's IPFS upload: the same
 * endpoint, the same multipart shape (`network` + `file`), the same byte cap
 * and the same four failure classes, so a consumer that mapped those onto
 * HTTP responses before can keep doing so — see `PinFailureReason`.
 *
 * `prepare()`, `getPolicy()` and `getHealth()` are implemented locally.
 * `getPin()`, `listPins()`, `repin()` and `unpin()` are not implemented and
 * throw `UNSUPPORTED_OPERATION`: GovTool never needed them, and shipping an
 * unverified call to Pinata's file-management API would be worse than an
 * honest gap.
 *
 * CIP validation of the document (`PinRequest.standard`) is out of scope here
 * and is not performed; `prepare().valid` reflects the size and content-type
 * policy only. Validation belongs to the metadata service.
 */
export class PinataPinningService implements PinningServiceV1 {
  private readonly jwt: string;
  private readonly uploadUrl: string;
  private readonly authCheckUrl: string;
  private readonly network: 'public' | 'private';
  private readonly maxBytes: number;
  private readonly allowedContentTypes: readonly string[];
  private readonly gatewayBaseUrls: readonly string[];
  private readonly fetchImpl: typeof fetch;
  private readonly now: () => Date;
  private lastSuccessAt: string | undefined;

  constructor(options: PinataPinningOptions) {
    if (!options.jwt || options.jwt.trim() === '') {
      throw new Error('PinataPinningService requires a non-empty jwt');
    }
    this.jwt = options.jwt;
    this.uploadUrl = options.uploadUrl ?? DEFAULT_UPLOAD_URL;
    this.authCheckUrl = options.authCheckUrl ?? DEFAULT_AUTH_CHECK_URL;
    this.network = options.network ?? 'public';
    this.maxBytes = options.maxBytes ?? DEFAULT_MAX_BYTES;
    this.allowedContentTypes =
      options.allowedContentTypes ?? DEFAULT_ALLOWED_CONTENT_TYPES;
    this.gatewayBaseUrls = options.gatewayBaseUrls ?? DEFAULT_GATEWAY_BASE_URLS;
    this.fetchImpl = options.fetch ?? fetch;
    this.now = options.now ?? (() => new Date());
  }

  async pin(req: PinRequest): Promise<PinRecord> {
    const byteSize = Buffer.byteLength(req.content, 'utf8');
    this.assertWithinPolicy(byteSize, req.contentType);
    const dataHash = blake2b256Hex(req.content);

    const formData = new FormData();
    formData.append('network', this.network);
    formData.append(
      'file',
      new Blob([req.content], { type: req.contentType }),
      req.fileName ?? DEFAULT_FILE_NAME,
    );

    let response: Response;
    try {
      response = await this.fetchImpl(this.uploadUrl, {
        method: 'POST',
        headers: { Authorization: `Bearer ${this.jwt}` },
        body: formData,
      });
    } catch (error) {
      throw new PinningError('BACKEND_UNAVAILABLE', String(error), {
        cause: error,
        details: { message: String(error) },
      });
    }

    const responseText = await response.text();

    if (!response.ok) {
      throw new PinningError(
        'BACKEND_ERROR',
        `Pinata API returned error status : ${response.status}`,
        { details: { status: response.status, body: responseText } },
      );
    }

    const cid = this.extractCid(responseText, response.status);
    const pinnedAt = this.now().toISOString();
    this.lastSuccessAt = pinnedAt;

    return {
      cid,
      url: `ipfs://${cid}`,
      gatewayUrls: this.gatewayBaseUrls.map(
        (base) => `${base.replace(/\/$/, '')}/ipfs/${cid}`,
      ),
      dataHash,
      byteSize,
      contentType: req.contentType,
      status: 'pinned',
      pinnedAt,
      replicas: [{ backend: PINATA_BACKEND_ID, status: 'pinned', pinnedAt }],
    };
  }

  async prepare(req: Omit<PinRequest, 'purpose'>): Promise<{
    dataHash: Hex;
    byteSize: number;
    valid: boolean;
    errors?: string[];
  }> {
    const byteSize = Buffer.byteLength(req.content, 'utf8');
    const errors: string[] = [];
    if (byteSize > this.maxBytes) {
      errors.push(
        `content is ${byteSize} bytes; the limit is ${this.maxBytes}`,
      );
    }
    if (!this.allowedContentTypes.includes(req.contentType)) {
      errors.push(`content type ${req.contentType} is not accepted`);
    }
    const result: Awaited<ReturnType<PinningServiceV1['prepare']>> = {
      dataHash: blake2b256Hex(req.content),
      byteSize,
      valid: errors.length === 0,
    };
    if (errors.length > 0) {
      result.errors = errors;
    }
    return result;
  }

  getPin(cid: Cid): Promise<PinRecord> {
    return Promise.reject(this.unsupported('getPin', { cid }));
  }

  listPins(): Promise<{ elements: PinRecord[]; nextCursor: string | null }> {
    return Promise.reject(this.unsupported('listPins'));
  }

  repin(cid: Cid): Promise<PinRecord> {
    return Promise.reject(this.unsupported('repin', { cid }));
  }

  unpin(cid: Cid): Promise<{ cid: Cid; status: 'unpinned' }> {
    return Promise.reject(this.unsupported('unpin', { cid }));
  }

  getPolicy(): Promise<PinningPolicy> {
    return Promise.resolve({
      maxBytes: this.maxBytes,
      allowedContentTypes: [...this.allowedContentTypes],
      retentionDays: null,
      // GovTool's upload path has always been open; authentication, if any,
      // is the deployment's concern in front of this service.
      requiresAuth: false,
    });
  }

  /**
   * Asks Pinata whether the JWT is accepted. `healthy` on 200, `degraded` on
   * any other status (the backend is up but this credential is not), and
   * `unavailable` when it cannot be reached.
   */
  async getHealth(): Promise<PinBackendHealth[]> {
    const base: PinBackendHealth = {
      backend: PINATA_BACKEND_ID,
      status: 'healthy',
    };
    if (this.lastSuccessAt !== undefined) {
      base.lastSuccessAt = this.lastSuccessAt;
    }
    try {
      const response = await this.fetchImpl(this.authCheckUrl, {
        method: 'GET',
        headers: { Authorization: `Bearer ${this.jwt}` },
      });
      if (response.ok) {
        return [base];
      }
      return [
        {
          ...base,
          status: 'degraded',
          message: `Pinata authentication check returned ${response.status}`,
        },
      ];
    } catch (error) {
      return [{ ...base, status: 'unavailable', message: String(error) }];
    }
  }

  private assertWithinPolicy(byteSize: number, contentType: string): void {
    if (byteSize > this.maxBytes) {
      throw new PinningError(
        'TOO_LARGE',
        `content is ${byteSize} bytes; the limit is ${this.maxBytes}`,
        { details: { byteSize, maxBytes: this.maxBytes } },
      );
    }
    if (!this.allowedContentTypes.includes(contentType)) {
      throw new PinningError(
        'UNSUPPORTED_CONTENT_TYPE',
        `content type ${contentType} is not accepted`,
        { details: { contentType } },
      );
    }
  }

  private extractCid(responseText: string, status: number): string {
    let parsed: PinataUploadResponse;
    try {
      parsed = JSON.parse(responseText) as PinataUploadResponse;
    } catch {
      throw this.invalidResponse(responseText, status);
    }
    const cid = parsed.data?.cid;
    if (typeof cid !== 'string' || cid === '') {
      throw this.invalidResponse(responseText, status);
    }
    return cid;
  }

  private invalidResponse(body: string, status: number): PinningError {
    return new PinningError(
      'BACKEND_INVALID_RESPONSE',
      'Failed to decode Pinata API response',
      { details: { status, body } },
    );
  }

  private unsupported(
    operation: string,
    details: Record<string, unknown> = {},
  ): PinningError {
    return new PinningError(
      'UNSUPPORTED_OPERATION',
      `Pinata pinning service does not implement ${operation}`,
      { details: { operation, ...details } },
    );
  }
}

export function createPinataPinning(
  options: PinataPinningOptions,
): PinningServiceV1 {
  return new PinataPinningService(options);
}
