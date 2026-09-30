import {
  HttpException,
  HttpStatus,
  Inject,
  Injectable,
  Logger,
} from '@nestjs/common';
import {
  PinningError,
  type PinningServiceV1,
} from '@govtool/data-providers/pinning';

import { asHttp } from 'src/common/errors';
import { ConfigService } from 'src/config/config.service';
import { PINNING } from 'src/providers/providers.module';
import { validateCip100Document } from './cip100-document';
import { UploadResponse } from './ipfs.type';
import { UploadRateLimiter } from './upload-rate-limiter';

/**
 * `pinData` takes the owner a quota is counted against. The legacy route is
 * unauthenticated and carries no wallet identity, so every legacy upload is
 * attributed to the backend itself.
 */
const LEGACY_UPLOAD_OWNER = 'govtool-legacy-upload';

const MAX_TRACKED_UPLOAD_CLIENTS = 10_000;

@Injectable()
export class IpfsService {
  private readonly logger = new Logger(IpfsService.name);
  private readonly rateLimiter: UploadRateLimiter;

  constructor(
    @Inject(PINNING) private readonly pinning: PinningServiceV1 | null,
    config: ConfigService,
  ) {
    this.rateLimiter = new UploadRateLimiter({
      ...config.get().ipfsUpload,
      maxTrackedClients: MAX_TRACKED_UPLOAD_CLIENTS,
    });
  }

  /**
   * The legacy upload endpoint. Size and backend failures are classified by
   * the pinning service and mapped back to the legacy `errorType` names in
   * `src/common/errors.ts`, so the response bodies are unchanged.
   */
  async upload(fileContent: string, clientIp: string): Promise<UploadResponse> {
    const validationError = validateCip100Document(fileContent);
    if (validationError) {
      throw new HttpException(
        { errorType: 'ValidationError', message: validationError },
        HttpStatus.BAD_REQUEST,
      );
    }

    if (this.pinning === null) {
      throw new HttpException(
        {
          errorType: 'IpfsUnconfiguredError',
          message: 'Backend is not configured for ipfs upload',
        },
        503,
      );
    }

    // Only well-formed requests consume the budget, so a client cannot burn
    // it with junk, and junk never reaches the pinning service.
    const decision = this.rateLimiter.consume(clientIp);
    if (!decision.allowed) {
      this.logger.warn(`IPFS upload rate limit hit for client ${clientIp}`);
      throw new HttpException(
        {
          errorType: 'RateLimitError',
          message: 'Too many uploads, please try again later',
          retryAfterSeconds: decision.retryAfterSeconds,
        },
        HttpStatus.TOO_MANY_REQUESTS,
      );
    }

    const pinning = this.pinning;
    return asHttp(async () => {
      // The on-chain hash is over these bytes, so they are pinned exactly as
      // received.
      try {
        const cid = await pinning.pinData(
          Buffer.from(fileContent, 'utf8'),
          LEGACY_UPLOAD_OWNER,
        );
        return { ipfsCid: cid };
      } catch (error) {
        throw this.withoutConnectionDetail(error);
      }
    });
  }

  /** A connection failure's text can name internal hosts; keep it server-side. */
  private withoutConnectionDetail(error: unknown): unknown {
    if (
      !PinningError.is(error) ||
      (error.reason !== 'BACKEND_UNAVAILABLE' &&
        error.reason !== 'BACKEND_TIMEOUT')
    ) {
      return error;
    }
    this.logger.error(`Failed to reach the pinning service: ${error.message}`);
    return new PinningError(error.reason, 'Failed to connect to Pinata');
  }
}
