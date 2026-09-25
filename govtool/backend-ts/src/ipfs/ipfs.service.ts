import { HttpException, HttpStatus, Injectable, Logger } from '@nestjs/common';

import { ConfigService } from 'src/config/config.service';
import { PinataUploadResponse, UploadResponse } from './ipfs.type';
import {
  sanitizeUploadFileName,
  validateCip100Document,
} from './cip100-document';
import { UploadRateLimiter } from './upload-rate-limiter';

const MAX_TRACKED_UPLOAD_CLIENTS = 10_000;

@Injectable()
export class IpfsService {
  private readonly logger = new Logger(IpfsService.name);
  private readonly rateLimiter: UploadRateLimiter;

  constructor(private readonly configSerivce: ConfigService) {
    const { ipfsUpload } = this.configSerivce.get();
    this.rateLimiter = new UploadRateLimiter({
      ...ipfsUpload,
      maxTrackedClients: MAX_TRACKED_UPLOAD_CLIENTS,
    });
  }

  async upload(
    requestedFileName: string | undefined,
    fileContent: string,
    clientIp: string,
  ): Promise<UploadResponse> {
    const size = Buffer.byteLength(fileContent, 'utf8');

    if (size > 1024 * 512) {
      throw new HttpException(
        {
          errorType: 'ValidationError',
          message: 'The uploaded file is larger than 500kb',
        },
        400,
      );
    }

    const validationError = validateCip100Document(fileContent);
    if (validationError) {
      throw new HttpException(
        { errorType: 'ValidationError', message: validationError },
        400,
      );
    }

    const jwt = this.configSerivce.get().pinataApiJwt;

    if (!jwt) {
      throw new HttpException(
        {
          errorType: 'IpfsUnconfiguredError',
          message: 'Backend is not configured for ipfs upload',
        },
        503,
      );
    }

    // Only well-formed requests consume the budget, so a client cannot burn
    // it with junk, and junk never reaches Pinata.
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

    const fileName = sanitizeUploadFileName(requestedFileName);
    const formData = new FormData();
    formData.append('network', 'public');
    formData.append(
      'file',
      new Blob([fileContent], { type: 'text/plain' }),
      fileName,
    );

    let response: Response;

    try {
      response = await fetch('https://upload.pinata.cloud/v3/files', {
        method: 'POST',
        headers: {
          Authorization: `Bearer ${jwt}`,
        },
        body: formData,
      });
    } catch (error) {
      this.logger.error('Failed to connect to Pinata', String(error));
      throw new HttpException(
        {
          errorType: 'PinataConenctionError',
          message: 'Failed to connect to Pinata',
        },
        503,
      );
    }

    const responseText = await response.text();

    if (!response.ok) {
      // Pinata's body can describe Intersect's account; keep it server-side.
      this.logger.error(
        `Pinata upload failed with status ${response.status}: ${responseText.slice(0, 1_000)}`,
      );
      throw new HttpException(
        {
          errorType: 'PinataAPIError',
          message: `Pinata API returned error status : ${response.status}`,
          pinataResponse: { status: String(response.status) },
        },
        503,
      );
    }

    let parsed: PinataUploadResponse;

    try {
      parsed = JSON.parse(responseText) as PinataUploadResponse;
    } catch {
      throw new HttpException(
        {
          errorType: 'PinataDecodingError',
          message: 'Failed to decode Pinata API reponse',
          pinataResponse: { status: 'unknown' },
        },
        503,
      );
    }

    if (!parsed.data?.cid) {
      throw new HttpException(
        {
          errorType: 'PinataDecodingError',
          message: 'Failed to decode Pinata API reponse',
          pinataResponse: { status: 'unknown' },
        },
        503,
      );
    }

    return {
      ipfsCid: parsed.data.cid,
    };
  }
}
