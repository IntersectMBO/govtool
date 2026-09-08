import { Injectable, Logger } from '@nestjs/common';
import { catchError, finalize, firstValueFrom } from 'rxjs';
import { HttpService } from '@nestjs/axios';
import * as blake from 'blakejs';
import { lookup } from 'node:dns/promises';
import { LookupFunction } from 'node:net';
import { Agent as HttpAgent } from 'node:http';
import { Agent as HttpsAgent } from 'node:https';
import * as ipaddr from 'ipaddr.js';

import { ValidateMetadataDTO } from '@dto';
import { LoggerMessage, MetadataValidationStatus } from '@enums';
import { validateMetadataStandard, parseMetadata, getStandard } from '@utils';
import { /* MetadataStandard, */ ValidateMetadataResult } from '@types';

class UrlBlockedError extends Error {
  readonly code = MetadataValidationStatus.URL_BLOCKED;

  constructor() {
    super(MetadataValidationStatus.URL_BLOCKED);
    this.name = 'UrlBlockedError';
  }
}

@Injectable()
export class AppService {
  constructor(private readonly httpService: HttpService) {}

  private readonly safeHttpAgent = new HttpAgent({
    keepAlive: true,
    lookup: this.createSafeLookup(),
  });

  private readonly safeHttpsAgent = new HttpsAgent({
    keepAlive: true,
    lookup: this.createSafeLookup(),
  });

  private stripIPv6Brackets(hostname: string): string {
    return hostname.startsWith('[') && hostname.endsWith(']')
      ? hostname.slice(1, -1)
      : hostname;
  }

  private isBlockedAddress(address: string): boolean {
    const normalized = this.stripIPv6Brackets(address);
    if (!ipaddr.isValid(normalized)) {
      return false;
    }

    return ipaddr.process(normalized).range() !== 'unicast';
  }

  private isUrlBlockedError(error: unknown): boolean {
    if (!error || typeof error !== 'object') {
      return false;
    }

    const candidate = error as {
      code?: unknown;
      cause?: { code?: unknown };
    };

    return (
      candidate.code === MetadataValidationStatus.URL_BLOCKED ||
      candidate.cause?.code === MetadataValidationStatus.URL_BLOCKED
    );
  }

  private async assertAllowedMetadataUrl(url: string): Promise<void> {
    let parsedUrl: URL;

    try {
      parsedUrl = new URL(url);
    } catch (error) {
      throw MetadataValidationStatus.URL_NOT_FOUND;
    }

    if (!['http:', 'https:'].includes(parsedUrl.protocol)) {
      throw MetadataValidationStatus.URL_BLOCKED;
    }

    const hostname = this.stripIPv6Brackets(parsedUrl.hostname.toLowerCase());
    if (hostname === 'localhost' || hostname.endsWith('.localhost')) {
      throw MetadataValidationStatus.URL_BLOCKED;
    }

    if (this.isBlockedAddress(hostname)) {
      throw MetadataValidationStatus.URL_BLOCKED;
    }

    let resolvedAddresses: { address: string }[];
    try {
      resolvedAddresses = await lookup(hostname, { all: true, verbatim: true });
    } catch (error) {
      throw MetadataValidationStatus.URL_NOT_FOUND;
    }

    if (
      resolvedAddresses.some(({ address }) => this.isBlockedAddress(address))
    ) {
      throw MetadataValidationStatus.URL_BLOCKED;
    }
  }

  private createSafeLookup(): LookupFunction {
    return (hostname, options, callback) => {
      lookup(hostname, options)
        .then((result) => {
          const addresses = Array.isArray(result) ? result : [result];

          if (addresses.some(({ address }) => this.isBlockedAddress(address))) {
            callback(new UrlBlockedError(), '', 0);
            return;
          }

          if (Array.isArray(result)) {
            callback(null, result);
            return;
          }

          callback(null, result.address, result.family);
        })
        .catch((error) => {
          callback(error as Error, '', 0);
        });
    };
  }

  async validateMetadata({
    hash,
    url,
    standard: paramStandard,
  }: ValidateMetadataDTO): Promise<ValidateMetadataResult> {
    let status: MetadataValidationStatus;
    let metadata: Record<string, unknown>;
    let standard = paramStandard;

    const isIPFS = url.startsWith('ipfs://');
    if (isIPFS) {
      url = `${process.env.IPFS_GATEWAY}/${url.slice(7)}`;
    }

    try {
      await this.assertAllowedMetadataUrl(url);
      const { data: rawData } = await firstValueFrom(
        this.httpService
          .get(url, {
            httpAgent: this.safeHttpAgent,
            httpsAgent: this.safeHttpsAgent,
            maxRedirects: 0,
            proxy: false,
            headers: {
              // Required to not being blocked by APIs that require a User-Agent
              'User-Agent': 'GovTool/Metadata-Validation-Tool',
              'Content-Type': 'application/json',
              ...(isIPFS &&
                process.env.IPFS_PROJECT_ID && {
                  project_id: process.env.IPFS_PROJECT_ID,
                }),
            },
          })
          .pipe(
            finalize(() => Logger.log(`Fetching ${url} completed`)),
            catchError((error) => {
              Logger.error(error, JSON.stringify(error));
              if (this.isUrlBlockedError(error)) {
                throw MetadataValidationStatus.URL_BLOCKED;
              }

              throw MetadataValidationStatus.URL_NOT_FOUND;
            }),
          ),
      );

      let parsedData;
      try {
        parsedData = JSON.parse(rawData);
      } catch (error) {
        throw MetadataValidationStatus.INCORRECT_FORMAT;
      }

      // TODO: Uncomment this when gov action: 7f320409d9998712ff3a3cdf0c9439e1543f236a3d746766f78f1fdbe1e06bf8#0 expires
      // if (
      //   standard === MetadataStandard.CIP108 &&
      //   !Array.isArray(parsedData.authors)
      // ) {
      //   throw MetadataValidationStatus.INCORRECT_FORMAT;
      // }

      if (!parsedData?.body) {
        throw MetadataValidationStatus.INCORRECT_FORMAT;
      }

      if (!standard) {
        standard = getStandard(parsedData);
      }

      if (standard) {
        await validateMetadataStandard(parsedData.body, standard);
        metadata = parseMetadata(parsedData.body);
      }

      const hashedMetadata = blake.blake2bHex(rawData, undefined, 32);

      if (hashedMetadata !== hash) {
        throw MetadataValidationStatus.INVALID_HASH;
      }
    } catch (error) {
      Logger.error(LoggerMessage.METADATA_VALIDATION_ERROR, error);
      if (Object.values(MetadataValidationStatus).includes(error)) {
        status = error;
      }
    }

    return { status, valid: !Boolean(status), metadata };
  }
}
