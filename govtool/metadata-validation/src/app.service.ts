import { Injectable, Logger } from '@nestjs/common';
import { catchError, finalize, firstValueFrom } from 'rxjs';
import { HttpService } from '@nestjs/axios';
import * as blake from 'blakejs';
import { lookup } from 'node:dns/promises';
import { isIP, LookupFunction } from 'node:net';
import { Agent as HttpAgent } from 'node:http';
import { Agent as HttpsAgent } from 'node:https';

import { ValidateMetadataDTO } from '@dto';
import { LoggerMessage, MetadataValidationStatus } from '@enums';
import { validateMetadataStandard, parseMetadata, getStandard } from '@utils';
import { /* MetadataStandard, */ ValidateMetadataResult } from '@types';

@Injectable()
export class AppService {
  constructor(private readonly httpService: HttpService) {}

  private readonly safeHttpAgent = new HttpAgent({
    lookup: this.createSafeLookup(),
  });

  private readonly safeHttpsAgent = new HttpsAgent({
    lookup: this.createSafeLookup(),
  });

  private isBlockedIPv4(address: string): boolean {
    const parts = address.split('.').map(Number);
    const [first, second] = parts;

    return (
      first === 0 ||
      first === 10 ||
      first === 127 ||
      (first === 100 && second >= 64 && second <= 127) ||
      (first === 169 && second === 254) ||
      (first === 172 && second >= 16 && second <= 31) ||
      (first === 192 && second === 0 && parts[2] === 0) ||
      (first === 192 && second === 168) ||
      (first === 198 && (second === 18 || second === 19)) ||
      first >= 224
    );
  }

  private isBlockedIPv6(address: string): boolean {
    const normalized = address.toLowerCase();
    const ipv4MappedPrefix = '::ffff:';

    if (normalized.startsWith(ipv4MappedPrefix)) {
      const mappedAddress = normalized.slice(ipv4MappedPrefix.length);
      if (isIP(mappedAddress) === 4) {
        return this.isBlockedIPv4(mappedAddress);
      }
    }

    return (
      normalized === '::' ||
      normalized === '::1' ||
      normalized.startsWith('fc') ||
      normalized.startsWith('fd') ||
      normalized.startsWith('fe80:') ||
      normalized.startsWith('::ffff:0:')
    );
  }

  private isBlockedAddress(address: string): boolean {
    const version = isIP(address);

    if (version === 4) {
      return this.isBlockedIPv4(address);
    }

    if (version === 6) {
      return this.isBlockedIPv6(address);
    }

    return false;
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

    const hostname = parsedUrl.hostname.toLowerCase();
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

          if (
            addresses.some(({ address }) => this.isBlockedAddress(address))
          ) {
            callback(new Error(MetadataValidationStatus.URL_BLOCKED), '', 0);
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
