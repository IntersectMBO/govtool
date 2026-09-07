import { Injectable, Logger } from '@nestjs/common';
import { catchError, finalize, firstValueFrom } from 'rxjs';
import { HttpService } from '@nestjs/axios';
import * as blake from 'blakejs';
import * as dns from 'dns';
import * as net from 'net';

import { ValidateMetadataDTO } from '@dto';
import { LoggerMessage, MetadataValidationStatus } from '@enums';
import { validateMetadataStandard, parseMetadata, getStandard } from '@utils';
import { /* MetadataStandard, */ ValidateMetadataResult } from '@types';

function isPrivateIP(ip: string): boolean {
  // Check for loopback
  if (ip === '127.0.0.1' || ip === '::1' || ip === 'localhost') return true;
  
  // Check for private ranges (RFC 1918)
  const parts = ip.split('.').map(Number);
  if (parts.length !== 4) return false;
  
  const [a, b, c, d] = parts;
  // 10.0.0.0/8
  if (a === 10) return true;
  // 172.16.0.0/12
  if (a === 172 && b >= 16 && b <= 31) return true;
  // 192.168.0.0/16
  if (a === 192 && b === 168) return true;
  // 169.254.0.0/16 (link-local)
  if (a === 169 && b === 254) return true;
  // 127.0.0.0/8 (loopback)
  if (a === 127) return true;
  // 0.0.0.0
  if (a === 0 && b === 0 && c === 0 && d === 0) return true;
  // Cloud metadata endpoint
  if (ip === '169.254.169.254') return true;
  
  return false;
}

function isBlockedHostname(hostname: string): boolean {
  const lower = hostname.toLowerCase();
  if (lower === 'localhost' || lower === '127.0.0.1' || lower === '::1') return true;
  if (lower.startsWith('metadata.') || lower === 'instance-data' || lower === 'instance-data.') return true;
  return false;
}

function validateUrl(url: string): void {
  let parsedUrl: URL;
  try {
    parsedUrl = new URL(url);
  } catch {
    throw new Error('Invalid URL format');
  }

  // Only allow http, https, and ipfs protocols
  if (!['http:', 'https:', 'ipfs:'].includes(parsedUrl.protocol)) {
    throw new Error('Unsupported protocol: ' + parsedUrl.protocol);
  }

  const hostname = parsedUrl.hostname.toLowerCase();
  
  // Block loopback and internal hostnames
  if (isBlockedHostname(hostname)) {
    throw new Error('Blocked hostname: ' + hostname);
  }

  // Check if hostname is an IP address and block private ranges
  if (net.isIPv4(hostname) || net.isIPv6(hostname)) {
    if (isPrivateIP(hostname)) {
      throw new Error('Blocked private/internal IP: ' + hostname);
    }
  }
}

@Injectable()
export class AppService {
  constructor(private readonly httpService: HttpService) {}

  async validateMetadata({
    hash,
    url,
    standard: paramStandard,
  }: ValidateMetadataDTO): Promise<ValidateMetadataResult> {
    let status: MetadataValidationStatus;
    let metadata: Record<string, unknown>;
    let standard = paramStandard;

    // SSRF protection: validate URL before fetching
    try {
      validateUrl(url);
    } catch (error) {
      const msg = error instanceof Error ? error.message : 'Unknown error';
      Logger.warn(`Blocked SSRF attempt: ${msg}`);
      return { status: MetadataValidationStatus.URL_NOT_FOUND, valid: false, metadata: undefined };
    }

    const isIPFS = url.startsWith('ipfs://');
    if (isIPFS) {
      url = `${process.env.IPFS_GATEWAY}/${url.slice(7)}`;
    }

    // Validate URL after IPFS gateway rewrite
    try {
      validateUrl(url);
    } catch (error) {
      const msg = error instanceof Error ? error.message : 'Unknown error';
      Logger.warn(`Blocked SSRF attempt after IPFS rewrite: ${msg}`);
      return { status: MetadataValidationStatus.URL_NOT_FOUND, valid: false, metadata: undefined };
    }

    try {
      const { data: rawData } = await firstValueFrom(
        this.httpService
          .get(url, {
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
