/* eslint-disable @typescript-eslint/only-throw-error, @typescript-eslint/no-base-to-string --
 * This service throws `MetadataValidationStatus` enum values as its control
 * flow and stringifies untyped JSON-LD field values. Both are the legacy
 * behaviour, and the thrown status is what reaches the response body, so it
 * is preserved rather than refactored.
 */
import { Inject, Injectable, Logger } from '@nestjs/common';
import * as blake from 'blakejs';
import type {
  MetadataFailureCode,
  MetadataServiceV1,
} from '@govtool/data-providers/metadata';

import { ConfigService } from '../config/config.service';
import { METADATA } from '../providers/providers.module';

import { ValidateMetadataDto } from './dto/validate-metadata.dto';
import { MetadataValidationStatus } from './metadata-status.enum';
import { MetadataStandard, ValidateMetadataResult } from './metadata.type';
import { fetchMetadataText, MetadataFetchError } from './safe-metadata-fetch';

@Injectable()
export class MetadataService {
  private readonly logger = new Logger(MetadataService.name);

  constructor(
    private readonly config: ConfigService,
    @Inject(METADATA)
    private readonly metadataService: MetadataServiceV1 | null,
  ) {}

  /**
   * The metadata service's failure codes in the legacy statuses the frontend
   * already renders.
   */
  private static readonly LEGACY_STATUS: Record<
    MetadataFailureCode,
    MetadataValidationStatus
  > = {
    FETCH_ERROR: MetadataValidationStatus.URL_NOT_FOUND,
    EXCEEDS_LIMIT: MetadataValidationStatus.EXCEEDS_LIMIT,
    JSON_PARSE_ERROR: MetadataValidationStatus.INCORRECT_FORMAT,
    HASH_MISMATCH: MetadataValidationStatus.INVALID_HASH,
    SCHEMA_INVALID: MetadataValidationStatus.INCORRECT_FORMAT,
  };

  /**
   * Resolve through the metadata service when one is configured: it caches by
   * hash, fails over across IPFS gateways and guards against private
   * addresses, none of which the local fetch does. Returns `undefined` when
   * there is no service or it cannot be reached, so the caller falls back to
   * the local fetch rather than reporting a working document as missing.
   */
  private async resolveThroughService(
    hash: string,
    url: string,
  ): Promise<
    | { ok: true; document: Record<string, unknown> }
    | { ok: false; status: MetadataValidationStatus }
    | undefined
  > {
    if (!this.metadataService) return undefined;
    try {
      const result = await this.metadataService.getMetadata(
        hash.toLowerCase(),
        url,
      );
      if (result.ok) {
        return {
          ok: true,
          document: (result.body ?? {}) as Record<string, unknown>,
        };
      }
      const status =
        result.code === 'FETCH_ERROR' && result.message.startsWith('Refused')
          ? MetadataValidationStatus.URL_BLOCKED
          : MetadataService.LEGACY_STATUS[result.code];
      return { ok: false, status };
    } catch (error) {
      this.logger.warn(
        `metadata service unavailable, falling back to a local fetch: ${error instanceof Error ? error.message : String(error)}`,
      );
      return undefined;
    }
  }

  async validateMetadata({
    hash,
    url,
    standard: paramStandard,
  }: ValidateMetadataDto): Promise<ValidateMetadataResult> {
    let status: MetadataValidationStatus | undefined;
    let metadata: Record<string, unknown> | undefined;
    let standard = paramStandard;

    try {
      const viaService = await this.resolveThroughService(hash, url);
      if (viaService && !viaService.ok) throw viaService.status;

      // The service has already verified the hash over the exact bytes; the
      // local path verifies it below.
      let rawData: string | undefined;
      let parsedData: Record<string, unknown>;

      if (viaService) {
        parsedData = viaService.document;
      } else {
        const resolvedUrl = this.resolveMetadataUrl(url);
        rawData = await this.fetchMetadata(
          resolvedUrl,
          url.startsWith('ipfs://'),
        );
        try {
          parsedData = JSON.parse(rawData) as Record<string, unknown>;
        } catch {
          throw MetadataValidationStatus.INCORRECT_FORMAT;
        }
      }

      if (!parsedData.body || typeof parsedData.body !== 'object') {
        throw MetadataValidationStatus.INCORRECT_FORMAT;
      }

      if (!standard) {
        standard = this.getStandard(parsedData);
      }

      if (standard) {
        this.validateMetadataStandard(
          parsedData.body as Record<string, unknown>,
          standard,
        );

        metadata = this.parseMetadata(
          parsedData.body as Record<string, unknown>,
        );
      }

      if (rawData !== undefined) {
        const hashedMetadata = blake.blake2bHex(rawData, undefined, 32);

        if (hashedMetadata.toLowerCase() !== hash.toLowerCase()) {
          throw MetadataValidationStatus.INVALID_HASH;
        }
      }
    } catch (error) {
      this.logger.error('Metadata validation failed', error);

      if (error instanceof MetadataFetchError) {
        status = error.code;
      } else if (
        Object.values(MetadataValidationStatus).includes(
          error as MetadataValidationStatus,
        )
      ) {
        status = error as MetadataValidationStatus;
      } else {
        status = MetadataValidationStatus.INTERNAL_ERROR;
      }
    }

    return {
      status,
      valid: !status,
      metadata,
    };
  }

  private resolveMetadataUrl(url: string): string {
    if (url.startsWith('ipfs://')) {
      const gateway = this.config.get().ipfsGateway;

      if (!gateway) {
        throw MetadataValidationStatus.URL_NOT_FOUND;
      }

      return `${gateway.replace(/\/$/, '')}/${url.slice(7)}`;
    }

    return url;
  }

  private fetchMetadata(url: string, isIpfs: boolean): Promise<string> {
    const projectId = this.config.get().ipfsProjectId;
    return fetchMetadataText(url, {
      'User-Agent': 'GovTool/Metadata-Validation-Tool',
      'Content-Type': 'application/json',
      ...(isIpfs && projectId ? { project_id: projectId } : {}),
    });
  }

  private getStandard(
    data: Record<string, unknown>,
  ): MetadataStandard | undefined {
    const json = JSON.stringify(data);

    if (json.includes(MetadataStandard.CIP119)) {
      return MetadataStandard.CIP119;
    }

    if (json.includes(MetadataStandard.CIP108)) {
      return MetadataStandard.CIP108;
    }

    return undefined;
  }

  private validateMetadataStandard(
    body: Record<string, unknown>,
    standard: MetadataStandard,
  ): true {
    switch (standard) {
      case MetadataStandard.CIP119: {
        const givenName = this.getFieldValue(body, 'givenName');

        if (!givenName) {
          throw MetadataValidationStatus.INCORRECT_FORMAT;
        }

        return true;
      }

      case MetadataStandard.CIP108:
        return this.validateCip108Body(body);

      default:
        return true;
    }
  }

  private validateCip108Body(body: Record<string, unknown>): true {
    const title = this.getFieldValue(body, 'title');
    const abstract = this.getFieldValue(body, 'abstract');
    const motivation = this.getFieldValue(body, 'motivation');
    const rationale = this.getFieldValue(body, 'rationale');

    if (!title || !abstract || !motivation || !rationale) {
      throw MetadataValidationStatus.INCORRECT_FORMAT;
    }

    if (String(title).length > 80 || String(abstract).length > 2500) {
      throw MetadataValidationStatus.INCORRECT_FORMAT;
    }

    return true;
  }

  private parseMetadata(
    body: Record<string, unknown>,
  ): Record<string, unknown> {
    const metadata: Record<string, unknown> = {};

    Object.keys(body).forEach((key) => {
      if (key === 'references' && Array.isArray(body[key])) {
        metadata[key] = (body[key] as Record<string, unknown>[]).map(
          (reference) => this.parseMetadata(reference),
        );
      } else {
        metadata[key] = this.getFieldValue(body, key);
      }
    });

    return metadata;
  }

  private getFieldValue(
    data: Record<string, unknown>,
    fieldName: string,
  ): unknown {
    const value = data[fieldName];

    if (value && typeof value === 'object' && '@value' in value) {
      return value['@value'];
    }

    return value;
  }
}
