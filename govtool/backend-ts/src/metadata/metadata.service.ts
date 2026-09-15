import { Injectable, Logger } from '@nestjs/common';
import * as blake from 'blakejs';

import { ValidateMetadataDto } from './dto/validate-metadata.dto';
import { MetadataValidationStatus } from './metadata-status.enum';
import {
  MetadataStandard,
  ValidateMetadataResult,
} from './metadata.type';

@Injectable()
export class MetadataService {
  private readonly logger = new Logger(MetadataService.name);

  async validateMetadata({
    hash,
    url,
    standard: paramStandard,
  }: ValidateMetadataDto): Promise<ValidateMetadataResult> {
    let status: MetadataValidationStatus | undefined;
    let metadata: Record<string, unknown> | undefined;
    let standard = paramStandard;

    try {
      const resolvedUrl = this.resolveMetadataUrl(url);
      const rawData = await this.fetchMetadata(resolvedUrl);

      let parsedData: Record<string, unknown>;

      try {
        parsedData = JSON.parse(rawData) as Record<string, unknown>;
      } catch {
        throw MetadataValidationStatus.INCORRECT_FORMAT;
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

      const hashedMetadata = blake.blake2bHex(rawData, undefined, 32);

      if (hashedMetadata.toLowerCase() !== hash.toLowerCase()) {
        throw MetadataValidationStatus.INVALID_HASH;
      }
    } catch (error) {
      this.logger.error('Metadata validation failed', error);

      if (Object.values(MetadataValidationStatus).includes(
        error as MetadataValidationStatus,
      )) {
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
      const gateway = process.env.IPFS_GATEWAY;

      if (!gateway) {
        throw MetadataValidationStatus.URL_NOT_FOUND;
      }

      return `${gateway.replace(/\/$/, '')}/${url.slice(7)}`;
    }

    return url;
  }

  private async fetchMetadata(url: string): Promise<string> {
    const response = await fetch(url, {
      headers: {
        'User-Agent': 'GovTool/Metadata-Validation-Tool',
        'Content-Type': 'application/json',
      },
    });

    if (!response.ok) {
      throw MetadataValidationStatus.URL_NOT_FOUND;
    }

    return response.text();
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

  private parseMetadata(body: Record<string, unknown>): Record<string, unknown> {
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

    if (
      value &&
      typeof value === 'object' &&
      '@value' in value
    ) {
      return (value as { '@value': unknown })['@value'];
    }

    return value;
  }
}
