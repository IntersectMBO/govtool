/* eslint-disable @typescript-eslint/only-throw-error --
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
import { UploadRateLimiter } from '../ipfs/upload-rate-limiter';
import { METADATA } from '../providers/providers.module';

import { ValidateMetadataDto } from './dto/validate-metadata.dto';
import { MetadataValidationStatus } from './metadata-status.enum';
import {
  MetadataIssue,
  MetadataStandard,
  ValidateMetadataResult,
} from './metadata.type';
import { fetchMetadataText, MetadataFetchError } from './safe-metadata-fetch';

/** CIP-108 length limits, per the CIP text (F43). */
const CIP108_TITLE_MAX_LENGTH = 80;
const CIP108_ABSTRACT_MAX_LENGTH = 2500;

/**
 * How long validation waits for the metadata service before falling back to
 * the local fetch, which is capped at 10 s. Together they stay under the
 * frontend's 30 s request timeout; the service keeps fetching after this and
 * caches the result for the next read (D152).
 */
const SERVICE_BUDGET_MS = 15_000;

/**
 * Submission checks through the metadata service fetch every time and keep a
 * report of each failure forever, so they are rate-limited per client and per
 * instance. Over the limit the check still runs, with the local fetch and no
 * report (D152).
 */
const VERIFY_LIMITS = {
  perClientLimit: 30,
  globalLimit: 600,
  windowSeconds: 600,
  maxTrackedClients: 10_000,
};

type ServiceAnswer =
  | { ok: true; document: Record<string, unknown> }
  | { ok: false; status: MetadataValidationStatus; reportId?: string };

@Injectable()
export class MetadataService {
  private readonly logger = new Logger(MetadataService.name);
  private readonly verifyLimiter = new UploadRateLimiter(VERIFY_LIMITS);

  constructor(
    private readonly config: ConfigService,
    @Inject(METADATA)
    private readonly metadataService: MetadataServiceV1 | null,
  ) {
    if (this.config.get().metadataAllowPrivateUrls) {
      this.logger.warn(
        'GOVTOOL_METADATA_ALLOW_PRIVATE_URLS=true: metadata fetches may reach loopback and private addresses. Local testing only.',
      );
    }
  }

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
   * hash, fails over across IPFS gateways, guards against private addresses
   * and keeps a fetch report of every failure, none of which the local fetch
   * does. With `verify` it fetches the url whatever the cache holds (D152).
   * Returns `undefined` when there is no service, it cannot verify, the client
   * is over the verify limit, it cannot be reached or it does not answer
   * within `SERVICE_BUDGET_MS`, so the caller falls back to the local fetch
   * rather than reporting a working document as missing.
   */
  private async resolveThroughService(
    hash: string,
    url: string,
    verify: { clientKey: string } | undefined,
  ): Promise<ServiceAnswer | undefined> {
    const service = this.metadataService;
    if (!service) return undefined;
    if (verify) {
      if (!service.verify) return undefined;
      if (!this.verifyLimiter.consume(verify.clientKey).allowed) {
        this.logger.warn(
          `metadata verify rate limit hit for client ${verify.clientKey}, verifying with a local fetch`,
        );
        return undefined;
      }
    }
    let timer: NodeJS.Timeout | undefined;
    try {
      const timeout = new Promise<undefined>((resolve) => {
        timer = setTimeout(() => resolve(undefined), SERVICE_BUDGET_MS);
      });
      const request = verify
        ? service.verify!(hash.toLowerCase(), url)
        : service.getMetadata(hash.toLowerCase(), url);
      const result = await Promise.race([request, timeout]);
      if (!result) {
        this.logger.warn(
          `metadata service gave no answer within ${SERVICE_BUDGET_MS} ms, falling back to a local fetch`,
        );
        return undefined;
      }
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
      return { ok: false, status, reportId: result.reportId };
    } catch (error) {
      this.logger.warn(
        `metadata service unavailable, falling back to a local fetch: ${error instanceof Error ? error.message : String(error)}`,
      );
      return undefined;
    } finally {
      clearTimeout(timer);
    }
  }

  /**
   * `verifyUrl` fetches `url` now instead of answering from the service's
   * hash cache, which ignores the url once the content is known (D112), so it
   * could not prove that `url` serves the document. Submission sets it; reads
   * take the cache. A failure the service saw carries its `reportId` either
   * way, so the author can see why (D152).
   *
   * `options.includeAuthors` adds the document's CIP-100 `authors` to
   * `metadata`, as the governance action UI reads them; the legacy route omits them.
   * `options.clientKey` is who asked, for the verify rate limit.
   */
  async validateMetadata(
    { hash, url, standard: paramStandard, verifyUrl }: ValidateMetadataDto,
    options: { includeAuthors?: boolean; clientKey?: string } = {},
  ): Promise<ValidateMetadataResult> {
    let status: MetadataValidationStatus | undefined;
    let reportId: string | undefined;
    let metadata: Record<string, unknown> | undefined;
    let issues: MetadataIssue[] = [];
    let standard = paramStandard;

    try {
      const viaService = await this.resolveThroughService(
        hash,
        url,
        verifyUrl ? { clientKey: options.clientKey ?? 'internal' } : undefined,
      );
      if (viaService && !viaService.ok) {
        reportId = viaService.reportId;
        throw viaService.status;
      }

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
        issues = this.validateMetadataStandard(
          parsedData.body as Record<string, unknown>,
          standard,
        );
        if (issues.some((issue) => issue.severity === 'error')) {
          throw MetadataValidationStatus.INCORRECT_FORMAT;
        }

        metadata = this.parseMetadata(
          parsedData.body as Record<string, unknown>,
        );
        if (options.includeAuthors) {
          metadata.authors = Array.isArray(parsedData.authors)
            ? parsedData.authors
            : [];
        }
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

    // Issues explain a format failure or warn about a valid document; beside
    // any other failure they would describe a document nobody can see.
    const showIssues =
      issues.length > 0 &&
      (!status || status === MetadataValidationStatus.INCORRECT_FORMAT);

    return {
      status,
      valid: !status,
      metadata,
      ...(showIssues && { issues }),
      ...(status && reportId && { reportId }),
    };
  }

  /**
   * The raw document at `url` (an `ipfs://` url through the gateway), under
   * the same guards as validation: no private addresses, no redirects, a
   * size cap and a timeout. Throws a `MetadataValidationStatus` or a
   * `MetadataFetchError`.
   */
  fetchDocumentText(url: string): Promise<string> {
    return this.fetchMetadata(
      this.resolveMetadataUrl(url),
      url.startsWith('ipfs://'),
    );
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
    return fetchMetadataText(
      url,
      {
        'User-Agent': 'GovTool/Metadata-Validation-Tool',
        'Content-Type': 'application/json',
        ...(isIpfs && projectId ? { project_id: projectId } : {}),
      },
      { allowPrivateAddresses: this.config.get().metadataAllowPrivateUrls },
    );
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

  /** Every rule `body` breaks; empty when it meets the standard. */
  private validateMetadataStandard(
    body: Record<string, unknown>,
    standard: MetadataStandard,
  ): MetadataIssue[] {
    switch (standard) {
      case MetadataStandard.CIP119:
        return this.getFieldValue(body, 'givenName')
          ? []
          : [{ field: 'givenName', rule: 'required', severity: 'error' }];

      case MetadataStandard.CIP108:
        return this.validateCip108Body(body);

      default:
        return [];
    }
  }

  /**
   * A missing field is an error. An over-long title or abstract is only a
   * warning: the document is still readable, and the old validator accepted
   * up to 84 / 3000 characters, so rejecting them would hide existing actions.
   */
  private validateCip108Body(body: Record<string, unknown>): MetadataIssue[] {
    const issues: MetadataIssue[] = [];
    const title = this.getFieldValue(body, 'title');
    const abstract = this.getFieldValue(body, 'abstract');

    if (!this.isNonBlankString(title)) {
      issues.push({ field: 'title', rule: 'required', severity: 'error' });
    }
    if (!this.isNonBlankString(abstract)) {
      issues.push({ field: 'abstract', rule: 'required', severity: 'error' });
    }
    for (const field of ['motivation', 'rationale']) {
      if (!this.getFieldValue(body, field)) {
        issues.push({ field, rule: 'required', severity: 'error' });
      }
    }

    const limits: [string, unknown, number][] = [
      ['title', title, CIP108_TITLE_MAX_LENGTH],
      ['abstract', abstract, CIP108_ABSTRACT_MAX_LENGTH],
    ];
    for (const [field, value, limit] of limits) {
      if (typeof value === 'string' && value.length > limit) {
        issues.push({
          field,
          rule: 'maxLength',
          severity: 'warning',
          limit,
          actual: value.length,
        });
      }
    }

    return issues;
  }

  // CIP-108 requires `title` and `abstract` as strings.
  // Empty or whitespace-only values carry no content, so they are rejected.
  private isNonBlankString(value: unknown): value is string {
    return typeof value === 'string' && value.trim().length > 0;
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
