import { HttpException, Inject, Injectable, Logger } from '@nestjs/common';
import type {
  MetadataRefreshOutcome,
  MetadataReport,
  MetadataReportSummary,
  MetadataResult,
  MetadataServiceV1,
} from '@govtool/data-providers/metadata';

import { redactIpsDeep } from '../common/redact-ip';
import { METADATA } from '../providers/providers.module';
import { forgetDocumentFailure } from './document-store';

/**
 * Left as the service sent them when redacting addresses (D171): the
 * document, which is the publisher's content; the urls, which the chain or
 * the publisher's server named; and the ids and hashes, which callers send
 * back. Everything else, the DNS answers, connect attempts, headers and
 * messages, has its addresses redacted.
 */
const UNREDACTED = new Set([
  'body',
  'url',
  'effectiveUrl',
  'redirectTo',
  'id',
  'reportId',
  'hash',
  'servedHash',
]);

/**
 * The backend's public door to the private metadata service (D119).
 *
 * It passes the contract's values through, with IP addresses redacted
 * (D171), and adds nothing about caching: no `Cache-Control`, no TTLs. The
 * retry window (one real fetch per anchor per minute, D125) is enforced by
 * the service; this relays its answer.
 */
@Injectable()
export class MetadataGatewayService {
  private readonly logger = new Logger(MetadataGatewayService.name);

  constructor(
    @Inject(METADATA) private readonly metadata: MetadataServiceV1 | null,
  ) {}

  resolve(hash: string, url: string): Promise<MetadataResult> {
    return this.call('resolve', (m) => m.getMetadata(hash.toLowerCase(), url));
  }

  async retry(hash: string, url: string): Promise<MetadataRefreshOutcome> {
    const outcome = await this.call('retry', (m) =>
      m.refresh(hash.toLowerCase(), url),
    );
    // A publisher fixed the url: the stored failure must not outlive it.
    if (outcome.result.ok) forgetDocumentFailure(hash, url);
    return outcome;
  }

  async getReport(id: string): Promise<MetadataReport> {
    const report = await this.call('getReport', (m) => m.getReport(id));
    if (report === null) {
      throw new HttpException(
        { errorType: 'NotFoundError', message: 'No such metadata report' },
        404,
      );
    }
    return report;
  }

  listReports(hash: string, url: string): Promise<MetadataReportSummary[]> {
    return this.call('listReports', (m) =>
      m.listReports(hash.toLowerCase(), url),
    );
  }

  /**
   * 503 when no service is configured. Anything the client throws is an
   * infrastructure fault: it is logged here and answered with a generic 502,
   * so neither the service's address nor its error text reaches the caller.
   */
  private async call<T>(
    operation: string,
    run: (metadata: MetadataServiceV1) => Promise<T>,
  ): Promise<T> {
    if (this.metadata === null) {
      throw new HttpException(
        {
          errorType: 'MetadataUnconfiguredError',
          message: 'Backend is not configured for metadata resolution',
        },
        503,
      );
    }
    try {
      return redactIpsDeep(await run(this.metadata), UNREDACTED);
    } catch (error) {
      if (error instanceof HttpException) throw error;
      this.logger.error(
        `metadata ${operation} failed: ${error instanceof Error ? error.message : String(error)}`,
      );
      throw new HttpException(
        {
          errorType: 'MetadataServiceError',
          message: 'The metadata service is unavailable',
        },
        502,
      );
    }
  }
}
