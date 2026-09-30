import {
  HttpException,
  Inject,
  Injectable,
  NotFoundException,
  ServiceUnavailableException,
} from '@nestjs/common';
import {
  ChainDataError,
  type ChainDataApiV1,
  type Committee,
  type GovAction,
  type OptionalArgument,
} from '@govtool/data-providers/chain-data';
import type { MetadataServiceV1 } from '@govtool/data-providers/metadata';

import { CacheService } from 'src/cache/cache.service';
import { ConfigService } from 'src/config/config.service';
import { asHttp } from 'src/common/errors';
import { legacyGovActionId } from 'src/common/legacy-ids';
import { LegacyNetwork } from 'src/common/legacy-network';
import { toLegacyEpochParams } from 'src/epoch/epoch.service';
import type { LegacyEpochParams } from 'src/epoch/epoch.type';
import {
  ENRICH_CONCURRENCY,
  mapLimit,
  proposalFields,
  resolveDocument,
} from 'src/metadata/enrich';
import { MetadataService } from 'src/metadata/metadata.service';
import { MetadataStandard } from 'src/metadata/metadata.type';
import { ProposalService } from 'src/proposal/proposal.service';
import { CHAIN_DATA, METADATA } from 'src/providers/providers.module';
import { SystemService } from 'src/system/system.service';
import {
  NO_TEXT,
  compareOutcomes,
  matchesOutcomeFilters,
  matchesOutcomeSearch,
  toOutcomeDetailRow,
  toOutcomeListRow,
  type OutcomeText,
} from './outcomes.mapping';
import type {
  OutcomeDetailRow,
  OutcomeListRow,
  OutcomeMetadataResponse,
  OutcomeNetworkMetrics,
  OutcomeSort,
  SignatureVerificationResult,
} from './outcomes.type';
import { DocumentSummaryCache } from './text-cache';
import { verifyAuthorWitness, type AuthorWitnessInput } from './signature';

const PDF_TIMEOUT_MS = 10_000;
const PDF_MAX_BYTES = 1024 * 1024;
const DOCUMENT_MAX_BYTES = 1024 * 1024;

const bodyOf = (
  document: Record<string, unknown>,
): Record<string, unknown> | undefined => {
  const body = document['body'];
  return typeof body === 'object' && body !== null && !Array.isArray(body)
    ? (body as Record<string, unknown>)
    : undefined;
};

/**
 * The governance outcomes routes, served from the chain-data contract so the
 * outcomes UI needs no service of its own (D143).
 *
 * Actions come out of the proposal snapshot the `/proposal` routes already
 * keep, so listing outcomes costs no extra provider read. Document text is
 * resolved through the metadata service when one is configured, and is null
 * otherwise; the UI then asks `…/metadata` for it, as it did before.
 */
@Injectable()
export class OutcomesService {
  /** Title and abstract per (hash, url), for search and list rows. */
  private readonly summaryCache = new DocumentSummaryCache();
  private warming = false;

  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    @Inject(METADATA) private readonly metadata: MetadataServiceV1 | null,
    private readonly proposals: ProposalService,
    private readonly cache: CacheService,
    private readonly network: LegacyNetwork,
    private readonly metadataValidation: MetadataService,
    private readonly system: SystemService,
    private readonly config: ConfigService,
  ) {}

  /* ----------------------------------------------------------------------- */
  /* Governance actions                                                       */
  /* ----------------------------------------------------------------------- */

  list(params: {
    search: string;
    filters: string[];
    sort: OutcomeSort;
    page: number;
    limit: number;
  }): Promise<OutcomeListRow[]> {
    return this.cache.getOrSet('outcomesList', params, () =>
      asHttp(async () => {
        const actions = await this.proposals.getActions();
        let candidates = actions.filter((a) =>
          matchesOutcomeFilters(a, params.filters),
        );
        let texts = new Map<string, OutcomeText>();
        if (params.search !== '') {
          // Title and abstract are document text, so a search needs every
          // candidate's; they are kept per (hash, url) between requests.
          texts = await this.summaries(candidates);
          candidates = candidates.filter((a) =>
            matchesOutcomeSearch(a, texts.get(a.id) ?? NO_TEXT, params.search),
          );
        }
        candidates.sort(compareOutcomes(params.sort));
        const start = (params.page - 1) * params.limit;
        const page = candidates.slice(start, start + params.limit);
        if (params.search === '') texts = await this.summaries(page);

        const [schedule, committee] = await Promise.all([
          this.network.epochSchedule(),
          this.committeeFor(page),
        ]);
        return page.map((a) =>
          toOutcomeListRow(a, schedule, committee, texts.get(a.id) ?? NO_TEXT),
        );
      }),
    );
  }

  get(txHash: string, index: string | undefined): Promise<OutcomeDetailRow> {
    // Validated before any read: a malformed hash or index is a 400.
    const { id } = legacyGovActionId(`${txHash}#${index ?? '0'}`);
    return this.cache.getOrSet('outcomesAction', id, () =>
      asHttp(async () => {
        const [action] = await this.proposals.getActions(id);
        if (action === undefined) {
          throw new NotFoundException({
            errorType: 'NotFoundError',
            message: `Governance action with ID '${txHash}#${index ?? '0'}' not found`,
          });
        }
        const [schedule, committee, info, texts] = await Promise.all([
          this.network.epochSchedule(),
          this.committeeFor([action]),
          this.chain.network.getNetworkInfo(),
          this.texts([action]),
        ]);
        return toOutcomeDetailRow(
          action,
          schedule,
          committee,
          info.data.currentEpoch,
          texts.get(action.id) ?? NO_TEXT,
        );
      }),
    );
  }

  /**
   * The legacy validation, in the outcomes UI's field names, with the
   * document's `authors` in `data` as the outcomes service returned them.
   */
  async getMetadata(
    url: string,
    hash: string,
  ): Promise<OutcomeMetadataResponse> {
    const result = await this.metadataValidation.validateMetadata(
      { url, hash, standard: MetadataStandard.CIP108 },
      { includeAuthors: true },
    );
    return {
      metadataStatus: result.status ?? null,
      metadataValid: result.valid,
      data: result.metadata,
    };
  }

  /**
   * The discussion-forum proposal submitted in `txHash`, as `{ data }` with
   * the forum's own item shape, or `{ data: null }` when there is none.
   */
  getProposal(txHash: string): Promise<{ data: unknown }> {
    const base = this.config.get().pdfApiUrl;
    if (base === null) {
      throw new ServiceUnavailableException({
        errorType: 'ServiceUnavailableError',
        message: 'GOVTOOL_PDF_API_URL is not configured',
      });
    }
    const hash = txHash.toLowerCase();
    if (!/^[0-9a-f]{64}$/.test(hash)) {
      throw new HttpException(
        {
          errorType: 'ValidationError',
          message: 'hash must be a 64-hex transaction hash',
        },
        400,
      );
    }
    return this.cache.getOrSet('outcomesPdfProposal', hash, async () => {
      const query = new URLSearchParams({
        'filters[prop_submission_tx_hash][$eq]': hash,
        'pagination[page]': '1',
        'pagination[pageSize]': '1',
      });
      const body = await this.fetchPdf(`${base}/proposals?${query.toString()}`);
      const items = (body as { data?: unknown }).data;
      const first: unknown =
        Array.isArray(items) && items.length > 0 ? items[0] : null;
      return { data: first };
    });
  }

  /* ----------------------------------------------------------------------- */
  /* Miscellaneous                                                            */
  /* ----------------------------------------------------------------------- */

  getEpochParams(epoch: number | undefined): Promise<LegacyEpochParams> {
    return this.cache.getOrSet('outcomesEpochParams', epoch ?? 'current', () =>
      asHttp(async () => {
        const q = await this.atEpoch(epoch, 'protocolParams.epoch');
        const { data } = await this.chain.network.getProtocolParams(q);
        return toLegacyEpochParams(data);
      }),
    );
  }

  /**
   * The figures the outcomes UI divides votes by, at `epoch` (the current one
   * when absent). A stake figure the provider cannot compute is a 501 naming
   * it, never a 0 that would read as "no stake".
   */
  getNetworkMetrics(epoch: number | undefined): Promise<OutcomeNetworkMetrics> {
    return this.cache.getOrSet(
      'outcomesNetworkMetrics',
      epoch ?? 'current',
      () =>
        asHttp(async () => {
          const [stakeQ, committeeQ, info] = await Promise.all([
            this.atEpoch(epoch, 'stakeDistribution.epoch'),
            this.atEpoch(epoch, 'committee.epoch'),
            this.chain.network.getNetworkInfo(),
          ]);
          const [{ data: stake }, { data: committee }] = await Promise.all([
            this.chain.network.getStakeDistribution(stakeQ),
            this.chain.governance.committee.getCommittee(committeeQ),
          ]);
          const at = epoch ?? info.data.currentEpoch;
          const need = (value: string | undefined, field: string): bigint => {
            if (value === undefined) {
              throw new ChainDataError(
                'CAPABILITY_UNSUPPORTED',
                `The configured chain-data provider cannot serve ${field}`,
              );
            }
            return BigInt(value);
          };
          const abstain = need(
            stake.alwaysAbstainVotingPower,
            'alwaysAbstainVotingPower',
          );
          const noConfidence = need(
            stake.alwaysNoConfidenceVotingPower,
            'alwaysNoConfidenceVotingPower',
          );
          const dreps = need(
            stake.totalStakeControlledByDReps,
            'totalStakeControlledByDReps',
          );
          return {
            epoch_no: at,
            // As the outcomes service reported it: active DReps plus both
            // predefined targets, the whole DRep-delegated stake.
            total_stake_controlled_by_active_dreps: String(
              dreps + abstain + noConfidence,
            ),
            total_stake_controlled_by_stake_pools: String(
              need(
                stake.totalStakeControlledBySPOs,
                'totalStakeControlledBySPOs',
              ),
            ),
            always_abstain_voting_power: String(abstain),
            spos_abstain_voting_power: String(
              need(
                stake.spoAlwaysAbstainVotingPower,
                'spoAlwaysAbstainVotingPower',
              ),
            ),
            always_no_confidence_voting_power: String(noConfidence),
            spos_no_confidence_voting_power: String(
              need(
                stake.spoAlwaysNoConfidenceVotingPower,
                'spoAlwaysNoConfidenceVotingPower',
              ),
            ),
            // Seats still in term at the epoch, resigned or not, as before.
            no_of_committee_members: committee.members.filter(
              (m) => m.termExpiryEpoch === null || m.termExpiryEpoch >= at,
            ).length,
            quorum_numerator: committee.quorum.numerator,
            quorum_denominator: committee.quorum.denominator,
          };
        }),
    );
  }

  /** CIP-100 author witness check over the document at `metadataUrl`. */
  async verifySignature(
    body: AuthorWitnessInput & { metadataUrl?: unknown },
  ): Promise<SignatureVerificationResult> {
    if (typeof body?.metadataUrl !== 'string' || body.metadataUrl === '') {
      return { isValid: false, error: 'metadataUrl is missing' };
    }
    let document: Record<string, unknown>;
    try {
      const text = await this.metadataValidation.fetchDocumentText(
        body.metadataUrl,
      );
      if (Buffer.byteLength(text) > DOCUMENT_MAX_BYTES)
        throw new Error('too large');
      const parsed: unknown = JSON.parse(text);
      if (
        typeof parsed !== 'object' ||
        parsed === null ||
        Array.isArray(parsed)
      ) {
        throw new Error('not an object');
      }
      document = parsed as Record<string, unknown>;
    } catch {
      return { isValid: false, error: 'Failed to fetch metadata' };
    }
    return verifyAuthorWitness(body, document, (url) =>
      this.jsonLdContext(url),
    );
  }

  /**
   * A remote JSON-LD `@context`, through the same guarded fetch as the
   * document (SSRF guard, size and time limits, ipfs:// via the gateway) and
   * cached by url. Which contexts to fetch at all is decided in #4255.
   */
  private jsonLdContext(url: string): Promise<Record<string, unknown>> {
    return this.cache.getOrSet('outcomesJsonLdContext', url, async () => {
      const text = await this.metadataValidation.fetchDocumentText(url);
      if (Buffer.byteLength(text) > DOCUMENT_MAX_BYTES) {
        throw new Error('JSON-LD context too large');
      }
      const parsed: unknown = JSON.parse(text);
      if (
        typeof parsed !== 'object' ||
        parsed === null ||
        Array.isArray(parsed)
      ) {
        throw new Error('JSON-LD context is not an object');
      }
      return parsed as Record<string, unknown>;
    });
  }

  /* ----------------------------------------------------------------------- */

  /**
   * `{ epoch }` for a provider that honours the argument; nothing for the
   * current epoch, which every provider serves; otherwise a 501, because
   * answering with current figures for a past epoch would be fabricating.
   */
  private async atEpoch(
    epoch: number | undefined,
    argument: OptionalArgument,
  ): Promise<{ epoch: number } | undefined> {
    if (epoch === undefined) return undefined;
    const capabilities = await this.system.getCapabilities();
    if (capabilities.optionalArguments.includes(argument)) return { epoch };
    const { data } = await this.chain.network.getNetworkInfo();
    if (data.currentEpoch === epoch) return undefined;
    throw new ChainDataError(
      'CAPABILITY_UNSUPPORTED',
      `The configured chain-data provider does not serve ${argument}`,
    );
  }

  /** The current committee, read only when an UpdateCommittee needs it. */
  private async committeeFor(actions: GovAction[]): Promise<Committee | null> {
    if (!actions.some((a) => a.body.type === 'UpdateCommittee')) return null;
    try {
      return (await this.chain.governance.committee.getCommittee()).data;
    } catch {
      // Only the "current term" column needs it; the rest of the row stands.
      return null;
    }
  }

  /**
   * Resolves every action's title and abstract into the summary cache, so
   * the first search after a start or a new block does not wait on them.
   * Expired entries are refreshed here and awaited by this run only; a
   * search meanwhile is served the value already held. One run at a time;
   * failures only leave entries unresolved or keep their previous value.
   */
  async warmSearchText(): Promise<void> {
    if (this.metadata === null || this.warming) return;
    this.warming = true;
    try {
      await this.summaries(await this.proposals.getActions(), true);
    } finally {
      this.warming = false;
    }
  }

  /**
   * Title and abstract only (what list rows and search read), through the
   * summary cache. The detail route resolves the whole document instead.
   */
  private async summaries(
    actions: GovAction[],
    awaitRefresh = false,
  ): Promise<Map<string, OutcomeText>> {
    const out = new Map<string, OutcomeText>();
    const metadata = this.metadata;
    if (metadata === null) return out;
    const summaries = await mapLimit(actions, ENRICH_CONCURRENCY, (a) => {
      const url = a.anchor?.url ?? null;
      const hash = a.anchor?.dataHash ?? null;
      if (!url || !hash) return Promise.resolve(undefined);
      return this.summaryCache.get(
        hash,
        url,
        async () => {
          const document = await resolveDocument(metadata, { url, hash });
          if (document === undefined) return undefined;
          const fields = proposalFields(bodyOf(document));
          return {
            title: fields?.title ?? null,
            abstract: fields?.abstract ?? null,
          };
        },
        { awaitRefresh },
      );
    });
    actions.forEach((a, i) => {
      const summary = summaries[i];
      if (summary !== undefined) out.set(a.id, { ...NO_TEXT, ...summary });
    });
    return out;
  }

  private async texts(actions: GovAction[]): Promise<Map<string, OutcomeText>> {
    const out = new Map<string, OutcomeText>();
    if (this.metadata === null) return out;
    const docs = await mapLimit(actions, ENRICH_CONCURRENCY, (a) =>
      resolveDocument(this.metadata, {
        url: a.anchor?.url ?? null,
        hash: a.anchor?.dataHash ?? null,
      }),
    );
    actions.forEach((a, i) => {
      const document = docs[i];
      if (document === undefined) return;
      const fields = proposalFields(bodyOf(document));
      out.set(a.id, { ...(fields ?? NO_TEXT), document });
    });
    return out;
  }

  /** GET against the operator-configured pdf API: bounded, no redirects. */
  private async fetchPdf(url: string): Promise<unknown> {
    let response: Response;
    try {
      response = await fetch(url, {
        headers: {
          Accept: 'application/json',
          'User-Agent': 'GovTool/Outcomes',
        },
        redirect: 'error',
        signal: AbortSignal.timeout(PDF_TIMEOUT_MS),
      });
    } catch {
      throw new ServiceUnavailableException({
        errorType: 'ServiceUnavailableError',
        message: 'The proposal discussion API cannot be reached',
      });
    }
    if (!response.ok) {
      throw new HttpException(
        {
          errorType: 'UpstreamError',
          message: `The proposal discussion API answered ${response.status}`,
        },
        response.status >= 500 ? 502 : response.status,
      );
    }
    const text = await response.text();
    if (Buffer.byteLength(text) > PDF_MAX_BYTES) {
      throw new HttpException(
        { errorType: 'UpstreamError', message: 'Proposal response too large' },
        502,
      );
    }
    try {
      return JSON.parse(text) as unknown;
    } catch {
      throw new HttpException(
        {
          errorType: 'UpstreamError',
          message: 'Proposal response is not JSON',
        },
        502,
      );
    }
  }
}
