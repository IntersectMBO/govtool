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
import { proposalFields, resolveDocument } from 'src/metadata/enrich';
import { MetadataService } from 'src/metadata/metadata.service';
import { MetadataStandard } from 'src/metadata/metadata.type';
import { ProposalService, isActionIdTerm } from 'src/proposal/proposal.service';
import { CHAIN_DATA, METADATA } from 'src/providers/providers.module';
import { SystemService } from 'src/system/system.service';
import {
  NO_TEXT,
  compareGovernanceActions,
  matchesGovernanceActionFilters,
  matchesGovernanceActionSearch,
  toGovernanceActionDetailRow,
  toGovernanceActionListRow,
  type GovernanceActionText,
} from './governance-actions.mapping';
import type {
  GovernanceActionDetailRow,
  GovernanceActionListRow,
  GovernanceActionMetadataResponse,
  GovernanceActionNetworkMetrics,
  GovernanceActionSort,
  SignatureVerificationResult,
} from './governance-actions.type';
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
 * The governance governance action routes, served from the chain-data contract so the
 * governance action UI needs no service of its own (D143).
 *
 * Actions come out of the proposal snapshot the `/proposal` routes already
 * keep, so listing governance action costs no extra provider read. Document text is
 * resolved through the metadata service when one is configured, and is null
 * otherwise; the UI then asks `…/metadata` for it, as it did before.
 */
@Injectable()
export class GovernanceActionsService {
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
    sort: GovernanceActionSort;
    page: number;
    limit: number;
  }): Promise<GovernanceActionListRow[]> {
    return this.cache.getOrSet('governanceActionsList', params, () =>
      asHttp(async () => {
        const actions = await this.proposals.getActions();
        let candidates = actions.filter((a) =>
          matchesGovernanceActionFilters(a, params.filters),
        );
        if (params.search !== '') {
          // Title and abstract are document text, read from the documents
          // the warmer fetched for every action. Until each has had an
          // answer a search would leave some out, so it refuses instead.
          if (
            !isActionIdTerm(params.search) &&
            !this.proposals.documentsReady
          ) {
            throw new ServiceUnavailableException({
              errorType: 'ServiceUnavailableError',
              message:
                'Governance action documents are still being fetched, so a text search cannot cover them yet. Try again shortly.',
            });
          }
          candidates = candidates.filter((a) =>
            matchesGovernanceActionSearch(a, this.storedText(a), params.search),
          );
        }
        candidates.sort(compareGovernanceActions(params.sort));
        const start = (params.page - 1) * params.limit;
        const page = candidates.slice(start, start + params.limit);

        const [schedule, committee] = await Promise.all([
          this.network.epochSchedule(),
          this.committeeFor(page),
        ]);
        return page.map((a) =>
          toGovernanceActionListRow(a, schedule, committee, this.storedText(a)),
        );
      }),
    );
  }

  get(
    txHash: string,
    index: string | undefined,
  ): Promise<GovernanceActionDetailRow> {
    // Validated before any read: a malformed hash or index is a 400.
    const { id } = legacyGovActionId(`${txHash}#${index ?? '0'}`);
    return this.cache.getOrSet('governanceActionsAction', id, () =>
      asHttp(async () => {
        const [action] = await this.proposals.getActions(id);
        if (action === undefined) {
          throw new NotFoundException({
            errorType: 'NotFoundError',
            message: `Governance action with ID '${txHash}#${index ?? '0'}' not found`,
          });
        }
        const [schedule, committee, info, text] = await Promise.all([
          this.network.epochSchedule(),
          this.committeeFor([action]),
          this.chain.network.getNetworkInfo(),
          this.text(action),
        ]);
        return toGovernanceActionDetailRow(
          action,
          schedule,
          committee,
          info.data.currentEpoch,
          text,
        );
      }),
    );
  }

  /**
   * The legacy validation, in the governance action UI's field names, with the
   * document's `authors` in `data` as the governance action service returned them.
   */
  async getMetadata(
    url: string,
    hash: string,
  ): Promise<GovernanceActionMetadataResponse> {
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
    return this.cache.getOrSet(
      'governanceActionsPdfProposal',
      hash,
      async () => {
        const query = new URLSearchParams({
          'filters[prop_submission_tx_hash][$eq]': hash,
          'pagination[page]': '1',
          'pagination[pageSize]': '1',
        });
        const body = await this.fetchPdf(
          `${base}/proposals?${query.toString()}`,
        );
        const items = (body as { data?: unknown }).data;
        const first: unknown =
          Array.isArray(items) && items.length > 0 ? items[0] : null;
        return { data: first };
      },
    );
  }

  /* ----------------------------------------------------------------------- */
  /* Miscellaneous                                                            */
  /* ----------------------------------------------------------------------- */

  getEpochParams(epoch: number | undefined): Promise<LegacyEpochParams> {
    return this.cache.getOrSet(
      'governanceActionsEpochParams',
      epoch ?? 'current',
      () =>
        asHttp(async () => {
          const q = await this.atEpoch(epoch, 'protocolParams.epoch');
          const { data } = await this.chain.network.getProtocolParams(q);
          return toLegacyEpochParams(data);
        }),
    );
  }

  /**
   * The figures the governance action UI divides votes by, at `epoch` (the current one
   * when absent). A stake figure the provider cannot compute is a 501 naming
   * it, never a 0 that would read as "no stake".
   */
  getNetworkMetrics(
    epoch: number | undefined,
  ): Promise<GovernanceActionNetworkMetrics> {
    return this.cache.getOrSet(
      'governanceActionsNetworkMetrics',
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
            // As the governance action service reported it: active DReps plus both
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
    return this.cache.getOrSet(
      'governanceActionsJsonLdContext',
      url,
      async () => {
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
      },
    );
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
   * An action's document text from the proposal routes' document store, or
   * none when it is not stored: the list never fetches while a request waits.
   */
  private storedText(action: GovAction): GovernanceActionText {
    const document = this.proposals.storedDocument({
      url: action.anchor?.url ?? null,
      hash: action.anchor?.dataHash ?? null,
    });
    return document === undefined ? NO_TEXT : this.textOf(document);
  }

  private textOf(document: Record<string, unknown>): GovernanceActionText {
    return { ...(proposalFields(bodyOf(document)) ?? NO_TEXT), document };
  }

  /**
   * One action's document text for its detail page: the stored document, or
   * fetched here when the store does not hold it, such as an action submitted
   * since the last fill.
   */
  private async text(action: GovAction): Promise<GovernanceActionText> {
    const anchor = {
      url: action.anchor?.url ?? null,
      hash: action.anchor?.dataHash ?? null,
    };
    const document =
      this.proposals.storedDocument(anchor) ??
      (await resolveDocument(this.metadata, anchor));
    return document === undefined ? NO_TEXT : this.textOf(document);
  }

  /** GET against the operator-configured pdf API: bounded, no redirects. */
  private async fetchPdf(url: string): Promise<unknown> {
    let response: Response;
    try {
      response = await fetch(url, {
        headers: {
          Accept: 'application/json',
          'User-Agent': 'GovTool/Backend',
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
