import type {
  EnactedActionSummary,
  Envelope,
  GovAction,
  GovActionActivityEvent,
  GovActionExpand,
  GovActionRef,
  GovActionType,
  PagedEnvelope,
  PageRequest,
  ProposalListQuery,
  ProposalsApi,
  RoleTally,
  VoteListQuery,
  VoteRecord,
  VoterRole,
} from '@govtool/data-providers/chain-data';

import { mapWithConcurrency } from '../../common/concurrency';
import type { EpochTimeResolver } from '../../common/epoch-time';
import { notFound, unsupported } from '../../common/errors';
import { parseGovActionId } from '../../common/ids';
import { envelope } from '../../common/meta';
import { toBlockfrostPage, toContractPage } from '../../common/paging';
import type { BlockfrostClient } from '../../http/client';
import type {
  BfProposal,
  BfProposalMetadata,
  BfProposalParameters,
  BfProposalRef,
  BfProposalVote,
  BfProposalWithdrawal,
} from '../../http/types';
import {
  govActionRef,
  mapProposal,
  toBlockfrostGovernanceType,
} from '../../mappers/proposal.mapper';
import { mapProposalVote, tallyByRole } from '../../mappers/vote.mapper';

const HYDRATION_CONCURRENCY = 8;
/** How many candidates `getEnacted` will hydrate before giving up. */
const ENACTED_SCAN_LIMIT = 400;

export class BlockfrostProposalsApi implements ProposalsApi {
  constructor(
    private readonly client: BlockfrostClient,
    private readonly epochs: EpochTimeResolver,
  ) {}

  /**
   * `/governance/proposals` lists `{tx_hash, cert_index, governance_type}`,
   * so the type is filterable before hydration — unlike DReps, where the
   * directory carries nothing. Everything else needs the detail read, plus
   * metadata, plus the type's sub-resource.
   *
   * Unlike a live-only source this serves **every** status, because each
   * proposal record carries its ratified / enacted / dropped / expired epoch.
   *
   * `sort` is refused: ordering by anything but Blockfrost's own order would
   * mean hydrating every proposal first.
   */
  async list(q?: ProposalListQuery): Promise<PagedEnvelope<GovAction>> {
    if (q?.sort !== undefined) {
      throw unsupported(
        'governance.proposals.list{sort}',
        'sorting requires hydrating every proposal; Blockfrost offers no server-side sort',
      );
    }
    if (q?.voterId !== undefined) {
      throw unsupported(
        'governance.proposals.list{voterId}',
        'per-voter annotation means one votes read per proposal',
      );
    }
    for (const field of q?.expand ?? []) {
      if (
        field === 'thresholds' ||
        field === 'protocolParams' ||
        field === 'myVote'
      ) {
        throw unsupported(`governance.proposals.list#${field}`);
      }
    }

    const page = toBlockfrostPage(q, { count: 25, order: 'desc' });
    const refs = await this.client.get<BfProposalRef[]>(
      '/governance/proposals',
      page,
    );

    const wantedTypes = q?.type;
    const candidates =
      wantedTypes === undefined || wantedTypes.length === 0
        ? refs
        : refs.filter((ref) =>
            wantedTypes
              .map(toBlockfrostGovernanceType)
              .includes(ref.governance_type),
          );

    let actions = (
      await mapWithConcurrency(candidates, HYDRATION_CONCURRENCY, (ref) =>
        this.hydrate(ref.tx_hash, ref.cert_index, q?.expand),
      )
    ).filter((a): a is GovAction => a !== null);

    const statuses = q?.status;
    if (statuses !== undefined && statuses.length > 0) {
      actions = actions.filter((a) => statuses.includes(a.lifecycle.status));
    }
    const search = q?.search?.toLowerCase();
    if (search !== undefined && search !== '') {
      actions = actions.filter((a) => {
        const body = a.metadata?.body;
        return [
          a.id,
          `${a.txHash}#${a.index}`,
          body?.title,
          body?.abstract,
          body?.motivation,
          body?.rationale,
        ].some(
          (value) =>
            typeof value === 'string' && value.toLowerCase().includes(search),
        );
      });
    }

    return envelope({
      ...toContractPage(actions, page),
      // The cursor tracks the Blockfrost page, so filtering never hides rows
      // from a caller that follows it.
      nextCursor: refs.length < page.count ? null : String(page.page + 1),
    });
  }

  async get(
    id: string,
    q?: { expand?: GovActionExpand[]; voterId?: string },
  ): Promise<Envelope<GovAction>> {
    if (q?.voterId !== undefined) {
      throw unsupported('governance.proposals.get{voterId}');
    }
    const { txHash, index } = parseGovActionId(id);
    const action = await this.hydrate(txHash, index, q?.expand);
    if (action === null) {
      throw notFound(`Proposal with id: ${txHash}#${index} not found`, { id });
    }
    return envelope(action);
  }

  /**
   * Every vote on the action, with the voter's role and identity — which
   * db-sync's SQL cannot give per proposal. The votes carry no timestamp and
   * no voting power, so `at` and `votingPower` are absent; see `mapProposalVote`.
   */
  async listVotes(
    id: string,
    q?: VoteListQuery,
  ): Promise<PagedEnvelope<VoteRecord>> {
    if (q?.sort !== undefined) {
      throw unsupported(
        'governance.proposals.listVotes{sort}',
        'votes carry no timestamp, so they cannot be ordered by time',
      );
    }
    const { txHash, index } = parseGovActionId(id);
    const ref = govActionRef(txHash, index);
    const page = toBlockfrostPage(q, { count: 100 });
    const rows = await this.client.get<BfProposalVote[]>(
      `/governance/proposals/${txHash}/${index}/votes`,
      page,
    );

    let votes = rows.map((row) => mapProposalVote(ref, row));

    const roles = q?.role;
    if (roles !== undefined && roles.length > 0) {
      votes = votes.filter((v) => roles.includes(v.voter.role));
    }
    const choices = q?.vote;
    if (choices !== undefined && choices.length > 0) {
      votes = votes.filter((v) => choices.includes(v.vote));
    }

    return envelope({
      ...toContractPage(votes, page),
      nextCursor: rows.length < page.count ? null : String(page.page + 1),
    });
  }

  /**
   * Tallies by **headcount**, from the full vote list.
   *
   * Blockfrost attaches no voting power to a vote, so a DRep or SPO tally
   * here is turnout, not the weight the ledger counts — `RoleTally.stake`,
   * `threshold` and `passing` are all left unset rather than computed from
   * counts. Only the committee, which the ledger really does count by head,
   * is directly comparable with db-sync.
   */
  async getTallies(
    id: string,
    q?: { role?: VoterRole },
  ): Promise<Envelope<RoleTally[]>> {
    const { txHash, index } = parseGovActionId(id);
    const rows = await this.client.getAll<BfProposalVote>(
      `/governance/proposals/${txHash}/${index}/votes`,
    );
    const tallies = tallyByRole(rows);
    return envelope(
      q?.role === undefined
        ? tallies
        : tallies.filter((t) => t.role === q.role),
    );
  }

  listActivity(
    _id: string,
    _q?: PageRequest,
  ): Promise<PagedEnvelope<GovActionActivityEvent>> {
    return Promise.reject(
      unsupported(
        'governance.proposals.listActivity',
        'a lifecycle timeline needs dated events; Blockfrost dates neither the submission nor the votes',
      ),
    );
  }

  /**
   * Scans the proposal directory newest-first for an enacted action of that
   * type. The directory carries `governance_type`, so candidates are found
   * without hydration and only they are read in full.
   *
   * Unlike db-sync this is not a single indexed lookup, so the scan is
   * bounded: if no enacted action of the type is found within
   * `ENACTED_SCAN_LIMIT` candidates, the answer is `null` rather than an
   * unbounded crawl.
   */
  async getEnacted(
    type: GovActionType,
  ): Promise<Envelope<EnactedActionSummary | null>> {
    const governanceType = toBlockfrostGovernanceType(type);
    let scanned = 0;

    for (let page = 1; scanned < ENACTED_SCAN_LIMIT; page += 1) {
      const refs = await this.client.get<BfProposalRef[]>(
        '/governance/proposals',
        { count: 100, page, order: 'desc' },
      );
      if (refs.length === 0) break;

      const candidates = refs.filter(
        (ref) => ref.governance_type === governanceType,
      );
      scanned += candidates.length;

      const details = await mapWithConcurrency(
        candidates,
        HYDRATION_CONCURRENCY,
        (ref) =>
          this.client.getOrNull<BfProposal>(
            `/governance/proposals/${ref.tx_hash}/${ref.cert_index}`,
          ),
      );

      for (const detail of details) {
        if (detail !== null && detail.enacted_epoch !== null) {
          const summary: EnactedActionSummary = {
            type,
            action: govActionRef(detail.tx_hash, detail.cert_index),
            submittedTx: {
              txHash: detail.tx_hash,
              index: detail.cert_index,
            },
          };
          const enactedAt = await this.epochs.stamp(detail.enacted_epoch);
          if (enactedAt !== null) summary.enactedAt = enactedAt;
          if (detail.governance_description !== null) {
            summary.rawBody = detail.governance_description;
          }
          return envelope(summary);
        }
      }

      if (refs.length < 100) break;
    }

    return envelope(null);
  }

  /** Probes successive certificate indices on the transaction. */
  async listByTx(txHash: string): Promise<Envelope<GovActionRef[]>> {
    const refs: GovActionRef[] = [];
    for (let index = 0; index < 16; index += 1) {
      const detail = await this.client.getOrNull<BfProposal>(
        `/governance/proposals/${txHash}/${index}`,
      );
      if (detail === null) break;
      refs.push(govActionRef(detail.tx_hash, detail.cert_index));
    }
    return envelope(refs);
  }

  private async hydrate(
    txHash: string,
    index: number,
    expand?: GovActionExpand[],
  ): Promise<GovAction | null> {
    const base = `/governance/proposals/${txHash}/${index}`;
    const proposal = await this.client.getOrNull<BfProposal>(base);
    if (proposal === null) return null;

    const wantMetadata = expand === undefined || expand.includes('metadata');

    const [metadata, parameters, withdrawals] = await Promise.all([
      wantMetadata
        ? this.client.getOrNull<BfProposalMetadata>(`${base}/metadata`)
        : Promise.resolve(null),
      // Only a ParameterChange has these; every other type 404s.
      proposal.governance_type === 'parameter_change'
        ? this.client.getOrNull<BfProposalParameters>(`${base}/parameters`)
        : Promise.resolve(null),
      proposal.governance_type === 'treasury_withdrawals'
        ? this.client.getOrNull<BfProposalWithdrawal[]>(`${base}/withdrawals`)
        : Promise.resolve(null),
    ]);

    const [expires, ratifiedAt, enactedAt, droppedAt, expiredAt] =
      await Promise.all([
        this.epochs.stamp(proposal.expiration),
        this.epochs.stamp(proposal.ratified_epoch),
        this.epochs.stamp(proposal.enacted_epoch),
        this.epochs.stamp(proposal.dropped_epoch),
        this.epochs.stamp(proposal.expired_epoch),
      ]);

    const action = mapProposal({
      proposal,
      metadata,
      parameters,
      withdrawals,
      stamps: { expires, ratifiedAt, enactedAt, droppedAt, expiredAt },
    });

    if (expand?.includes('tallies')) {
      const rows = await this.client.getAll<BfProposalVote>(`${base}/votes`);
      action.tallies = tallyByRole(rows);
    }

    return action;
  }
}
