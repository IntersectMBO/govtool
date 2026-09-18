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

import { internal, notFound, unsupported } from '../../common/errors';
import { formatLegacyGovActionId, parseGovActionId } from '../../common/ids';
import { envelope } from '../../common/meta';
import { paginate } from '../../common/paging';
import { runSql } from '../../db/run';
import type { Queryable } from '../../db/queryable';
import {
  mapEnactedRow,
  mapProposalRow,
  toDbSyncType,
} from '../../mappers/proposal.mapper';
import type { EnactedProposalDetailsRow, ProposalRow } from '../../rows';

/**
 * The two action types whose enacted instance the statement can return. The
 * legacy service silently substituted `HardForkInitiation` for anything else;
 * this refuses instead, so a caller is never handed the wrong action's body.
 */
const ENACTED_TYPES: ReadonlySet<GovActionType> = new Set<GovActionType>([
  'ParameterChange',
  'HardForkInitiation',
]);

export class DbSyncProposalsApi implements ProposalsApi {
  constructor(private readonly db: Queryable) {}

  /**
   * `list-proposals.sql` returns live actions only — its first CTE requires
   * `expiration > MAX(epoch)` with no ratified/enacted/expired/dropped epoch.
   * So `status` can only ever be `['live']`, and a query for anything else is
   * refused rather than answered with an empty page.
   *
   * The statement's own search binding is used for the exact-id lookup in
   * `get`; for a list read it is passed empty and the caller's `search` is
   * applied here, over title, abstract, motivation, rationale and the
   * `txHash#index` id — the same fields, in the same order, as the legacy
   * service.
   */
  async list(q?: ProposalListQuery): Promise<PagedEnvelope<GovAction>> {
    const statuses = q?.status;
    if (
      statuses !== undefined &&
      statuses.length > 0 &&
      !(statuses.length === 1 && statuses[0] === 'live')
    ) {
      throw unsupported('governance.proposals.list{status}');
    }
    for (const field of q?.expand ?? []) {
      if (field !== 'tallies' && field !== 'metadata') {
        throw unsupported(`governance.proposals.list#${field}`);
      }
    }
    if (q?.voterId !== undefined) {
      throw unsupported('governance.proposals.list{voterId}');
    }

    let actions = await this.fetch('');

    const types = q?.type;
    if (types !== undefined && types.length > 0) {
      actions = actions.filter((action) => types.includes(action.type));
    }
    actions = this.filterBySearch(actions, q?.search);
    actions = this.sort(actions, q?.sort);

    return envelope(paginate(actions, q));
  }

  async get(
    id: string,
    q?: { expand?: GovActionExpand[]; voterId?: string },
  ): Promise<Envelope<GovAction>> {
    if (q?.voterId !== undefined) {
      throw unsupported('governance.proposals.get{voterId}');
    }
    const legacyId = formatLegacyGovActionId(parseGovActionId(id));
    const actions = await this.fetch(legacyId);

    if (actions.length === 0) {
      throw notFound(`Proposal with id: ${legacyId} not found`, { id });
    }
    if (actions.length !== 1) {
      throw internal(
        `Multiple proposals found for id: ${legacyId}. This should never happen`,
      );
    }
    return envelope(actions[0] as GovAction);
  }

  async getEnacted(
    type: GovActionType,
  ): Promise<Envelope<EnactedActionSummary | null>> {
    if (!ENACTED_TYPES.has(type)) {
      throw unsupported(`governance.proposals.getEnacted{type=${type}}`);
    }
    const rows = await runSql<EnactedProposalDetailsRow>(
      this.db,
      'get-previous-enacted-governance-action-proposal-details.sql',
      [toDbSyncType(type)],
    );
    const row = rows[0];
    if (rows.length !== 1 || row === undefined) {
      return envelope(null);
    }
    return envelope(mapEnactedRow(type, row));
  }

  /** Used by `dreps.listVotes` to resolve a vote's action. `null` when not live. */
  async findLive(legacyId: string): Promise<GovAction | null> {
    const actions = await this.fetch(legacyId);
    return actions[0] ?? null;
  }

  listVotes(
    _id: string,
    _q?: VoteListQuery,
  ): Promise<PagedEnvelope<VoteRecord>> {
    return Promise.reject(unsupported('governance.proposals.listVotes'));
  }

  getTallies(
    _id: string,
    _q?: { role?: VoterRole },
  ): Promise<Envelope<RoleTally[]>> {
    return Promise.reject(unsupported('governance.proposals.getTallies'));
  }

  listActivity(
    _id: string,
    _q?: PageRequest,
  ): Promise<PagedEnvelope<GovActionActivityEvent>> {
    return Promise.reject(unsupported('governance.proposals.listActivity'));
  }

  listByTx(_txHash: string): Promise<Envelope<GovActionRef[]>> {
    return Promise.reject(unsupported('governance.proposals.listByTx'));
  }

  /** The statement binds the search term once bare, four times as `%…%`, once bare. */
  private async fetch(search: string): Promise<GovAction[]> {
    const rows = await runSql<ProposalRow>(this.db, 'list-proposals.sql', [
      search,
      `%${search}%`,
      `%${search}%`,
      `%${search}%`,
      `%${search}%`,
      search,
    ]);
    return rows.map(mapProposalRow);
  }

  private filterBySearch(actions: GovAction[], search?: string): GovAction[] {
    if (!search) {
      return actions;
    }
    const needle = search.toLowerCase();
    return actions.filter((action) => {
      const body = action.metadata?.body;
      const haystack = [
        `${action.txHash}#${action.index}`,
        body?.title,
        body?.abstract,
        body?.motivation,
        body?.rationale,
      ].filter((value): value is string => typeof value === 'string');
      return haystack.some((value) => value.toLowerCase().includes(needle));
    });
  }

  private sort(
    actions: GovAction[],
    sort?: ProposalListQuery['sort'],
  ): GovAction[] {
    const copied = [...actions];
    switch (sort) {
      case 'newest':
        return copied.sort((a, b) => this.submittedAt(b) - this.submittedAt(a));
      case 'oldest':
        return copied.sort((a, b) => this.submittedAt(a) - this.submittedAt(b));
      case 'soonestToExpire':
        return copied.sort((a, b) => this.expiresAt(a) - this.expiresAt(b));
      case 'mostYesVotes':
        return copied.sort((a, b) => this.yesVotes(b) - this.yesVotes(a));
      default:
        return copied;
    }
  }

  /**
   * `lifecycle.submitted` is optional on the contract — a provider can know
   * an action's whole lifecycle except its submission point. This provider
   * always fills it, so the fallback is unreachable here; it exists so the
   * ordering is total rather than throwing on a stamp it did not produce.
   */
  private submittedAt(action: GovAction): number {
    const time = action.lifecycle.submitted?.time;
    return time === undefined ? 0 : Date.parse(time);
  }

  /** Legacy ordering put a null expiry last. */
  private expiresAt(action: GovAction): number {
    const time = action.lifecycle.expires?.time;
    return time === undefined ? Number.MAX_SAFE_INTEGER : Date.parse(time);
  }

  /**
   * Legacy `MostYesVotes` summed the DRep, pool and CC yes figures — stake
   * and headcount together. Reproduced, including that mixing.
   */
  private yesVotes(action: GovAction): number {
    let total = 0;
    for (const tally of action.tallies ?? []) {
      if (tally.stake !== undefined) {
        total += Number(tally.stake.yes);
      }
      if (tally.count !== undefined) {
        total += tally.count.yes;
      }
    }
    return total;
  }
}
