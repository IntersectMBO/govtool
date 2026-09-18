import type {
  Envelope,
  EpochNo,
  GovernanceMetrics,
  MetricsApi,
} from '@govtool/data-providers/chain-data';

import { unsupported } from '../../common/errors';
import { envelope } from '../../common/meta';
import { toLovelace } from '../../common/numbers';
import type { KoiosHttpClient } from '../../http/client';
import type {
  CommitteeInfoRow,
  DRepEpochSummaryRow,
  TotalsRow,
} from '../../rows';

/**
 * The fields of `GovernanceMetrics` that Koios has no way to compute, and why.
 *
 * These are all *required* on the contract type, which is why `get()` refuses
 * the whole route rather than returning a record with holes in it. The
 * computable two thirds are still reachable, through `getAvailable()`.
 */
export const UNCOMPUTABLE_METRICS: Record<string, string> = {
  uniqueDelegators:
    'would mean calling /drep_delegators once per DRep and de-duplicating the union',
  totalDelegations: 'same walk as uniqueDelegators',
  totalActiveDReps:
    'the active flag is on /drep_info, which takes explicit ids — there is no aggregate',
  totalInactiveDReps: 'same as totalActiveDReps',
  totalActiveCip119CompliantDReps:
    'needs the metadata body of every DRep, one /drep_metadata batch at a time',
  totalRegisteredDirectVoters:
    'Koios does not model GovTool’s direct (sole) voter at all',
};

export class KoiosMetricsApi implements MetricsApi {
  constructor(private readonly http: KoiosHttpClient) {}

  /**
   * Refused as a whole.
   *
   * Six of `GovernanceMetrics`' required fields need a walk over every DRep —
   * `/drep_info` and `/drep_delegators` both take explicit ids and Koios
   * publishes no aggregate for them — so answering this route means well over
   * a thousand requests, or filling required numbers with zeros that a
   * dashboard would render as fact. `getAvailable()` returns the subset that
   * is one cheap request each.
   */
  get(_q?: { epoch?: EpochNo }): Promise<Envelope<GovernanceMetrics>> {
    return Promise.reject(
      unsupported(
        'governance.metrics.get',
        `Koios cannot compute ${Object.keys(UNCOMPUTABLE_METRICS).join(', ')} without walking every DRep; use getAvailable() for the rest`,
      ),
    );
  }

  /**
   * The counters Koios *can* serve, each from one request:
   * proposal and vote totals from PostgREST's `content-range`, the DRep
   * distribution from `/drep_epoch_summary`, the committee from
   * `/committee_info` and the treasury from `/totals`.
   *
   * Not part of `ChainDataApiV1` — a caller reaches it through the concrete
   * provider, having decided what to do about the missing fields.
   */
  async getAvailable(q?: {
    epoch?: EpochNo;
  }): Promise<Envelope<Partial<GovernanceMetrics>>> {
    const epochQuery = q?.epoch === undefined ? {} : { _epoch_no: q.epoch };

    const [
      drepSummary,
      committee,
      totals,
      registeredDReps,
      allActions,
      liveActions,
      drepVotes,
      spoVotes,
      ccVotes,
    ] = await Promise.all([
      this.http.get<DRepEpochSummaryRow>('drep_epoch_summary', epochQuery, {
        order: 'epoch_no.desc',
        limit: 1,
      }),
      this.http.get<CommitteeInfoRow>('committee_info'),
      this.http.get<TotalsRow>('totals', epochQuery, {
        order: 'epoch_no.desc',
        limit: 1,
      }),
      this.count('drep_list', { registered: 'eq.true' }, 'drep_id'),
      this.count('proposal_list', {}, 'proposal_id'),
      this.count(
        'proposal_list',
        {
          enacted_epoch: 'is.null',
          ratified_epoch: 'is.null',
          expired_epoch: 'is.null',
          dropped_epoch: 'is.null',
        },
        'proposal_id',
      ),
      this.count('vote_list', { voter_role: 'eq.DRep' }, 'vote_tx_hash'),
      this.count('vote_list', { voter_role: 'eq.SPO' }, 'vote_tx_hash'),
      this.count(
        'vote_list',
        { voter_role: 'eq.ConstitutionalCommittee' },
        'vote_tx_hash',
      ),
    ]);

    const summaryRow = drepSummary.rows[0];
    const committeeRow = committee.rows[0];
    const totalsRow = totals.rows[0];

    const metrics: Partial<GovernanceMetrics> = {};
    if (summaryRow !== undefined) {
      metrics.epoch = summaryRow.epoch_no;
      metrics.totalDRepDistribution = toLovelace(summaryRow.amount);
    }
    if (registeredDReps !== null) {
      metrics.totalRegisteredDReps = registeredDReps;
    }
    if (allActions !== null) metrics.totalGovernanceActions = allActions;
    if (liveActions !== null) metrics.totalLiveGovernanceActions = liveActions;
    if (drepVotes !== null) metrics.totalDRepVotes = drepVotes;
    if (spoVotes !== null) metrics.totalSpoVotes = spoVotes;
    if (ccVotes !== null) metrics.totalCcVotes = ccVotes;
    if (committeeRow !== undefined) {
      metrics.committee = {
        size: committeeRow.members.length,
        quorum: {
          numerator: committeeRow.quorum_numerator,
          denominator: committeeRow.quorum_denominator,
        },
      };
    }
    if (totalsRow !== undefined) {
      metrics.treasury = {
        balance: toLovelace(totalsRow.treasury),
        reserves: toLovelace(totalsRow.reserves),
      };
    }
    return envelope(metrics);
  }

  /**
   * A count with no rows: `limit=0` plus `Prefer: count=exact`, so PostgREST
   * runs the `COUNT(*)` and returns the total in `content-range` without
   * serialising a single row. `select` is narrowed to one column for the same
   * reason.
   */
  private async count(
    path: string,
    query: Record<string, string>,
    column: string,
  ): Promise<number | null> {
    const response = await this.http.get(path, query, {
      limit: 0,
      select: column,
      count: 'exact',
    });
    return response.total;
  }
}
