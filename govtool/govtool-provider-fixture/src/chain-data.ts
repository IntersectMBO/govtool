/**
 * `ChainDataApiV1` over the frozen dataset. Everything is filtered, sorted and
 * paged in memory — there is no network and no database.
 */

import type {
  AccountsApi,
  ChainDataApiV1,
  CommitteeApi,
  DRep,
  DRepsApi,
  DRepVoteRow,
  GovAction,
  GovActionLineage,
  GovActionType,
  NetworkApi,
  PoolsApi,
  ProposalsApi,
  ProviderCapabilities,
  ProviderIdentity,
  SystemApi,
  TransactionsApi,
  VoteAggregate,
  VoteRecord,
} from '@govtool/data-providers/chain-data';
import { ChainDataError } from '@govtool/data-providers/chain-data';

import type { FixtureData } from './data';
import { envelope, invalid, notFound, pagedEnvelope } from './paging';

/** Which lineage an action type belongs to. `null` = no previous action. */
const LINEAGE: Record<GovActionType, GovActionLineage | null> = {
  ParameterChange: 'pparamUpdate',
  HardForkInitiation: 'hardFork',
  // UpdateCommittee and NoConfidence SHARE the committee lineage — the enacted
  // head of either is the same action.
  UpdateCommittee: 'committee',
  NoConfidence: 'committee',
  NewConstitution: 'constitution',
  TreasuryWithdrawals: null,
  InfoAction: null,
};

/** Deterministic shuffle: a fixture must be reproducible across runs. */
function seededShuffle<T>(rows: readonly T[], seed: number): T[] {
  const out = [...rows];
  let s = seed || 1;
  for (let i = out.length - 1; i > 0; i--) {
    s = (s * 1103515245 + 12345) & 0x7fffffff;
    const j = s % (i + 1);
    [out[i], out[j]] = [out[j]!, out[i]!];
  }
  return out;
}

const epochOf = (a: GovAction) => a.lifecycle.submitted.epoch;

export function createChainData(data: FixtureData): ChainDataApiV1 {
  const net = data.networkInfo.network;
  const env = <T>(d: T) => envelope(d, net);
  const paged = <T>(rows: readonly T[], q: { page: number; size: number }) =>
    pagedEnvelope(rows, q, net);

  /* -- vote aggregates ----------------------------------------------------- */

  /**
   * Computed as head COUNTS, and declared as such.
   *
   * The fixture holds voting power only for the DReps it captured, and most
   * voters on a given action are not among them — so a stake-weighted total
   * would be a number assembled from partial data. A count is the honest
   * representation here, and `representation: 'count'` is what tells a consumer
   * to render it without an ada prefix.
   */
  function aggregatesFor(action: GovAction): VoteAggregate[] {
    const votes = data.votes[action.id] ?? [];
    if (votes.length === 0) return [];
    const roles: VoteAggregate['role'][] = ['drep', 'spo', 'cc'];
    const thresholds = data.protocolParams;
    const out: VoteAggregate[] = [];
    for (const role of roles) {
      {
        const mine = votes.filter((v) => v.voter.role === role);
        if (mine.length === 0) continue;
        const count = (choice: string) =>
          String(mine.filter((v) => v.choice === choice).length);
        const threshold =
          role === 'spo'
            ? thresholds.poolThresholds.committeeNormal
            : role === 'cc'
              ? data.committee?.quorum ?? { numerator: 2, denominator: 3 }
              : thresholds.drepThresholds.committeeNormal;
        out.push({
          role,
          representation: 'count',
          yes: count('yes'),
          no: count('no'),
          abstain: count('abstain'),
          notVoted: '0',
          totalEligible: String(mine.length),
          threshold,
        });
      }
    }
    return out;
  }

  const withAggregates = (a: GovAction): GovAction => ({
    ...a,
    voteAggregates: aggregatesFor(a),
  });

  /* -- network ------------------------------------------------------------- */

  const network: NetworkApi = {
    getNetworkInfo: () => Promise.resolve(env(data.networkInfo)),
    getProtocolParams: async (q) => {
      // The fixture is one frozen epoch, so a past epoch is genuinely
      // unavailable — and `protocolParams.epoch` is therefore not declared.
      if (q?.epoch !== undefined && q.epoch !== data.protocolParams.epoch) {
        return Promise.reject(
          new ChainDataError('CAPABILITY_UNSUPPORTED', 'the fixture holds one epoch only'),
        );
      }
      return Promise.resolve(env(data.protocolParams));
    },
    getStakeDistribution: () => Promise.resolve(env(data.stakeDistribution)),
    getTreasury: () => Promise.resolve(env(data.treasury)),
    // Present only when the dataset holds the genesis: availability is the interface's to say.
    ...(data.genesisParams ? { getGenesisParams: () => Promise.resolve(env(data.genesisParams!)) } : {}),
  };

  /* -- accounts ------------------------------------------------------------ */

  const accounts: AccountsApi = {
    get: async (stakeAddress) => {
      const row = data.accounts.find((a) => a.account.stakeAddress === stakeAddress);
      if (!row) throw notFound('account', stakeAddress);
      return Promise.resolve(env(row.account));
    },
    getDelegation: async (stakeAddress) => {
      const row = data.accounts.find((a) => a.account.stakeAddress === stakeAddress);
      if (!row) throw notFound('account', stakeAddress);
      return Promise.resolve(env(row.delegation));
    },
  };

  /* -- dreps --------------------------------------------------------------- */

  const dreps: DRepsApi = {
    list: async (q) => {
      let rows: DRep[] = data.dreps;
      if (q.status?.length) rows = rows.filter((d) => q.status!.includes(d.status));
      if (q.kind?.length) rows = rows.filter((d) => q.kind!.includes(d.kind));
      if (q.search) {
        const term = q.search.toLowerCase();
        // Only exact-id matching is declared, so that is all this does.
        rows = rows.filter((d) => d.id.toLowerCase() === term);
      }

      const sort = q.sort ?? 'random';
      if (sort === 'random') {
        // Random ordering is NOT paged: `size` applies, page 2 is refused.
        if (q.page > 1) throw invalid('a randomly ordered read is not paged');
        const shuffled = seededShuffle(rows, rows.length);
        return Promise.resolve(
          envelope({ elements: shuffled.slice(0, q.size), total: rows.length }, net),
        );
      }
      const sorted = [...rows].sort((a, b) => {
        if (sort === 'votingPower') {
          return Number(BigInt(b.votingPower?.amount ?? '0') - BigInt(a.votingPower?.amount ?? '0'));
        }
        if (sort === 'registrationDate') {
          return b.registration.latest.at.epoch - a.registration.latest.at.epoch;
        }
        return (b.activity?.voted ?? 0) - (a.activity?.voted ?? 0);
      });
      return Promise.resolve(paged(sorted, q));
    },

    get: async (id) => {
      const drep = data.dreps.find((d) => d.id === id);
      if (!drep) throw notFound('drep', id);
      return Promise.resolve(env(drep));
    },

    listVotes: async (id, q) => {
      const voted = new Map<string, VoteRecord>();
      for (const [actionId, votes] of Object.entries(data.votes)) {
        const mine = votes.find((v) => 'id' in v.voter && v.voter.id === id);
        if (mine) voted.set(actionId, mine);
      }
      // The listing covers voted AND not-voted actions, so the participation
      // denominator is the length of this list.
      let rows: DRepVoteRow[] = data.proposals
        .filter((p) => data.votes[p.id] !== undefined)
        .map((p) => {
          const vote = voted.get(p.id);
          const action = { id: p.id, type: p.type };
          return vote
            ? { voted: true as const, action, choice: vote.choice, anchor: vote.anchor, txRef: vote.txRef }
            : { voted: false as const, action };
        });
      if (q.voted !== undefined) rows = rows.filter((r) => r.voted === q.voted);
      return Promise.resolve(paged(rows, q));
    },

    listDelegators: (id, q) =>
      Promise.resolve(paged(data.drepDelegators[id] ?? [], q)),

    getCounts: () =>
      Promise.resolve(
        env({
          totalRegistered: data.dreps.length,
          totalActive: data.dreps.filter((d) => d.status === 'active').length,
          totalInactive: data.dreps.filter((d) => d.status === 'inactive').length,
          anonymous: data.dreps.filter((d) => d.kind === 'anonymous').length,
        }),
      ),
  };

  /* -- proposals ----------------------------------------------------------- */

  const proposals: ProposalsApi = {
    list: async (q) => {
      let rows = data.proposals;
      if (q.type?.length) rows = rows.filter((p) => q.type!.includes(p.type));
      if (q.status?.length) rows = rows.filter((p) => q.status!.includes(p.lifecycle.status));
      if (q.search) {
        const term = q.search.toLowerCase();
        rows = rows.filter((p) => p.id.toLowerCase().includes(term));
      }
      const sort = q.sort ?? 'newest';
      const sorted = [...rows].sort((a, b) =>
        sort === 'oldest' ? epochOf(a) - epochOf(b) : epochOf(b) - epochOf(a),
      );
      return Promise.resolve(paged(sorted.map(withAggregates), q));
    },

    get: async (id, q) => {
      const action = data.proposals.find((p) => p.id === id || p.txHash === id);
      if (!action) throw notFound('proposal', id);
      const full = withAggregates(action);
      if (q?.voterId) {
        const mine = (data.votes[action.id] ?? []).find(
          (v) => 'id' in v.voter && v.voter.id === q.voterId,
        );
        return Promise.resolve(env({ ...full, myVote: mine ?? null }));
      }
      return Promise.resolve(env(full));
    },

    /**
     * The last ENACTED action of the lineage. `null` is the genesis case and is
     * a legitimate answer, not a failure.
     */
    getEnacted: async (lineage) => {
      const head = [...data.proposals]
        .filter((p) => LINEAGE[p.type] === lineage && p.lifecycle.status === 'enacted')
        .sort((a, b) => (b.lifecycle.enactedAt?.epoch ?? 0) - (a.lifecycle.enactedAt?.epoch ?? 0))[0];
      return Promise.resolve(
        env(head ? { id: head.id, txHash: head.txHash, index: head.index } : null),
      );
    },

    listVotes: async (id, q) => {
      const action = data.proposals.find((p) => p.id === id || p.txHash === id);
      if (!action) throw notFound('proposal', id);
      return Promise.resolve(paged(data.votes[action.id] ?? [], q));
    },
  };

  /* -- pools, committee, transactions -------------------------------------- */

  const pools: PoolsApi = {
    list: async (q) => {
      const rows = q.search
        ? data.pools.filter((p) => p.poolId.includes(q.search!))
        : data.pools;
      return Promise.resolve(paged(rows, q));
    },
    get: async (id) => {
      const pool = data.pools.find((p) => p.poolId === id);
      if (!pool) throw notFound('pool', id);
      return Promise.resolve(env(pool));
    },
    listVotes: async (id, q) => {
      const rows = Object.values(data.votes)
        .flat()
        .filter((v) => 'id' in v.voter && v.voter.id === id);
      return Promise.resolve(paged(rows, q));
    },
  };

  const committee: CommitteeApi = {
    getCommittee: async () => {
      if (!data.committee) throw notFound('committee', 'current');
      return Promise.resolve(env(data.committee));
    },
    getMember: async (coldCredential) => {
      const member = data.committee?.members.find((m) => m.coldCredential === coldCredential);
      if (!member) throw notFound('committee member', coldCredential);
      return Promise.resolve(env(member));
    },
    getConstitution: () => Promise.resolve(env(data.constitution)),
  };

  const transactions: TransactionsApi = {
    get: async (txHash) => {
      const known =
        data.proposals.some((p) => p.txHash === txHash) ||
        Object.values(data.votes).flat().some((v) => v.txRef.txHash === txHash);
      return Promise.resolve(env({ txHash, onChain: known }));
    },
  };

  /* -- system -------------------------------------------------------------- */

  const identity: ProviderIdentity = {
    id: 'fixture',
    name: 'Frozen mainnet fixture',
  };

  /**
   * What this provider honours. Note what is NOT here: `protocolParams.epoch`
   * (one frozen epoch), free-text DRep search (no index — that is the index
   * provider's job), and any aggregate representation but `count`.
   */
  const capabilities: ProviderCapabilities = {
    sorts: {
      dreps: ['votingPower', 'registrationDate', 'activity', 'random'],
      proposals: ['newest', 'oldest'],
    },
    filters: { dreps: ['status', 'kind'], proposals: ['type', 'status'] },
    search: ['exactId'],
    voteAggregate: ['count'],
    optionalArguments: [],
  };

  const system: SystemApi = {
    getIdentity: () => Promise.resolve(env(identity)),
    getCapabilities: () => Promise.resolve(env(capabilities)),
    getHealth: () =>
      Promise.resolve(
        env({ status: 'healthy' as const, tip: data.networkInfo.tip, message: `frozen at ${data.capturedAt}` }),
      ),
  };

  return {
    network,
    accounts,
    governance: { dreps, proposals, pools, committee },
    transactions,
    system,
  };
}
