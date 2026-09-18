/**
 * Worked example: the Koios provider's capability declaration.
 *
 * Never executed and never shipped — type-checked by `npm run typecheck`, which
 * for a contracts package is the meaningful test. It exists for three reasons:
 *
 *   1. It is the skeleton a provider author copies. Every value below is
 *      transcribed from a real refusal site or mapper in
 *      `govtool-provider-koios`.
 *   2. It proves the declaration is writable — that the exhaustive records are
 *      a bearable amount of work, not a theoretical nicety.
 *   3. It exercises the user's three cases end to end: a DRep's delegation
 *      join/leave history, a governance action's vote listing, and sorting that
 *      listing by voting power rather than chronologically.
 */

import type {
  AnyCapabilityRefusal,
  CapabilityTable,
  DatasetId,
  EntityDeclarations,
  FeatureSet,
  FieldOverride,
  PagingSupport,
  ProviderCapabilityDocument,
  SearchSupport,
} from '../src/chain-data';
import {
  declarationProblems,
  declareCapabilities,
  deriveFeatures,
  refusalIsDeclared,
  resolveCapabilities,
} from '../src/chain-data';

/* ------------------------------------------------------------------------- */
/* Shared shorthands                                                          */
/* ------------------------------------------------------------------------- */

const ALWAYS = { serves: 'always' } as const;
const ON_EXPAND = { serves: 'onExpand' } as const;

/** A field the source does not record. Silently absent unless stated. */
const absent = (note: string) =>
  ({
    serves: 'never',
    cause: 'notInSource',
    whenRequested: 'ignored',
    note,
  }) as const;

/** A field the source does not record, and asking for it throws. */
const refused = (note: string) =>
  ({
    serves: 'never',
    cause: 'notInSource',
    whenRequested: 'throws',
    note,
  }) as const;

/** A field that exists upstream but cannot be expressed in the contract's type. */
const lossy = (note: string) =>
  ({
    serves: 'never',
    cause: 'representation',
    whenRequested: 'ignored',
    note,
  }) as const;

const KOIOS_PAGING: PagingSupport = {
  cursor: 'honoured',
  offset: 'honoured',
  maxLimit: 1000, // KOIOS_MAX_PAGE_SIZE
  // NOT 'everything'. The contract says an omitted limit returns everything;
  // on Koios it returns one 1000-row page, and the backend's DRep snapshot
  // shipped a bug by believing the contract.
  omittedLimitMeans: 'oneMaxPage',
  defaultLimit: 1000,
  total: 'exact', // PostgREST content-range
};

/** Koios can look an id up and has no text index and no Ada Handle resolution. */
const ID_ONLY_SEARCH: SearchSupport = {
  modes: { exactId: 'honoured', freeText: 'rejected', adaHandle: 'rejected' },
  // GovTool's backend always sends `search`, so throwing on the empty string
  // would break a call that otherwise works.
  emptyStringAccepted: true,
};

/* ------------------------------------------------------------------------- */
/* Datasets                                                                    */
/* ------------------------------------------------------------------------- */

const KOIOS_DATASETS: CapabilityTable = declareCapabilities(
  {
    /* -- the DRep directory: Koios can list, cannot rank, cannot see live ---- */
    'drep.identity.current': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      // /drep_list exposes only drep_id, hex, has_script and registered, so
      // there is nothing to order 1000 arbitrary rows by. Every key rejected
      // means the UI hides the control rather than presenting an arbitrary
      // page as a ranking.
      sort: {
        votingPower: 'rejected',
        registrationDate: 'rejected',
        activity: 'rejected',
        status: 'rejected',
        random: 'rejected',
      },
      filters: {
        status: {
          values: {
            active: 'honoured',
            inactive: 'honoured',
            retired: 'honoured',
          },
          defaultsTo: 'active',
        },
        kind: {
          // Koios cannot distinguish a direct voter from a DRep.
          values: { drep: 'honoured', directVoter: 'rejected' },
        },
      },
      expand: {
        metadata: 'honoured',
        liveVotingPower: 'rejected',
        delegators: 'approximated',
        activity: 'honoured',
      },
      search: ID_ONLY_SEARCH,
    },

    'drep.registration.current': {
      reachability: 'served',
      pollable: true,
    },
    'drep.registration.events': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
    },

    'drep.stake.current': {
      reachability: 'served',
      pollable: true,
      // Koios reports the epoch snapshot only. The axis has two sides and this
      // is per route: another provider refuses `active` on the delegator list.
      basis: { active: 'honoured', live: 'rejected' },
      batch: {
        explicitIds: 'honoured',
        allIds: 'honoured',
        maxIdsPerRequest: 50,
      },
    },
    'drep.stake.series': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          'Koios reports DRep voting power for the current snapshot only; ' +
          'there is no per-epoch history to range over.',
      },
    },

    'drep.delegation.current': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      basis: { active: 'honoured', live: 'rejected' },
    },

    /* == THE USER'S CASE (a) ================================================ */
    // Declared `notInSource` with `scope: 'source'`, and the REGISTRY says no
    // known source records it, so the derived feature comes out
    // `permanentlyAbsent: true` — the signal to delete the component rather
    // than ship a toggle that is off on every provider forever.
    'drep.delegation.events': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          '/drep_delegators is a snapshot of who delegates now, with no join ' +
          'or leave times; the transitions are not recorded.',
      },
    },

    'drep.ballot.current': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      sort: { newest: 'honoured', oldest: 'honoured', votingPower: 'rejected' },
      filters: {
        vote: {
          values: { yes: 'honoured', no: 'honoured', abstain: 'honoured' },
        },
        proposalType: {
          values: {
            ParameterChange: 'honoured',
            HardForkInitiation: 'honoured',
            TreasuryWithdrawals: 'honoured',
            NoConfidence: 'honoured',
            UpdateCommittee: 'honoured',
            NewConstitution: 'honoured',
            InfoAction: 'honoured',
          },
        },
      },
      expand: {
        votingPower: 'rejected',
        rationale: 'honoured',
        proposal: 'honoured',
      },
    },
    'drep.aggregate.current': {
      reachability: 'served',
      pollable: true,
    },

    /* -- governance actions -------------------------------------------------- */
    'proposal.identity.current': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      // Three of five. Vote weights live on /proposal_voting_summary, which
      // cannot be joined into the listing, so the two weighted orderings are
      // refused and the chronological ones stay — the sort menu loses two
      // items rather than disappearing.
      sort: {
        newest: 'honoured',
        oldest: 'honoured',
        soonestToExpire: 'honoured',
        mostYesVotes: 'rejected',
        highestParticipation: 'rejected',
      },
      filters: {
        type: {
          values: {
            ParameterChange: 'honoured',
            HardForkInitiation: 'honoured',
            TreasuryWithdrawals: 'honoured',
            NoConfidence: 'honoured',
            UpdateCommittee: 'honoured',
            NewConstitution: 'honoured',
            InfoAction: 'honoured',
          },
        },
        status: {
          values: {
            live: 'honoured',
            ratified: 'honoured',
            enacted: 'honoured',
            expired: 'honoured',
            dropped: 'honoured',
          },
          // Status is four separate nullable epoch columns, so any ONE status
          // is expressible and two at once is not. No value-set can say this.
          maxSelected: 1,
        },
      },
      expand: {
        tallies: 'honoured',
        thresholds: 'rejected',
        metadata: 'honoured',
        myVote: 'rejected',
        protocolParams: 'rejected',
      },
      // `{voterId}` on list, `{voterId}` on get and `expand: 'myVote'` are the
      // same capability. One name, declared once.
      joins: { callerVote: 'rejected' },
      search: ID_ONLY_SEARCH,
    },
    'proposal.body.current': {
      reachability: 'served',
      pollable: true,
      filters: {
        type: {
          values: {
            ParameterChange: 'honoured',
            HardForkInitiation: 'honoured',
            TreasuryWithdrawals: 'honoured',
            NoConfidence: 'honoured',
            UpdateCommittee: 'honoured',
            NewConstitution: 'honoured',
            InfoAction: 'honoured',
          },
        },
      },
    },
    'proposal.tally.current': {
      reachability: 'served',
      pollable: true,
      filters: {
        role: {
          values: {
            drep: 'honoured',
            spo: 'honoured',
            cc: 'honoured',
            direct: 'rejected',
          },
        },
      },
    },

    /* == THE USER'S CASE (b) and (c) ======================================== */
    'proposal.ballot.current': {
      reachability: 'served', // (b) the vote listing IS available here
      pollable: true,
      paging: KOIOS_PAGING,
      // (c) chronological only. `votingPower` is refused, so the sort menu
      // keeps two items and loses one — not the whole control.
      sort: { newest: 'honoured', oldest: 'honoured', votingPower: 'rejected' },
      filters: {
        vote: {
          values: { yes: 'honoured', no: 'honoured', abstain: 'honoured' },
        },
        role: {
          values: {
            drep: 'honoured',
            spo: 'honoured',
            cc: 'honoured',
            direct: 'rejected',
          },
        },
        proposalType: {
          values: {
            ParameterChange: 'honoured',
            HardForkInitiation: 'honoured',
            TreasuryWithdrawals: 'honoured',
            NoConfidence: 'honoured',
            UpdateCommittee: 'honoured',
            NewConstitution: 'honoured',
            InfoAction: 'honoured',
          },
        },
      },
      expand: {
        votingPower: 'rejected',
        rationale: 'honoured',
        proposal: 'honoured',
      },
      caveats: [
        {
          kind: 'identifierGranularity',
          note:
            'Koios does not number the voting procedures within a transaction, ' +
            'so two votes cast in one tx are not separable.',
        },
      ],
    },
    'proposal.identity.events': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
    },
    'proposal.outcome.current': {
      reachability: 'served',
      pollable: false,
      filters: {
        type: {
          values: {
            ParameterChange: 'honoured',
            HardForkInitiation: 'honoured',
            TreasuryWithdrawals: 'honoured',
            NoConfidence: 'honoured',
            UpdateCommittee: 'honoured',
            NewConstitution: 'honoured',
            InfoAction: 'honoured',
          },
        },
      },
    },

    'vote.ballot.current': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      sort: { newest: 'honoured', oldest: 'honoured', votingPower: 'rejected' },
      expand: {
        votingPower: 'rejected',
        rationale: 'honoured',
        proposal: 'honoured',
      },
      joins: { voterOfVote: 'honoured', proposalOfVote: 'honoured' },
      search: ID_ONLY_SEARCH,
    },

    /* -- network ------------------------------------------------------------- */
    'network.identity.current': {
      reachability: 'served',
      pollable: true,
    },
    'network.chain.series': {
      reachability: 'served',
      pollable: false,
      paging: KOIOS_PAGING,
    },
    'network.params.current': {
      reachability: 'served',
      pollable: false,
      caveats: [
        {
          kind: 'precisionLoss',
          contractType: 'Ratio',
          sourceType: 'float',
          note:
            'Koios reports the DRep and SPO vote thresholds as floating point; ' +
            'a float cannot honestly be turned back into the on-chain ratio.',
        },
      ],
    },
    'network.params.asAt': {
      reachability: 'served',
      pollable: false,
    },
    'network.stake.current': {
      reachability: 'served',
      pollable: false,
      basis: { active: 'honoured', live: 'rejected' },
    },
    'network.treasury.current': {
      reachability: 'served',
      pollable: false,
    },

    // Served through an extension, refused through `governance.metrics.get`:
    // seven counters are one cheap request each, six need a walk over every
    // DRep. Per-field declaration is what lets the dashboard degrade per tile,
    // and the three fields GovTool actually reads are all in the cheap set.
    'network.aggregate.current': {
      reachability: 'served',
      pollable: false,
      refusedRoutes: [
        {
          route: 'governance.metrics.get',
          unavailable: {
            kind: 'tooExpensive',
            scope: 'source',
            // The cheaper path is named and TYPED: a cost refusal cannot
            // compile without one, because "expensive" and "absent" call for
            // different reactions from a consumer.
            fallback: 'committee.identity.current',
            fallbackMethod: 'metrics.getAvailable',
            reason:
              'Six of the thirteen counters need a walk over every DRep. The ' +
              'seven cheap ones are served by metrics.getAvailable().',
          },
        },
      ],
    },

    /* -- account -------------------------------------------------------------- */
    'account.identity.current': {
      reachability: 'served',
      pollable: true,
      expand: {
        balance: 'honoured',
        votingPower: 'honoured',
        delegation: 'honoured',
        drep: 'rejected',
        poolDelegation: 'honoured',
        adaHandles: 'rejected',
      },
    },
    'account.stake.current': {
      reachability: 'served',
      pollable: true,
    },
    'account.stake.asAt': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason: 'Koios reports account balances at the tip only.',
      },
    },
    'account.delegation.current': {
      reachability: 'served',
      pollable: true,
    },
    'account.delegation.events': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      filters: {
        kind: { values: { governance: 'honoured', pool: 'honoured' } },
      },
    },
    'account.registration.events': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
    },

    /* -- pools, committee, voters --------------------------------------------- */
    'pool.identity.current': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      search: {
        modes: {
          exactId: 'honoured',
          freeText: 'rejected',
          adaHandle: 'rejected',
        },
        emptyStringAccepted: true,
      },
    },
    'pool.ballot.current': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      sort: { newest: 'honoured', oldest: 'honoured', votingPower: 'rejected' },
    },
    'committee.identity.current': {
      reachability: 'served',
      pollable: false,
    },
    'constitution.identity.current': {
      reachability: 'served',
      pollable: false,
    },
    'constitution.identity.events': {
      reachability: 'served',
      pollable: false,
      paging: KOIOS_PAGING,
    },
    'voter.identity.current': {
      reachability: 'served',
      pollable: true,
    },
    'voter.identity.list': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          'Koios indexes DReps, pools and the committee separately; there is ' +
          'no role-agnostic voter listing to page.',
      },
    },

    /* -- transactions and surveys ---------------------------------------------- */
    'transaction.identity.current': {
      reachability: 'served',
      pollable: true,
    },
    // A representation gap filed as one, rather than as a missing route.
    'survey.body.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'representation',
        scope: 'source',
        reason:
          'Koios exposes transaction metadata as decoded JSON only; the ' +
          'contract requires the label-17 payload as CBOR hex.',
        caveat: {
          kind: 'encodingMismatch',
          contractEncoding: 'CBOR hex',
          sourceEncoding: 'decoded JSON',
          note:
            'The same decoded-JSON property is a GAIN for DRep and proposal ' +
            'metadata; it is only a loss where the bytes themselves matter.',
        },
      },
    },

    /* -- metadata (produced by the Metadata Service) ---------------------------- */
    'drep.metadata.current': {
      reachability: 'served',
      pollable: false,
    },
    'proposal.metadata.current': {
      reachability: 'served',
      pollable: false,
    },
    'vote.metadata.current': {
      reachability: 'served',
      pollable: false,
    },
  },
  // Nothing outstanding on Koios. A migrating provider lists here what it has
  // not assessed yet, so the under-claim is a reviewable diff.
  [] as readonly DatasetId[],
);

/* ------------------------------------------------------------------------- */
/* Entities — exhaustive over every optional field                            */
/* ------------------------------------------------------------------------- */

const KOIOS_ENTITIES: EntityDeclarations = {
  NetworkInfo: {
    fields: { networkMagic: ALWAYS, era: ALWAYS },
  },
  EpochSummary: { fields: { firstBlock: ALWAYS, lastBlock: ALWAYS } },
  BlockSummary: { fields: { txCount: ALWAYS } },
  ProtocolParams: {
    fields: {
      govActionDeposit: ALWAYS,
      drepDeposit: ALWAYS,
      keyDeposit: ALWAYS,
      poolDeposit: ALWAYS,
      minFeeA: ALWAYS,
      minFeeB: ALWAYS,
      minFeeRefScriptCostPerByte: ALWAYS,
      coinsPerUtxoByte: ALWAYS,
      govActionLifetime: ALWAYS,
      drepActivity: ALWAYS,
      committeeMinSize: ALWAYS,
      committeeMaxTermLength: ALWAYS,
      dvt: lossy('Stored as floating point; cannot become an exact Ratio.'),
      pvt: lossy('Stored as floating point; cannot become an exact Ratio.'),
      protocolVersion: ALWAYS,
    },
  },
  StakeDistribution: {
    fields: {
      epoch: ALWAYS,
      totalActiveStake: ALWAYS,
      totalLiveStake: absent('Koios reports the epoch snapshot only.'),
      totalStakeControlledByDReps: ALWAYS,
      totalStakeControlledBySPOs: ALWAYS,
      alwaysAbstainVotingPower: ALWAYS,
      alwaysNoConfidenceVotingPower: ALWAYS,
    },
  },
  Treasury: {
    fields: { delta: absent('No previous-epoch balance is joined.') },
  },
  StakeBalance: {
    fields: { utxo: ALWAYS, rewards: ALWAYS, rewardsRest: ALWAYS },
  },
  VotingPower: { fields: { epoch: ALWAYS, share: absent('Not computed.') } },
  Account: {
    fields: {
      providerId: absent('Koios has no internal row id to expose.'),
      balance: ON_EXPAND,
      votingPower: ON_EXPAND,
      delegation: ON_EXPAND,
      poolDelegation: ON_EXPAND,
      latestRegistration: ALWAYS,
      latestDeregistration: ALWAYS,
      drep: refused('Resolving the DRep behind a stake key is a second read.'),
      adaHandles: refused('Koios does not resolve Ada Handles.'),
    },
  },
  Delegation: { fields: { since: ALWAYS } },
  PoolDelegation: { fields: {} },
  StakeRegistrationEvent: {
    fields: {
      at: ALWAYS,
      slot: ALWAYS,
      // /account_updates reports the absolute slot but no block height.
      block: absent('Koios reports the slot, not the block height.'),
    },
  },
  DelegationHistoryEvent: {
    fields: {
      at: ALWAYS,
      from: absent(
        'A delegation listing gives the target each certificate set, not the ' +
          'one it replaced.',
      ),
    },
  },
  DRep: {
    fields: {
      cip105Id: ALWAYS,
      registrationByKind: absent(
        'Koios cannot split DRep and direct-voter registrations on one key.',
      ),
      isCip119Compliant: ALWAYS,
      liveVotingPower: refused('Koios reports the epoch snapshot only.'),
      delegators: ON_EXPAND,
      activity: ON_EXPAND,
      adaHandles: refused('Koios does not resolve Ada Handles.'),
    },
  },
  Registration: {
    fields: {
      status: ALWAYS,
      registeredAt: ALWAYS,
      registrationTx: ALWAYS,
      retiredAt: ALWAYS,
      retirementTx: ALWAYS,
    },
  },
  DRepActivity: {
    fields: {
      notVotedCount: absent(
        'Counting actions a DRep did NOT vote on needs the full votable set ' +
          'per DRep, which Koios cannot produce in one read.',
      ),
      lastVotedAt: ALWAYS,
      inactiveFromEpoch: ALWAYS,
      participationRate: absent('Derived from notVotedCount; see that field.'),
    },
  },
  DRepDelegator: { fields: {} },
  DRepHistoryEvent: {
    fields: {
      at: ALWAYS,
      anchor: ALWAYS,
      changes: absent('No diff is computed.'),
    },
  },
  DRepVotingPowerEntry: { fields: { givenName: ALWAYS } },
  SpoVoter: {
    fields: {
      cip105Id: ALWAYS,
      ticker: ALWAYS,
      name: ALWAYS,
      liveStake: ALWAYS,
      activeStake: ALWAYS,
      pledge: ALWAYS,
    },
  },
  CommitteeMember: { fields: { cip105Id: ALWAYS } },
  Committee: { fields: {} },
  Constitution: { fields: {} },
  GovAction: {
    fields: {
      providerId: absent('Koios has no internal row id to expose.'),
      body: ALWAYS,
      rawBody: ALWAYS,
      deposit: ALWAYS,
      depositReturnAddress: ALWAYS,
      proposedBy: ALWAYS,
      tallies: ON_EXPAND,
      protocolParamsAtSubmission: refused(
        'Parameters in force at submission are not joined onto a proposal.',
      ),
      protocolParamsAtEnactment: refused(
        'Parameters in force at enactment are not joined onto a proposal.',
      ),
      myVote: refused(
        'Koios cannot join a voter onto a proposal listing; read ' +
          '/governance/votes instead.',
      ),
    },
  },
  GovActionLifecycle: { fields: { submitted: ALWAYS } },
  GovActionActivityEvent: { fields: { voter: ALWAYS, vote: ALWAYS } },
  EnactedActionSummary: {
    fields: {
      enactedAt: ALWAYS,
      submittedTx: ALWAYS,
      body: ALWAYS,
      rawBody: ALWAYS,
    },
  },
  RoleTally: {
    fields: {
      stake: ALWAYS,
      count: ALWAYS,
      notVotedStake: absent('No eligible-but-not-voted figure is published.'),
      totalEligibleStake: absent(
        'The denominator would have to be aggregated over the whole ' +
          'distribution per action.',
      ),
      threshold: lossy(
        'The threshold is a float upstream; see ProtocolParams.',
      ),
      passing: lossy('Derived from a threshold that cannot be represented.'),
    },
  },
  VoteRecord: { fields: { at: ALWAYS } },
  GovernanceMetrics: {
    fields: {
      epoch: ALWAYS,
      totalDRepDistribution: ALWAYS,
      totalLiveGovernanceActions: ALWAYS,
      totalSpoVotes: ALWAYS,
      totalCcVotes: ALWAYS,
      treasury: ALWAYS,
    },
    // Required fields Koios cannot compute — why `governance.metrics.get` is in
    // `refusedRoutes` above. Naming them turns "the dashboard is unavailable"
    // into "these six tiles are", and none of them is among the three fields
    // the frontend actually reads.
    unfillable: [
      {
        field: 'uniqueDelegators',
        cause: 'tooExpensive',
        note: 'Needs a walk over every DRep to union their delegators.',
      },
      {
        field: 'totalDelegations',
        cause: 'tooExpensive',
        note: 'Needs a walk over every DRep.',
      },
      {
        field: 'totalActiveDReps',
        cause: 'tooExpensive',
        note: 'Activity is per-DRep; classifying all of them is a walk.',
      },
      {
        field: 'totalInactiveDReps',
        cause: 'tooExpensive',
        note: 'Activity is per-DRep; classifying all of them is a walk.',
      },
      {
        field: 'totalActiveCip119CompliantDReps',
        cause: 'tooExpensive',
        note: 'Needs every DRep metadata document resolved and validated.',
      },
      {
        field: 'totalRegisteredDirectVoters',
        cause: 'notInSource',
        note: 'Koios cannot distinguish a direct voter from a DRep.',
      },
    ],
  },
  TransactionState: {
    fields: {
      confirmations: ALWAYS,
      includedAt: ALWAYS,
      effects: ALWAYS,
      votingProcedures: ALWAYS,
    },
  },
  SurveyDefinition: { fields: {} },
};

/**
 * One field, two answers, because two routes reading one dataset genuinely
 * disagree. Without this the dataset would have to pick one and lie about the
 * other route.
 */
const KOIOS_FIELD_OVERRIDES: readonly FieldOverride[] = [
  {
    entity: 'DRep',
    field: 'delegators',
    route: 'governance.dreps.list',
    support: {
      serves: 'never',
      cause: 'tooExpensive',
      whenRequested: 'ignored',
      note:
        'A per-DRep delegator count on a 1000-row page is one request per row; ' +
        'the single-DRep read serves it.',
    },
    note: 'List and single-read differ on this field.',
  },
];

export const KOIOS_CAPABILITIES: ProviderCapabilityDocument = {
  schemaVersion: 2,
  provider: 'koios',
  network: 'mainnet',
  providerVersion: '0.4.0',
  generatedAt: '2026-09-18T00:00:00Z',
  datasets: KOIOS_DATASETS,
  entities: KOIOS_ENTITIES,
  fieldOverrides: KOIOS_FIELD_OVERRIDES,
  unreviewed: [],
  // Koios has no instance faults today. Blockfrost's four 500s and timeouts
  // land here, emitted by a health probe, never baked into the constant.
  overrides: [],
  extensions: [
    {
      dataset: 'network.aggregate.current',
      method: 'metrics.getAvailable',
      returns: 'partialRecord',
      description:
        'The seven GovernanceMetrics counters Koios serves in one request ' +
        'each, for a dashboard that degrades per tile instead of per page.',
    },
  ],
  metadata: {
    resolvedBy: 'provider',
    validatesAgainstStandard: true,
    carriesFailureMessage: false,
  },
};

/* ------------------------------------------------------------------------- */
/* The drift cross-check                                                      */
/* ------------------------------------------------------------------------- */

/**
 * Refusals a provider's own code throws. In the real package these are
 * collected by the test harness from each `capabilityUnsupported(...)` site;
 * here three of them stand in, one per user case.
 *
 * `refusalIsDeclared` closes the loop that produced 47 refusal keys matching no
 * table entry and 16 table entries matching no refusal site: the thrown value
 * and the declared value are now the same typed object.
 */
const KOIOS_REFUSALS: readonly AnyCapabilityRefusal[] = [
  {
    dataset: 'drep.delegation.events',
    control: { kind: 'dataset' },
    cause: 'notInSource',
    scope: 'source',
    reason: '/drep_delegators is a snapshot with no join or leave times.',
  },
  {
    dataset: 'proposal.ballot.current',
    control: { kind: 'sort', key: 'votingPower' },
    cause: 'notInSource',
    scope: 'source',
    reason:
      'Koios never records the power applied to an individual vote, so it ' +
      'cannot order by it.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'filterCardinality', name: 'status', max: 1 },
    cause: 'notInSource',
    scope: 'source',
    reason:
      'Status is four separate epoch columns; two at once is not expressible.',
  },
];

/** What a provider's `capabilities.spec.ts` asserts. */
export function koiosDriftCheck(): string[] {
  const table = resolveCapabilities(KOIOS_CAPABILITIES);
  const problems = declarationProblems(KOIOS_CAPABILITIES);
  for (const refusal of KOIOS_REFUSALS) {
    if (!refusalIsDeclared(table, KOIOS_CAPABILITIES.entities, refusal)) {
      problems.push(`undeclared refusal: ${refusal.dataset}`);
    }
  }
  return problems;
}

/** What the backend serves to the browser. */
export const KOIOS_FEATURES: FeatureSet = deriveFeatures(
  KOIOS_CAPABILITIES,
  'sha256:example',
);
