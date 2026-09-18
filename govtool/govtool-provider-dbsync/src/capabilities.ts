/**
 * What this provider can and cannot serve, declared once rather than
 * discovered by a caller hitting `CAPABILITY_UNSUPPORTED`.
 *
 * Every value below is transcribed from a statement in `sql/`, a mapper in
 * `src/mappers/`, or a refusal site in `src/api/`. The 17 SQL files are frozen
 * — they are byte-for-byte the legacy GovTool backend's — so nearly every gap
 * here is `notImplemented` rather than `notInSource`: db-sync the database
 * records the data, and this build has no statement that selects it. That
 * distinction matters to a consumer, because closing a `notImplemented` gap is
 * a new statement in this package, while a `notInSource` gap needs a different
 * source entirely.
 *
 * Two things this file deliberately does NOT do:
 *
 *   - It does not bake in the `utxo_view` fault. That view is absent on the
 *     shared preview db-sync and present on mainnet, so it is a property of the
 *     box, not of the code; see `missingUtxoViewOverride`.
 *   - It does not claim the network. `getCapabilities()` reads `meta` and
 *     passes it in, so a document from a preprod deployment says preprod.
 */

import type {
  AnyCapabilityRefusal,
  CapabilityOverride,
  CapabilityTable,
  DatasetId,
  EntityDeclarations,
  FieldOverride,
  NetworkId,
  PagingSupport,
  ProviderCapabilityDocument,
  SearchSupport,
  Timestamp,
} from '@govtool/data-providers/chain-data';
import { declareCapabilities } from '@govtool/data-providers/chain-data';

export const PROVIDER_ID = 'dbsync';

/** Kept in step with `package.json`; it is what a capability digest is cut over. */
export const DBSYNC_PROVIDER_VERSION = '0.1.0';

/** When this declaration was last reviewed against `sql/` and `src/`. */
export const DBSYNC_CAPABILITIES_REVIEWED_AT: Timestamp =
  '2026-09-18T00:00:00Z';

/* ------------------------------------------------------------------------- */
/* Shared shorthands                                                          */
/* ------------------------------------------------------------------------- */

const ALWAYS = { serves: 'always' } as const;
const ON_EXPAND = { serves: 'onExpand' } as const;

/**
 * A field no bundled statement selects. Silently absent: either it is not a
 * member of the route's `expand` union, or the route never inspects `expand`.
 */
const absent = (note: string) =>
  ({
    serves: 'never',
    cause: 'notImplemented',
    whenRequested: 'ignored',
    note,
  }) as const;

/** As `absent`, and naming it in `expand` throws `CAPABILITY_UNSUPPORTED`. */
const refused = (note: string) =>
  ({
    serves: 'never',
    cause: 'notImplemented',
    whenRequested: 'throws',
    note,
  }) as const;

/** A field db-sync itself does not record. A new statement would not help. */
const unrecorded = (note: string) =>
  ({
    serves: 'never',
    cause: 'notInSource',
    whenRequested: 'ignored',
    note,
  }) as const;

/** Present in db-sync, inexpressible in the contract's type. */
const lossy = (note: string) =>
  ({
    serves: 'never',
    cause: 'representation',
    whenRequested: 'ignored',
    note,
  }) as const;

/** As `lossy`, and the `expand` member that would carry it throws. */
const lossyRefused = (note: string) =>
  ({
    serves: 'never',
    cause: 'representation',
    whenRequested: 'throws',
    note,
  }) as const;

/** Populated on some reads and not others; the note says which. */
const sometimes = (note: string) => ({ serves: 'conditional', note }) as const;

/**
 * Every list read here materialises the whole result set and pages it in
 * process (`src/common/paging.ts`), so paging behaves exactly as the contract
 * describes: no cap, an omitted `limit` really is everything, and `total` is a
 * count of the array rather than an estimate.
 */
const IN_MEMORY_PAGING: PagingSupport = {
  cursor: 'honoured',
  offset: 'honoured',
  maxLimit: null,
  omittedLimitMeans: 'everything',
  total: 'exact',
};

/**
 * `list-dreps.sql` matches a raw credential hash, the CIP-105 `drep_hash.view`,
 * or `given_name ILIKE '%…%'`. An Ada Handle is accepted and simply matches no
 * name — declared `ignored` rather than `rejected` because nothing throws.
 */
const DREP_SEARCH: SearchSupport = {
  modes: { exactId: 'honoured', freeText: 'honoured', adaHandle: 'ignored' },
  // The statement's guard is `COALESCE($1,'') = '' OR …`, so the empty string
  // is the documented "no filter" value, not an error.
  emptyStringAccepted: true,
};

/**
 * `list-proposals.sql` matches `txHash#index` exactly, or title / abstract /
 * motivation / rationale with ILIKE; the list route re-applies the same fields
 * in process. Ada Handles are meaningless for an action and match nothing.
 */
const PROPOSAL_SEARCH: SearchSupport = {
  modes: { exactId: 'honoured', freeText: 'honoured', adaHandle: 'ignored' },
  emptyStringAccepted: true,
};

const ALL_GOV_ACTION_TYPES = {
  ParameterChange: 'honoured',
  HardForkInitiation: 'honoured',
  TreasuryWithdrawals: 'honoured',
  NoConfidence: 'honoured',
  UpdateCommittee: 'honoured',
  NewConstitution: 'honoured',
  InfoAction: 'honoured',
} as const;

/* ------------------------------------------------------------------------- */
/* Datasets                                                                    */
/* ------------------------------------------------------------------------- */

const DBSYNC_DATASETS: CapabilityTable = declareCapabilities(
  {
    /* -- network ------------------------------------------------------------ */
    'network.identity.current': {
      reachability: 'served',
      pollable: true,
    },
    'network.chain.series': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'No bundled statement lists epochs or blocks. db-sync has `epoch` ' +
          'and `block` in full; the legacy GovTool screens never paged them, ' +
          'so the SQL this provider is frozen against has no such query.',
      },
    },
    'network.params.current': {
      reachability: 'served',
      pollable: false, // parameters change at most once per epoch
      caveats: [
        {
          kind: 'precisionLoss',
          contractType: 'Ratio',
          sourceType: 'double precision',
          note:
            'db-sync stores the dvt/pvt thresholds as floats. They are left ' +
            'out of the typed fields rather than rounded back into a ratio; a ' +
            'consumer that needs them reads `ProtocolParams.raw`.',
        },
      ],
    },
    'network.params.asAt': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'get-current-epoch-params.sql is `ORDER BY epoch_no DESC LIMIT 1` ' +
          'and takes no argument. `epoch_param` holds every past epoch, so a ' +
          'parameterised statement would serve this; refusing is preferred to ' +
          'answering a past epoch with the current one.',
      },
    },
    'network.stake.current': {
      reachability: 'served',
      pollable: false,
      // `getStakeDistribution` on this provider takes no arguments at all, so
      // both `epoch` and `basis` are accepted and dropped on the floor. That is
      // `ignored`, not `rejected`: nothing throws.
      basis: { active: 'ignored', live: 'ignored' },
      caveats: [
        {
          kind: 'impliedFilter',
          param: 'basis',
          restrictedTo: ['active'],
          note:
            'get-network-total-stake.sql reads the current epoch`s drep_distr ' +
            'and pool_stat snapshot. The `basis` and `epoch` arguments the ' +
            'contract allows are not read.',
        },
      ],
    },
    'network.treasury.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'No bundled statement reads `ada_pots`. The dashboard tile GovTool ' +
          'renders comes from elsewhere, so the legacy SQL never needed one.',
      },
    },
    // The strength of this provider: get-network-metrics.sql answers all
    // thirteen required counters in ONE statement, including the six Koios
    // needs a walk over every DRep for.
    'network.aggregate.current': {
      reachability: 'served',
      // One statement, but a very wide one: a dozen CTEs, several of them
      // whole-table aggregates. The legacy deployment put a cache warmer in
      // front of it rather than letting the dashboard poll it.
      pollable: false,
    },

    /* -- account ------------------------------------------------------------ */
    'account.identity.current': {
      reachability: 'served',
      // get-account-info.sql, plus one statement per expanded field.
      pollable: true,
      expand: {
        votingPower: 'honoured',
        delegation: 'honoured',
        // `accounts.get` throws for anything else; there is no statement that
        // returns a stake balance, a pool delegation, the DRep behind a stake
        // key, or an Ada Handle.
        balance: 'rejected',
        drep: 'rejected',
        poolDelegation: 'rejected',
        adaHandles: 'rejected',
      },
    },
    'account.stake.current': {
      reachability: 'served',
      pollable: true,
      // There is no `basis` argument on `accounts.getVotingPower`, and
      // get-stake-key-voting-power.sql sums the account's CURRENT utxo and
      // rewards. So the answer is always `live`, and asking for the epoch
      // snapshot is accepted and not applied rather than refused.
      basis: { live: 'honoured', active: 'ignored' },
    },
    'account.stake.asAt': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'get-stake-key-voting-power.sql sums the account`s current UTxO and ' +
          'rewards with no epoch bound; `accounts.getVotingPower{epoch}` is ' +
          'refused rather than answered with the tip figure.',
      },
    },
    'account.delegation.current': {
      reachability: 'served',
      pollable: true,
    },
    'account.delegation.events': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'get-current-delegation.sql is `LIMIT 1` over the newest ' +
          '`delegation_vote` row. db-sync keeps every certificate with its ' +
          'transaction, so the history is one statement away; this build has ' +
          'none, and no pool-delegation statement at all.',
      },
    },
    'account.registration.events': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'get-account-info.sql reduces `stake_registration` and ' +
          '`stake_deregistration` to a single `is_registered` boolean by ' +
          'comparing their MAX(epoch_no); the certificates themselves are not ' +
          'selected.',
      },
    },

    /* -- drep --------------------------------------------------------------- */
    'drep.identity.current': {
      reachability: 'served',
      // The statement is a twenty-CTE crawl over drep_hash, drep_registration,
      // voting_procedure and off_chain_vote_data. GovTool's backend caches it
      // and warms the cache on a timer; a consumer should not poll it.
      pollable: false,
      paging: IN_MEMORY_PAGING,
      // All five, applied in process over the materialised rows. `random` uses
      // the legacy `seededHash`, so a seed pages stably.
      sort: {
        votingPower: 'honoured',
        registrationDate: 'honoured',
        activity: 'honoured',
        status: 'honoured',
        random: 'honoured',
      },
      filters: {
        // Status is derived per row (`deriveStatus`) and then filtered here,
        // so any combination of the three is expressible. Omitting the filter
        // returns every status, so there is no `defaultsTo`.
        status: {
          values: {
            active: 'honoured',
            inactive: 'honoured',
            retired: 'honoured',
          },
        },
        // `deriveKind` splits a DRep from a direct voter using the deposit
        // sign and whether a non-deregistering anchor exists, so both are real.
        kind: { values: { drep: 'honoured', directVoter: 'honoured' } },
      },
      // `dreps.get` throws for every expand but `metadata`; `dreps.list` never
      // inspects `q.expand` at all. The dataset has to answer for both, so it
      // takes the answer that keeps a caller safe — see DBSYNC_FIELD_OVERRIDES
      // for what `list` actually does with each field.
      expand: {
        metadata: 'honoured',
        liveVotingPower: 'rejected',
        delegators: 'rejected',
        activity: 'rejected',
      },
      search: DREP_SEARCH,
      caveats: [
        {
          kind: 'boundedWindow',
          windowDays: 365,
          note:
            '`DRepActivity.votesCast` is the VotesLastYear CTE: distinct ' +
            'actions voted on where `block.time >= now() - INTERVAL 1 year`. ' +
            'It is a trailing window, not a lifetime count, and `sort: ' +
            '"activity"` orders by it.',
        },
        {
          kind: 'notExhaustive',
          note:
            'An exact-id search matches a LOWERCASE raw hex credential, or a ' +
            'CIP-105 `drep_hash.view` whose bech32 prefix is `drep1`. Three ' +
            'id forms therefore return an empty page for a DRep that exists: ' +
            'a CIP-129 id (it clears the `^drep1…` guard and is then compared ' +
            'against a CIP-105 view), a `drep_script1…` CIP-105 id (it fails ' +
            'that guard outright, so a script DRep is unreachable by any ' +
            'bech32 id), and an upper-case hex credential (the statement ' +
            'compares it to `encode(raw, "hex")`). Resolve to a lowercase ' +
            'hash before searching.',
        },
      ],
    },
    'drep.registration.current': {
      reachability: 'served',
      pollable: true,
    },
    'drep.registration.events': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'Both DRep statements collapse `drep_registration` to its newest row ' +
          '(`DISTINCT ON (drep_hash_id) … ORDER BY tx_id DESC`). The ' +
          'register / update / retire certificates are all in the table; no ' +
          'bundled statement lists them.',
      },
    },
    'drep.stake.current': {
      reachability: 'served',
      // `getVotingPowers(ids)` runs get-filtered-dreps-voting-power.sql once
      // PER ID — the legacy loop, because the statement takes a single pair.
      // The no-ids form is a single statement, but a dataset gets one cost and
      // the worst one is the honest one.
      pollable: false,
      basis: {
        active: 'honoured',
        // get-voting-power.sql reads `drep_distr`, which is an epoch snapshot.
        // `getVotingPower{basis: 'live'}` throws rather than returning the
        // snapshot under a live label.
        live: 'rejected',
      },
      batch: {
        explicitIds: 'honoured',
        allIds: 'honoured',
        // No cap, because there is no upstream request budget — but see `cost`.
      },
      caveats: [
        {
          kind: 'notExhaustive',
          note:
            'get-dreps-voting-power-list.sql is `DISTINCT ON (raw)`, and both ' +
            'predefined options have a NULL `raw`, so the all-ids form ' +
            'collapses `drep_always_abstain` and ' +
            '`drep_always_no_confidence` into a single row. The legacy API ' +
            'had the same hole and the SQL is frozen.',
        },
      ],
    },
    'drep.stake.series': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'get-voting-power.sql is `ORDER BY epoch_no DESC LIMIT 1`. ' +
          '`drep_distr` holds one row per DRep per epoch, so the chart is one ' +
          'statement away; `getVotingPower{fromEpoch,toEpoch}` throws today.',
      },
    },
    'drep.delegation.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'No bundled statement reads `delegation_vote` by DRep. The reverse ' +
          'direction — one account`s current DRep — is served by ' +
          'get-current-delegation.sql.',
      },
    },
    // The registry marks this a universal gap. On db-sync it is not: every
    // `delegation_vote` row carries its transaction and block, so joins and
    // leaves ARE recorded. What is missing is a statement, which is why this
    // is `notImplemented` rather than `notInSource`.
    'drep.delegation.events': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'db-sync records each `delegation_vote` certificate with its tx and ' +
          'block, so the join/leave timeline exists in the database. No ' +
          'bundled statement selects it.',
      },
    },
    'drep.ballot.current': {
      reachability: 'served',
      // get-votes.sql, then get-drep-info.sql for `has_script`, then
      // list-proposals.sql ONCE PER VOTE to resolve the action.
      pollable: false,
      paging: IN_MEMORY_PAGING,
      // `listVotes` never reads `q.sort`; it paginates the statement's own
      // `DISTINCT ON … ORDER BY` output. Every key is therefore accepted and
      // silently not applied, which is the one state a UI must never offer.
      sort: { newest: 'ignored', oldest: 'ignored', votingPower: 'ignored' },
      // Same story: `q.vote` and `q.proposalType` are never read.
      filters: {
        vote: { values: { yes: 'ignored', no: 'ignored', abstain: 'ignored' } },
        proposalType: {
          values: {
            ParameterChange: 'ignored',
            HardForkInitiation: 'ignored',
            TreasuryWithdrawals: 'ignored',
            NoConfidence: 'ignored',
            UpdateCommittee: 'ignored',
            NewConstitution: 'ignored',
            InfoAction: 'ignored',
          },
        },
      },
      // `VoteListQuery.search` is the third member `listVotes` accepts and
      // never reads, alongside `sort` and the two filters above. Declared
      // rather than omitted, because an omitted control reads as "refused"
      // while this one is the `ignored` trap: the call succeeds and the term
      // is dropped.
      search: {
        modes: {
          exactId: 'ignored',
          freeText: 'ignored',
          adaHandle: 'ignored',
        },
        emptyStringAccepted: true,
      },
      expand: {
        // get-votes.sql selects no `drep_distr` amount, so the power applied
        // to a vote is not recoverable; asking throws.
        votingPower: 'rejected',
        rationale: 'honoured',
        proposal: 'honoured',
      },
      caveats: [
        {
          kind: 'impliedFilter',
          param: 'status',
          restrictedTo: ['live'],
          note:
            'Each vote`s action is resolved through list-proposals.sql, which ' +
            'returns live actions only, and a vote whose action is no longer ' +
            'live is DROPPED from the result. A DRep`s record therefore ' +
            'shrinks as actions conclude. The legacy API behaved identically.',
        },
        {
          kind: 'impliedFilter',
          param: 'includeSuperseded',
          restrictedTo: ['false'],
          note:
            'get-votes.sql is `DISTINCT ON (proposal, voter)` ordered by ' +
            'newest, so a re-vote hides the vote it replaced. ' +
            '`includeSuperseded: true` is accepted and cannot be honoured.',
        },
      ],
    },
    // Served — but through `governance.dreps.list`, not through the route the
    // registry assigns this dataset. list-dreps.sql carries `votes_last_year`;
    // get-drep-info.sql carries no counters at all. See DBSYNC_FIELD_OVERRIDES.
    'drep.aggregate.current': {
      reachability: 'served',
      pollable: false,
      caveats: [
        {
          kind: 'boundedWindow',
          windowDays: 365,
          note:
            'Reached through `governance.dreps.list` — NOT through ' +
            '`governance.dreps.get`, the route the registry assigns this ' +
            'dataset, which selects no counters and throws for ' +
            '`expand: "activity"`. See the `DRep.activity` field overrides. ' +
            '`votesCast` counts distinct actions voted on in the trailing ' +
            'year. `notVotedCount` and `participationRate` are not computed, ' +
            'so a participation percentage cannot be rendered.',
        },
      ],
    },

    /* -- pool --------------------------------------------------------------- */
    'pool.identity.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'The legacy SQL touches `pool_stat` only to weight an action`s SPO ' +
          'tally (list-proposals.sql) and to total SPO stake ' +
          '(get-network-total-stake.sql). Nothing reads `pool_hash` or ' +
          '`pool_metadata_ref`, so a pool cannot be returned as a voter.',
      },
    },
    'pool.ballot.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'get-votes.sql is keyed on a DRep credential ' +
          '(`WHERE drep_hash.raw = decode($1,"hex")`); `voting_procedure` ' +
          'holds the pool votes too, but no bundled statement reads them by ' +
          '`pool_voter`.',
      },
    },

    /* -- committee & constitution -------------------------------------------- */
    'committee.identity.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'get-network-metrics.sql counts committee members and reads the ' +
          'quorum, and list-proposals.sql resolves cold credentials for an ' +
          'UpdateCommittee body — but neither returns the membership, and no ' +
          'statement resolves a hot credential or a single member.',
      },
    },
    'constitution.identity.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'No bundled statement reads `constitution`. The legacy GovTool ' +
          'frontend fetched the constitution document from IPFS directly.',
      },
    },
    'constitution.identity.events': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'Successive constitutions would come from `constitution` joined to ' +
          'the enacting proposal; no bundled statement reads either.',
      },
    },

    /* -- proposal ------------------------------------------------------------ */
    'proposal.identity.current': {
      reachability: 'served',
      pollable: false, // ~400 lines of CTEs; the legacy deployment cached it
      paging: IN_MEMORY_PAGING,
      sort: {
        newest: 'honoured',
        oldest: 'honoured',
        soonestToExpire: 'honoured',
        // Legacy MostYesVotes: the DRep and SPO yes-stake plus the CC yes
        // head-count, added together. Reproduced, mixed units and all.
        mostYesVotes: 'honoured',
        // NOT rejected. The sort switch has no case for it and falls through
        // `default: return copied`, so the list comes back in statement order
        // and is presented as sorted. A UI must not offer this.
        highestParticipation: 'ignored',
      },
      filters: {
        type: { values: ALL_GOV_ACTION_TYPES },
        status: {
          // list-proposals.sql's first CTE requires `expiration > MAX(epoch)`
          // with no ratified / enacted / expired / dropped epoch, so the set is
          // live by construction. Anything else throws.
          values: {
            live: 'honoured',
            ratified: 'rejected',
            enacted: 'rejected',
            expired: 'rejected',
            dropped: 'rejected',
          },
          maxSelected: 1,
          defaultsTo: 'live',
        },
      },
      expand: {
        // Both are already on every row, whether or not they are named.
        tallies: 'honoured',
        metadata: 'honoured',
        // `proposals.list` throws for these three. `proposals.get` never looks
        // at `expand`, so there they are silently ignored — see
        // DBSYNC_FIELD_OVERRIDES.
        thresholds: 'rejected',
        myVote: 'rejected',
        protocolParams: 'rejected',
      },
      // `list{voterId}`, `get{voterId}` and `expand: 'myVote'` are one
      // capability. get-votes.sql is keyed on a DRep credential and cannot be
      // joined onto the proposal listing, so all three refuse.
      joins: { callerVote: 'rejected' },
      search: PROPOSAL_SEARCH,
      refusedRoutes: [
        {
          route: 'governance.proposals.listByTx',
          unavailable: {
            kind: 'notImplemented',
            scope: 'source',
            reason:
              'list-proposals.sql binds `txHash#index`, never a bare tx hash, ' +
              'so it cannot return every action a transaction submitted.',
          },
        },
      ],
      caveats: [
        {
          kind: 'impliedFilter',
          param: 'status',
          restrictedTo: ['live'],
          note:
            'The statement returns live actions only. Every element therefore ' +
            'has `lifecycle.status: "live"` and null ratified / enacted / ' +
            'expired / dropped stamps — that is the query`s shape, not a ' +
            'statement about the action.',
        },
        {
          kind: 'precisionLoss',
          contractType: 'Ratio',
          sourceType: 'double precision',
          note:
            'list-proposals.sql renders an UpdateCommittee threshold as a ' +
            'float, so no typed `GovActionBody` is built for that type. ' +
            '`rawBody` carries the statement`s own rendering.',
        },
        {
          kind: 'notExhaustive',
          note:
            '`proposals.get` accepts a CIP-129 id and converts it, but the ' +
            'LIST route`s search matches only the `txHash#index` form, so ' +
            'searching a listing by CIP-129 id returns an empty page.',
        },
      ],
    },
    'proposal.body.current': {
      reachability: 'served',
      pollable: false,
      // `proposals.get` takes an id, not a type, so nothing here throws. The
      // `type` filter is nonetheless the control a consumer reads to learn
      // WHICH typed bodies exist — `govAction.details` derives its detail tabs
      // from exactly this record — so leaving it out reported zero body types
      // for a provider that builds six of the seven. `buildBody` has a case
      // for six; `UpdateCommittee` falls to `return undefined`, which is
      // `ignored` rather than `rejected` because the call still succeeds and
      // simply comes back without `GovAction.body`.
      filters: {
        type: {
          values: {
            ParameterChange: 'honoured',
            HardForkInitiation: 'honoured',
            TreasuryWithdrawals: 'honoured',
            NoConfidence: 'honoured',
            NewConstitution: 'honoured',
            InfoAction: 'honoured',
            UpdateCommittee: 'ignored',
          },
        },
      },
      caveats: [
        {
          kind: 'precisionLoss',
          contractType: 'Ratio',
          sourceType: 'double precision',
          note:
            'Six of the seven action types get a typed body. UpdateCommittee ' +
            'does not, because its quorum arrives as a float.',
        },
      ],
    },
    'proposal.tally.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'There is no per-action tally statement. The tallies ARE served, ' +
          'inline on every `proposals.list` and `proposals.get` element, ' +
          'because list-proposals.sql computes them; `getTallies` would need ' +
          'a statement keyed on one action, and there is none.',
      },
    },
    'proposal.ballot.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'get-votes.sql is keyed on a DRep credential, not on an action, so ' +
          'the "who voted on this" listing cannot be answered. ' +
          '`voting_procedure` has the rows; the statement does not select ' +
          'them that way.',
      },
    },
    'proposal.identity.events': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'A lifecycle feed would union the submitting tx, each ' +
          '`voting_procedure` row and the ratified / enacted / expired / ' +
          'dropped epochs. list-proposals.sql exposes the live set only and ' +
          'flattens the rest to nulls.',
      },
    },
    'proposal.outcome.current': {
      reachability: 'served',
      pollable: false,
      filters: {
        type: {
          // get-previous-enacted-…-details.sql is `WHERE gap.type = $1 AND
          // enacted_epoch IS NOT NULL`, so any type is expressible in SQL —
          // but the provider allows only the two the legacy screens compare,
          // because backend-ts silently substituted HardForkInitiation for the
          // rest and handed back the wrong action's body. Refusing beats that.
          values: {
            ParameterChange: 'honoured',
            HardForkInitiation: 'honoured',
            TreasuryWithdrawals: 'rejected',
            NoConfidence: 'rejected',
            UpdateCommittee: 'rejected',
            NewConstitution: 'rejected',
            InfoAction: 'rejected',
          },
          maxSelected: 1,
        },
      },
    },

    /* -- vote ---------------------------------------------------------------- */
    'vote.ballot.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'get-votes.sql requires a DRep credential, so there is no ' +
          'cross-cutting feed and no lookup by vote transaction. A DRep`s own ' +
          'record is served at `governance.dreps.listVotes`.',
      },
    },

    /* -- voter directory ------------------------------------------------------ */
    'voter.identity.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'Role-agnostic resolution would have to try a DRep, a pool and a ' +
          'committee credential. Only the DRep statement exists here, so a ' +
          'resolve that answered would be answering for one role and ' +
          'guessing about two.',
      },
    },
    'voter.identity.list': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'source',
        reason:
          'No bundled statement unions `drep_hash`, `pool_hash` and ' +
          '`committee_hash`; two of the three are not read at all.',
      },
    },

    /* -- transaction & survey -------------------------------------------------- */
    'transaction.identity.current': {
      reachability: 'served',
      pollable: true, // this is the post-submission polling path
      caveats: [
        {
          kind: 'notExhaustive',
          note:
            'get-transaction-status.sql answers `EXISTS` over `tx`, so an ' +
            'unindexed hash and a mempool hash are indistinguishable and both ' +
            'report `unknown`. There is no `pending` and no `failed`.',
        },
      ],
    },
    // The other strength: no other surveyed provider serves CIP-179 at all,
    // because label-17 metadata is needed as raw CBOR and the HTTP sources
    // decode it to JSON. db-sync keeps `tx_metadata.bytes`.
    'survey.body.current': {
      reachability: 'served',
      pollable: false,
    },

    /* -- metadata (produced by db-sync's off-chain fetcher) --------------------- */
    'drep.metadata.current': {
      reachability: 'served',
      pollable: false,
    },
    'proposal.metadata.current': {
      reachability: 'served',
      pollable: false,
    },
    // Served, but reached through `governance.dreps.listVotes` rather than the
    // `governance.votes.get` the registry assigns: get-votes.sql joins
    // `voting_anchor`, so the rationale's anchor is known. Its BODY is not —
    // no off_chain_vote_data join — so the projection is always `pending`
    // with an empty body.
    'vote.metadata.current': {
      reachability: 'served',
      pollable: false,
      caveats: [
        {
          kind: 'notExhaustive',
          note:
            'get-votes.sql selects the anchor url and hash but joins no ' +
            'off_chain_vote_data, so a vote rationale is always status ' +
            '`pending` with an empty body even when db-sync has fetched the ' +
            'document.',
        },
      ],
    },
  },
  // Nothing outstanding: every dataset above was read against `sql/` and
  // `src/api/` for this declaration.
  [] as readonly DatasetId[],
);

/* ------------------------------------------------------------------------- */
/* Entities — exhaustive over every optional field                            */
/* ------------------------------------------------------------------------- */

const DBSYNC_ENTITIES: EntityDeclarations = {
  NetworkInfo: {
    fields: {
      // db-sync's `meta` table has network_name, start_time and version — no
      // protocol magic and no era. Neither is recorded anywhere in the schema.
      networkMagic: unrecorded('db-sync`s `meta` table has no magic column.'),
      era: unrecorded('db-sync records no era name.'),
    },
  },
  EpochSummary: {
    fields: {
      firstBlock: absent('network.listEpochs has no statement.'),
      lastBlock: absent('network.listEpochs has no statement.'),
    },
  },
  BlockSummary: {
    fields: { txCount: absent('network.listBlocks has no statement.') },
  },
  ProtocolParams: {
    // `raw` is ROW_TO_JSON(epoch_param) and is always complete; the typed
    // fields are lifted from it only when the column parses as a number.
    fields: {
      keyDeposit: ALWAYS,
      poolDeposit: ALWAYS,
      minFeeA: ALWAYS,
      minFeeB: ALWAYS,
      coinsPerUtxoByte: ALWAYS,
      protocolVersion: ALWAYS,
      // Conway columns. NULL on a pre-Conway `epoch_param` row and therefore
      // dropped by the mapper — unreachable in practice, since this route only
      // ever returns the CURRENT epoch, but true of the mapping.
      govActionDeposit: sometimes('Conway column; absent on a pre-Conway row.'),
      drepDeposit: sometimes('Conway column; absent on a pre-Conway row.'),
      minFeeRefScriptCostPerByte: sometimes(
        'Conway column; absent on a pre-Conway row.',
      ),
      govActionLifetime: sometimes(
        'Conway column; absent on a pre-Conway row.',
      ),
      drepActivity: sometimes('Conway column; absent on a pre-Conway row.'),
      committeeMinSize: sometimes('Conway column; absent on a pre-Conway row.'),
      committeeMaxTermLength: sometimes(
        'Conway column; absent on a pre-Conway row.',
      ),
      dvt: lossy(
        'Stored as double precision; a float cannot become an exact Ratio. ' +
          'Read `raw.dvt_*` and decide there.',
      ),
      pvt: lossy(
        'Stored as double precision; a float cannot become an exact Ratio. ' +
          'Read `raw.pvt_*` and decide there.',
      ),
    },
  },
  StakeDistribution: {
    fields: {
      totalStakeControlledByDReps: ALWAYS,
      totalStakeControlledBySPOs: ALWAYS,
      alwaysAbstainVotingPower: ALWAYS,
      alwaysNoConfidenceVotingPower: ALWAYS,
      epoch: absent(
        'get-network-total-stake.sql resolves the current epoch in a CTE and ' +
          'does not project it.',
      ),
      // The one figure a tally percentage needs as a denominator.
      totalActiveStake: absent(
        'The statement totals DRep and SPO stake but never `epoch_stake`, so ' +
          'the tally denominator is not available.',
      ),
      totalLiveStake: absent('No statement sums live stake.'),
    },
  },
  Treasury: {
    fields: { delta: absent('network.getTreasury has no statement.') },
  },
  StakeBalance: {
    fields: {
      // `Account.balance` is refused, and DRepDelegator is on a refused
      // dataset, so no read here ever produces a StakeBalance.
      utxo: absent('No statement returns a stake balance breakdown.'),
      rewards: absent('No statement returns a stake balance breakdown.'),
      rewardsRest: absent('No statement returns a stake balance breakdown.'),
    },
  },
  VotingPower: {
    fields: {
      epoch: absent(
        'get-voting-power.sql selects `amount` alone, and the stake-key ' +
          'statement is not epoch-bound at all.',
      ),
      share: absent('No statement divides a power by a total.'),
    },
  },
  Account: {
    fields: {
      providerId: ALWAYS, // stake_address.id
      votingPower: ON_EXPAND,
      delegation: ON_EXPAND,
      balance: refused('No statement returns the account`s UTxO and rewards.'),
      poolDelegation: refused(
        'No statement reads `delegation` (stake → pool).',
      ),
      drep: refused(
        'Resolving the DRep behind a stake key is a second statement.',
      ),
      adaHandles: refused('No statement resolves Ada Handles.'),
      // Not members of `AccountExpand`, so they cannot even be asked for.
      latestRegistration: absent(
        'get-account-info.sql reduces the certificates to one `is_registered` ' +
          'boolean.',
      ),
      latestDeregistration: absent(
        'get-account-info.sql reduces the certificates to one `is_registered` ' +
          'boolean.',
      ),
    },
  },
  Delegation: {
    fields: {
      since: absent(
        'get-current-delegation.sql returns the certificate`s tx hash but ' +
          'joins no block, so it has neither the epoch nor the time.',
      ),
    },
  },
  PoolDelegation: { fields: {} },
  StakeRegistrationEvent: {
    fields: {
      at: absent('accounts.listStakeEvents has no statement.'),
      slot: absent('accounts.listStakeEvents has no statement.'),
      block: absent('accounts.listStakeEvents has no statement.'),
    },
  },
  DelegationHistoryEvent: {
    fields: {
      at: absent('accounts.listDelegationHistory has no statement.'),
      from: absent('accounts.listDelegationHistory has no statement.'),
    },
  },
  DRep: {
    fields: {
      // The directory selects `drep_hash.view`; the single-DRep statement does
      // not, so `dreps.get` returns a DRep with no CIP-105 id. Overridden per
      // route below.
      cip105Id: sometimes(
        'Selected by list-dreps.sql, not by get-drep-info.sql.',
      ),
      registrationByKind: sometimes(
        'Only get-drep-info.sql splits the DRep and direct-voter ' +
          'registrations; the directory row carries one registration.',
      ),
      activity: sometimes(
        'list-dreps.sql carries `votes_last_year`; get-drep-info.sql selects ' +
          'no counters.',
      ),
      isCip119Compliant: absent(
        'get-network-metrics.sql counts CIP-119 compliant DReps in aggregate, ' +
          'but no statement flags one. Nothing here validates a document ' +
          'against the standard.',
      ),
      // The distinction the whole FieldSupport split exists for: asking
      // `dreps.get` for this throws, asking `dreps.list` is ignored.
      liveVotingPower: refused(
        'db-sync exposes `drep_distr`, an epoch snapshot. A live figure would ' +
          'mean summing every delegator`s current balance; no statement does.',
      ),
      delegators: refused('No statement counts a DRep`s delegators.'),
      adaHandles: absent('No statement resolves Ada Handles.'),
    },
  },
  Registration: {
    fields: {
      registrationTx: ALWAYS,
      status: sometimes(
        '`deriveStatus` needs the `active` flag and the deposit sign, which ' +
          'only list-dreps.sql computes. `dreps.get` returns a registration ' +
          'with no status.',
      ),
      registeredAt: sometimes(
        'list-dreps.sql carries newestRegister.time; get-drep-info.sql joins ' +
          'no block for the registration certificate.',
      ),
      retirementTx: sometimes(
        'Only get-drep-info.sql selects the retirement certificate.',
      ),
      retiredAt: absent(
        'Neither DRep statement joins a block to the retirement certificate, ' +
          'so there is no retirement time.',
      ),
    },
  },
  DRepActivity: {
    fields: {
      notVotedCount: absent(
        'Would need the set of actions votable during each DRep`s ' +
          'registration; the VotesLastYear CTE counts only what was voted on.',
      ),
      participationRate: absent('Derived from notVotedCount; see that field.'),
      lastVotedAt: absent(
        'The LatestVoteEpoch CTE feeds the `active` computation inside the ' +
          'statement and is not projected.',
      ),
      inactiveFromEpoch: absent(
        '`active` is computed in SQL from drep_activity and the latest vote ' +
          'or registration epoch; the boundary itself is not selected.',
      ),
    },
  },
  DRepDelegator: { fields: {} },
  DRepHistoryEvent: {
    fields: {
      at: absent('governance.dreps.listHistory has no statement.'),
      anchor: absent('governance.dreps.listHistory has no statement.'),
      changes: absent('governance.dreps.listHistory has no statement.'),
    },
  },
  DRepVotingPowerEntry: {
    // Both voting-power list statements LEFT JOIN off_chain_vote_drep_data,
    // so the key is always present (null when there is no metadata).
    fields: { givenName: ALWAYS },
  },
  SpoVoter: {
    fields: {
      cip105Id: absent('governance.pools.* has no statement.'),
      ticker: absent('governance.pools.* has no statement.'),
      name: absent('governance.pools.* has no statement.'),
      liveStake: absent('governance.pools.* has no statement.'),
      activeStake: absent('governance.pools.* has no statement.'),
      pledge: absent('governance.pools.* has no statement.'),
    },
  },
  CommitteeMember: {
    fields: {
      cip105Id: absent('governance.committee.* has no statement.'),
    },
  },
  Committee: { fields: {} },
  Constitution: { fields: {} },
  GovAction: {
    fields: {
      providerId: ALWAYS, // gov_action_proposal.id
      rawBody: ALWAYS, // the statement's pre-shaped `description`
      tallies: ALWAYS, // every tally column is on the row, expand or not
      body: sometimes(
        'Typed for six of the seven types. UpdateCommittee never gets one ' +
          '(float quorum), and a pre-shaped description that does not match ' +
          'the expected shape yields none either — `rawBody` always carries it.',
      ),
      deposit: absent(
        'list-proposals.sql does not select `gov_action_proposal.deposit`.',
      ),
      depositReturnAddress: absent(
        'list-proposals.sql does not join the return address.',
      ),
      proposedBy: absent('list-proposals.sql does not select the proposer.'),
      // `proposals.list` throws for `expand: 'protocolParams'` and
      // `'myVote'`; `proposals.get` ignores `expand` entirely — overridden
      // per route below.
      protocolParamsAtSubmission: refused(
        'No statement joins `epoch_param` onto a proposal.',
      ),
      protocolParamsAtEnactment: refused(
        'No statement joins `epoch_param` onto a proposal.',
      ),
      myVote: refused(
        'get-votes.sql is keyed on a DRep credential and cannot be joined ' +
          'onto the proposal listing.',
      ),
    },
  },
  GovActionLifecycle: {
    // creator_block.time and latest_epoch.no are on every row.
    fields: { submitted: ALWAYS },
  },
  GovActionActivityEvent: {
    fields: {
      voter: absent('governance.proposals.listActivity has no statement.'),
      vote: absent('governance.proposals.listActivity has no statement.'),
    },
  },
  EnactedActionSummary: {
    fields: {
      submittedTx: ALWAYS,
      rawBody: sometimes(
        'Set when `gov_action_proposal.description` is non-null; unlike the ' +
          'list statement this is db-sync`s raw JSON, not a pre-shaped one.',
      ),
      enactedAt: absent(
        'The statement filters on `enacted_epoch IS NOT NULL` but does not ' +
          'select it.',
      ),
      body: absent(
        'No typed body is built here: the raw description is not pre-shaped ' +
          'the way the list statement`s is.',
      ),
    },
  },
  RoleTally: {
    fields: {
      // The ledger weighs DReps and SPOs by stake and the committee by head,
      // and the mapper fills only the field that applies to the role.
      stake: sometimes('Set for `drep` and `spo`, never for `cc`.'),
      count: sometimes('Set for `cc`, never for `drep` or `spo`.'),
      notVotedStake: absent(
        'The statement sums the votes cast; it never computes the eligible ' +
          'but unvoted remainder.',
      ),
      totalEligibleStake: absent(
        'The denominator is `epoch_stake` per action; no statement joins it.',
      ),
      // Both are what `expand: 'thresholds'` would carry, and
      // `proposals.list` THROWS for that member — so the entity answer is the
      // throwing one and `governance.proposals.get`, which never inspects
      // `expand`, is the override below. Same split as `GovAction.myVote`.
      threshold: lossyRefused(
        'The per-type thresholds are the float dvt/pvt columns; see ' +
          'ProtocolParams.',
      ),
      passing: lossyRefused(
        'Derived from a threshold that cannot be represented.',
      ),
    },
  },
  VoteRecord: {
    // block.epoch_no and block.time are selected by get-votes.sql.
    fields: { at: ALWAYS },
    misreported: [
      {
        field: 'votingPower',
        sends: 'null',
        wouldMean: 'the vote was counted with no voting power behind it',
        note:
          'get-votes.sql joins no `drep_distr`, so the power applied to the ' +
          'vote is unknown. The contract makes the field required-nullable, ' +
          'so `null` is sent where "not read" is meant.',
      },
    ],
  },
  GovernanceMetrics: {
    fields: {
      totalDRepDistribution: sometimes(
        '`total_drep_distr` is nullable; the key is omitted when it is NULL.',
      ),
      epoch: absent(
        'The statement resolves the current epoch in a CTE and does not ' +
          'project it.',
      ),
      totalLiveGovernanceActions: absent(
        '`total_gov_action_proposals` counts every proposal ever; the live ' +
          'subset is not counted separately.',
      ),
      totalSpoVotes: absent(
        'TotalDRepVotes counts `voting_procedure` rows with a drep_voter; ' +
          'there is no pool_voter counterpart.',
      ),
      totalCcVotes: absent(
        'There is no committee_voter counterpart to TotalDRepVotes.',
      ),
      treasury: absent('No statement reads `ada_pots`.'),
    },
    misreported: [
      {
        field: 'committee',
        sends: '{ quorum: { numerator: 0, denominator: 0 } }',
        wouldMean: 'the committee quorum is zero out of zero',
        note:
          '`CommitteeThreshold` is `SELECT c.* FROM committee … LIMIT 1`, and ' +
          'both quorum columns are NULLable on that row. `mapMetricsRow` reads ' +
          'them through `toStrictInteger`, and `Number(null)` is `0`, which is ' +
          'an integer — so a missing threshold is reported as the ratio 0/0 ' +
          'rather than refused. `committee.size` is a real count either way.',
      },
    ],
  },
  TransactionState: {
    fields: {
      votingProcedures: ALWAYS, // json_agg(voting_procedure.*), `[]` when none
      effects: absent(
        'The raw `voting_procedure` rows are returned untyped; resolving each ' +
          'into a voter and an action would be a statement per row.',
      ),
      confirmations: absent(
        'get-transaction-status.sql answers EXISTS; it joins no block and so ' +
          'cannot measure depth.',
      ),
      includedAt: absent('The statement joins no block.'),
    },
  },
  SurveyDefinition: { fields: {} },
};

/* ------------------------------------------------------------------------- */
/* Field overrides — where two routes reading one dataset disagree             */
/* ------------------------------------------------------------------------- */

/**
 * `governance.dreps.list` and `governance.dreps.get` read the same dataset from
 * two different statements, and they disagree about seven fields. The dataset's
 * `expand` record has to take the safe answer for both; these say what each
 * route actually does, which is the difference between "the column is missing"
 * and "the call throws".
 */
const DBSYNC_FIELD_OVERRIDES: readonly FieldOverride[] = [
  {
    entity: 'DRep',
    field: 'liveVotingPower',
    route: 'governance.dreps.list',
    support: {
      serves: 'never',
      cause: 'notImplemented',
      whenRequested: 'ignored',
      note:
        '`dreps.list` never inspects `q.expand`, so this comes back 200 with ' +
        'the key simply missing.',
    },
    note: 'list ignores the request; get throws for it.',
  },
  {
    entity: 'DRep',
    field: 'delegators',
    route: 'governance.dreps.list',
    support: {
      serves: 'never',
      cause: 'notImplemented',
      whenRequested: 'ignored',
      note: '`dreps.list` never inspects `q.expand`.',
    },
    note: 'list ignores the request; get throws for it.',
  },
  {
    entity: 'DRep',
    field: 'activity',
    route: 'governance.dreps.list',
    // Not `onExpand`: the directory statement always carries
    // `votes_last_year`, so the field is there whether or not it was asked for.
    support: ALWAYS,
    note: 'Always populated on the directory row, expand or not.',
  },
  {
    entity: 'DRep',
    field: 'activity',
    route: 'governance.dreps.get',
    support: {
      serves: 'never',
      cause: 'notImplemented',
      whenRequested: 'throws',
      note: 'get-drep-info.sql selects no vote counters, and asking throws.',
    },
    note: 'The single-DRep read has no activity at all.',
  },
  {
    entity: 'DRep',
    field: 'cip105Id',
    route: 'governance.dreps.list',
    support: ALWAYS,
    note: 'list-dreps.sql selects `drep_hash.view`.',
  },
  {
    entity: 'DRep',
    field: 'cip105Id',
    route: 'governance.dreps.get',
    support: {
      serves: 'never',
      cause: 'notImplemented',
      whenRequested: 'ignored',
      note: 'get-drep-info.sql does not select `view`, so `id` is computed.',
    },
    note: 'The single-DRep read cannot report a CIP-105 id.',
  },
  {
    entity: 'DRep',
    field: 'registrationByKind',
    route: 'governance.dreps.get',
    support: ALWAYS,
    note: 'get-drep-info.sql selects both registrations and both retirements.',
  },
  {
    entity: 'DRep',
    field: 'registrationByKind',
    route: 'governance.dreps.list',
    support: {
      serves: 'never',
      cause: 'notImplemented',
      whenRequested: 'ignored',
      note: 'The directory row carries one registration, not a split.',
    },
    note: 'Only the single-DRep read splits DRep from direct voter.',
  },
  {
    entity: 'Registration',
    field: 'status',
    route: 'governance.dreps.list',
    support: ALWAYS,
    note: 'Derived by `deriveStatus` from `active` and the deposit sign.',
  },
  {
    entity: 'Registration',
    field: 'status',
    route: 'governance.dreps.get',
    support: {
      serves: 'never',
      cause: 'notImplemented',
      whenRequested: 'ignored',
      note: 'get-drep-info.sql computes no activity flag, so no status.',
    },
    note: 'Status is a directory-only field; the status FILTER is too.',
  },
  {
    entity: 'Registration',
    field: 'registeredAt',
    route: 'governance.dreps.list',
    support: ALWAYS,
    note: 'newestRegister.time is on the directory row.',
  },
  {
    entity: 'Registration',
    field: 'registeredAt',
    route: 'governance.dreps.get',
    support: {
      serves: 'never',
      cause: 'notImplemented',
      whenRequested: 'ignored',
      note: 'get-drep-info.sql joins no block for the registration tx.',
    },
    note: 'The single-DRep read knows the certificate, not its time.',
  },
  {
    entity: 'Registration',
    field: 'retirementTx',
    route: 'governance.dreps.get',
    support: ALWAYS,
    note: 'get-drep-info.sql selects both retirement tx hashes.',
  },
  {
    entity: 'Registration',
    field: 'retirementTx',
    route: 'governance.dreps.list',
    support: {
      serves: 'never',
      cause: 'notImplemented',
      whenRequested: 'ignored',
      note: 'The directory row has the latest registration only.',
    },
    note: 'Retirement is a single-DRep-read field.',
  },
  /* -- proposals: `get` never looks at `expand`, `list` refuses it ----------- */
  {
    entity: 'GovAction',
    field: 'myVote',
    route: 'governance.proposals.get',
    support: {
      serves: 'never',
      cause: 'notImplemented',
      whenRequested: 'ignored',
      note:
        '`proposals.get` validates `voterId` and then ignores `expand` ' +
        'entirely, so `expand: ["myVote"]` is accepted and dropped. Passing ' +
        '`voterId` still throws.',
    },
    note: 'get ignores the expand; list throws for it.',
  },
  {
    entity: 'GovAction',
    field: 'protocolParamsAtSubmission',
    route: 'governance.proposals.get',
    support: {
      serves: 'never',
      cause: 'notImplemented',
      whenRequested: 'ignored',
      note: '`proposals.get` never inspects `expand`.',
    },
    note: 'get ignores the expand; list throws for it.',
  },
  {
    entity: 'GovAction',
    field: 'protocolParamsAtEnactment',
    route: 'governance.proposals.get',
    support: {
      serves: 'never',
      cause: 'notImplemented',
      whenRequested: 'ignored',
      note: '`proposals.get` never inspects `expand`.',
    },
    note: 'get ignores the expand; list throws for it.',
  },
  {
    entity: 'RoleTally',
    field: 'threshold',
    route: 'governance.proposals.get',
    support: {
      serves: 'never',
      cause: 'representation',
      whenRequested: 'ignored',
      note: '`proposals.get` never inspects `expand`, so `thresholds` is dropped.',
    },
    note: 'get ignores the expand; list throws for it.',
  },
  {
    entity: 'RoleTally',
    field: 'passing',
    route: 'governance.proposals.get',
    support: {
      serves: 'never',
      cause: 'representation',
      whenRequested: 'ignored',
      note: '`proposals.get` never inspects `expand`, so `thresholds` is dropped.',
    },
    note: 'get ignores the expand; list throws for it.',
  },
];

/* ------------------------------------------------------------------------- */
/* Deployment faults — never part of the constant                             */
/* ------------------------------------------------------------------------- */

/**
 * `get-stake-key-voting-power.sql` joins `utxo_view`. That view exists on a
 * full db-sync and is absent on the shared preview instance, where the
 * statement raises `relation "utxo_view" does not exist` — the fault that
 * shipped as a silent 0 ada on every connected wallet, because the legacy
 * service caught the error and returned 0.
 *
 * It is a property of the box, so it is NOT in the table above. A deployment
 * that has probed for the view passes this to `dbSyncCapabilities`, and it
 * clears when the view comes back without a release.
 */
export function missingUtxoViewOverride(
  observedAt?: Timestamp,
): CapabilityOverride {
  return {
    dataset: 'account.stake.current',
    reachability: 'refused',
    unavailable: {
      kind: 'deploymentFault',
      scope: 'deployment',
      symptom: 'relation "utxo_view" does not exist',
      reason:
        'get-stake-key-voting-power.sql joins `utxo_view`, which this db-sync ' +
        'instance does not provide. `accounts.get` with ' +
        '`expand: ["votingPower"]` fails the same way.',
      ...(observedAt === undefined ? {} : { observedAt }),
    },
  };
}

/* ------------------------------------------------------------------------- */
/* The document                                                               */
/* ------------------------------------------------------------------------- */

/**
 * The declaration `system.getCapabilities()` returns.
 *
 * `network` is a parameter because this provider reads it from `meta` at
 * runtime; `overrides` is a parameter because a deployment fault must not be a
 * compile-time constant.
 */
export function dbSyncCapabilities(
  network: NetworkId,
  overrides: readonly CapabilityOverride[] = [],
): ProviderCapabilityDocument {
  return {
    schemaVersion: 2,
    provider: PROVIDER_ID,
    network,
    providerVersion: DBSYNC_PROVIDER_VERSION,
    generatedAt: DBSYNC_CAPABILITIES_REVIEWED_AT,
    datasets: DBSYNC_DATASETS,
    entities: DBSYNC_ENTITIES,
    fieldOverrides: DBSYNC_FIELD_OVERRIDES,
    unreviewed: [],
    overrides,
    // No out-of-contract methods: everything this provider can answer is a
    // contract route.
    extensions: [],
    metadata: {
      // db-sync's own off-chain fetcher resolves and stores the documents;
      // this provider projects what it stored.
      resolvedBy: 'provider',
      // db-sync checks the anchor hash, which is why a stored row implies
      // "hash ok" — but nothing validates the document against CIP-100 / 108 /
      // 119, which is why `DRep.isCip119Compliant` is never filled.
      validatesAgainstStandard: false,
      // off_chain_vote_fetch_error.fetch_error, on the directory statement.
      carriesFailureMessage: true,
    },
  };
}

/**
 * Refusals this provider's code can throw, as typed values.
 *
 * One entry per `unsupported(...)` site in `src/api/`, so the declaration above
 * and the refusals below can be cross-checked mechanically by
 * `refusalIsDeclared` — see test/capabilities.spec.ts. Kept next to the
 * declaration so that adding a refusal site without declaring it is a visible
 * diff in one file.
 */
export const DBSYNC_REFUSALS: readonly AnyCapabilityRefusal[] = [
  /* -- network.api.ts ------------------------------------------------------- */
  {
    dataset: 'network.params.asAt',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'network.getProtocolParams{epoch}: the statement takes no epoch.',
  },
  {
    dataset: 'network.chain.series',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'network.listEpochs has no statement.',
  },
  {
    dataset: 'network.chain.series',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'network.listBlocks has no statement.',
  },
  {
    dataset: 'network.treasury.current',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'network.getTreasury has no statement.',
  },

  /* -- accounts.api.ts ------------------------------------------------------ */
  {
    dataset: 'account.identity.current',
    control: { kind: 'expand', field: 'balance' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'accounts.get#balance: no statement returns a stake balance.',
  },
  {
    dataset: 'account.identity.current',
    control: { kind: 'expand', field: 'drep' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'accounts.get#drep: resolving the DRep is a second statement.',
  },
  {
    dataset: 'account.identity.current',
    control: { kind: 'expand', field: 'poolDelegation' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'accounts.get#poolDelegation: no statement reads `delegation`.',
  },
  {
    dataset: 'account.identity.current',
    control: { kind: 'expand', field: 'adaHandles' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'accounts.get#adaHandles: no statement resolves Ada Handles.',
  },
  {
    dataset: 'account.stake.asAt',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'accounts.getVotingPower{epoch}: the statement is not epoch-bound.',
  },
  {
    dataset: 'account.delegation.events',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'accounts.listDelegationHistory has no statement.',
  },
  {
    dataset: 'account.registration.events',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'accounts.listStakeEvents has no statement.',
  },

  /* -- governance/dreps.api.ts ---------------------------------------------- */
  {
    dataset: 'drep.identity.current',
    control: { kind: 'expand', field: 'liveVotingPower' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.dreps.get#liveVotingPower: drep_distr is a snapshot.',
  },
  {
    dataset: 'drep.identity.current',
    control: { kind: 'expand', field: 'delegators' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.dreps.get#delegators: no delegator statement.',
  },
  {
    dataset: 'drep.identity.current',
    control: { kind: 'expand', field: 'activity' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.dreps.get#activity: get-drep-info.sql has no counters.',
  },
  {
    dataset: 'drep.stake.series',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason:
      'governance.dreps.getVotingPower{fromEpoch,toEpoch}: the statement is ' +
      'LIMIT 1 on the newest epoch.',
  },
  {
    dataset: 'drep.stake.current',
    control: { kind: 'basis', basis: 'live' },
    cause: 'notImplemented',
    scope: 'source',
    reason:
      'governance.dreps.getVotingPower{basis=live}: drep_distr is the epoch ' +
      'snapshot, and returning it as live would be a lie.',
  },
  {
    dataset: 'drep.ballot.current',
    control: { kind: 'expand', field: 'votingPower' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.dreps.listVotes#votingPower: no drep_distr join.',
  },
  {
    dataset: 'drep.delegation.current',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.dreps.listDelegators has no statement.',
  },
  {
    dataset: 'drep.delegation.events',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.dreps.listDelegationEvents has no statement.',
  },
  {
    dataset: 'drep.registration.events',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.dreps.listHistory has no statement.',
  },

  /* -- governance/metrics.api.ts -------------------------------------------- */
  // `governance.metrics.get{epoch}` throws, and there is no
  // `network.aggregate.asAt` dataset to hang a whole-dataset refusal on — the
  // registry has an `asAt` twin for protocol params and for account stake, but
  // not for the aggregate counters. Filed against the FIELD the refusal is
  // really about: get-network-metrics.sql is anchored to the current epoch and
  // does not project one, so this provider has no epoch dimension on metrics
  // at all.
  {
    dataset: 'network.aggregate.current',
    control: { kind: 'field', entity: 'GovernanceMetrics', field: 'epoch' },
    cause: 'notImplemented',
    scope: 'source',
    reason:
      'governance.metrics.get{epoch}: the statement resolves the current ' +
      'epoch internally, takes no argument and does not project one.',
  },

  /* -- governance/index.ts (voters) ----------------------------------------- */
  {
    dataset: 'voter.identity.current',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.voters.resolve has no SPO or committee statement.',
  },
  {
    dataset: 'voter.identity.list',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.voters.list has no union statement.',
  },

  /* -- governance/committee.api.ts ------------------------------------------ */
  {
    dataset: 'committee.identity.current',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.committee.getCommittee has no statement.',
  },
  {
    dataset: 'committee.identity.current',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.committee.getMember has no statement.',
  },
  {
    dataset: 'constitution.identity.current',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.committee.getConstitution has no statement.',
  },
  {
    dataset: 'constitution.identity.events',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.committee.listConstitutionHistory has no statement.',
  },

  /* -- governance/pools.api.ts ---------------------------------------------- */
  {
    dataset: 'pool.identity.current',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.pools.list has no statement.',
  },
  {
    dataset: 'pool.identity.current',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.pools.get has no statement.',
  },
  {
    dataset: 'pool.ballot.current',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.pools.listVotes has no statement.',
  },

  /* -- governance/votes.api.ts ---------------------------------------------- */
  {
    dataset: 'vote.ballot.current',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.votes.list has no statement.',
  },
  {
    dataset: 'vote.ballot.current',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.votes.get has no statement.',
  },

  /* -- governance/proposals.api.ts ------------------------------------------ */
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'filter', name: 'status', value: 'ratified' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.proposals.list{status}: the statement is live-only.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'filter', name: 'status', value: 'enacted' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.proposals.list{status}: the statement is live-only.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'filter', name: 'status', value: 'expired' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.proposals.list{status}: the statement is live-only.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'filter', name: 'status', value: 'dropped' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.proposals.list{status}: the statement is live-only.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'expand', field: 'thresholds' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.proposals.list#thresholds: float dvt/pvt only.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'expand', field: 'myVote' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.proposals.list#myVote: no per-voter join.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'expand', field: 'protocolParams' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.proposals.list#protocolParams: no epoch_param join.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'join', join: 'callerVote' },
    cause: 'notImplemented',
    scope: 'source',
    reason:
      'governance.proposals.list{voterId} and .get{voterId}: get-votes.sql ' +
      'cannot be joined onto the proposal statement.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'route', route: 'governance.proposals.listByTx' },
    cause: 'notImplemented',
    scope: 'source',
    reason:
      'governance.proposals.listByTx: the statement binds `txHash#index`, ' +
      'never a bare tx hash.',
  },
  {
    dataset: 'proposal.ballot.current',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.proposals.listVotes has no statement.',
  },
  {
    dataset: 'proposal.tally.current',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.proposals.getTallies has no per-action statement.',
  },
  {
    dataset: 'proposal.identity.events',
    control: { kind: 'dataset' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.proposals.listActivity has no statement.',
  },
  {
    dataset: 'proposal.outcome.current',
    control: { kind: 'filter', name: 'type', value: 'TreasuryWithdrawals' },
    cause: 'notImplemented',
    scope: 'source',
    reason:
      'governance.proposals.getEnacted{type}: only ParameterChange and ' +
      'HardForkInitiation are served, rather than substituting the wrong ' +
      'action`s body the way backend-ts did.',
  },
  {
    dataset: 'proposal.outcome.current',
    control: { kind: 'filter', name: 'type', value: 'NoConfidence' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.proposals.getEnacted{type}: see TreasuryWithdrawals.',
  },
  {
    dataset: 'proposal.outcome.current',
    control: { kind: 'filter', name: 'type', value: 'UpdateCommittee' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.proposals.getEnacted{type}: see TreasuryWithdrawals.',
  },
  {
    dataset: 'proposal.outcome.current',
    control: { kind: 'filter', name: 'type', value: 'NewConstitution' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.proposals.getEnacted{type}: see TreasuryWithdrawals.',
  },
  {
    dataset: 'proposal.outcome.current',
    control: { kind: 'filter', name: 'type', value: 'InfoAction' },
    cause: 'notImplemented',
    scope: 'source',
    reason: 'governance.proposals.getEnacted{type}: see TreasuryWithdrawals.',
  },
];
