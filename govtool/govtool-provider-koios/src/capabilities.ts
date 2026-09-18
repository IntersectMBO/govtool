/**
 * What this provider can and cannot serve, declared against
 * `@govtool/data-providers/chain-data`'s capability layer.
 *
 * Every value here is transcribed from a refusal site, a mapper or a row type
 * in this package. Where a comment cites a reason it is the reason in the
 * code, not a guess from the Koios documentation — the two disagree often
 * enough (`drep_status` vs `registered`, `NewCommittee` vs `UpdateCommittee`)
 * that only the code is authoritative.
 *
 * The shape of Koios' gaps is different from db-sync's. There the limit was a
 * frozen set of SQL statements written for GovTool's screens; here every
 * endpoint Koios publishes is reachable, and the gaps are the fields Koios
 * itself does not record:
 *
 * - **no per-vote voting power** — per-role totals and per-DRep snapshots
 *   exist, the power applied to one vote does not;
 * - **no delegation event stream** — `/drep_delegators` is a snapshot;
 * - **thresholds as floats** — `Ratio` cannot be reconstructed from `0.67`;
 * - **no aggregate over DReps** — `/drep_info` takes explicit ids, so every
 *   "how many DReps are …" question is a walk;
 * - **no direct/sole voter concept** — a GovTool-specific distinction Koios
 *   does not model;
 * - **metadata as decoded JSON** — a *gain* for DRep, proposal and vote
 *   metadata (Koios resolves and validates it) and a loss for CIP-179
 *   surveys, which need the original CBOR.
 *
 * Two things this file says that the old `Record<string, CapabilityLevel>`
 * table could not, and both were wrong in it:
 *
 * 1. `/drep_list` accepts **no** search term at all, not just no free text.
 *    A caller passing an exact DRep id gets `CAPABILITY_UNSUPPORTED`; only
 *    the empty string is tolerated, because GovTool's backend always sends
 *    the parameter.
 * 2. Several options are **ignored**, not refused — `dreps.listVotes` and
 *    `pools.listVotes` take a `VoteListQuery` and read nothing off it but the
 *    page. A UI must not offer those controls; a refusal-only model reported
 *    them as working.
 * 3. A field can be **sent and wrong**. `serves: 'always'` was the easiest
 *    value in this file to get wrong, because nothing fails when a field is
 *    silently absent, and `null` on a required field is worse still: the
 *    contract reads it as "known to be absent on chain", which is not what
 *    Koios means by it for a delegator's join time. Every such case is now
 *    `conditional` or `misreported` and pinned by a test in
 *    `test/capabilities.spec.ts` to the call that produces it.
 */

import type {
  AnyCapabilityRefusal,
  CapabilityTable,
  EntityDeclarations,
  FieldOverride,
  NetworkId,
  PagingSupport,
  ProviderCapabilityDocument,
  SearchSupport,
} from '@govtool/data-providers/chain-data';
import { declareCapabilities } from '@govtool/data-providers/chain-data';

/* ------------------------------------------------------------------------- */
/* Shared shorthands                                                          */
/* ------------------------------------------------------------------------- */

const ALWAYS = { serves: 'always' } as const;
const ON_EXPAND = { serves: 'onExpand' } as const;

/** Populated on some reads and not others. */
const sometimes = (note: string) => ({ serves: 'conditional', note }) as const;

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

/**
 * The reason the governance protocol parameters are conditional: they are
 * Conway columns, and `network.params.asAt` reads arbitrary past epochs.
 */
const CONWAY_ONLY =
  'A Conway column on /epoch_params. `network.params.asAt` serves any past ' +
  'epoch, and for a pre-Conway one the column is null, which mapProtocolParams ' +
  'turns into an absent field rather than a zero.';

/**
 * Paging pushed into PostgREST — `src/common/paging.ts#toKoiosPage`.
 *
 * NOT `everything`. The contract says an omitted `limit` returns everything;
 * `resolveLimit` substitutes `KOIOS_MAX_PAGE_SIZE` instead, because PostgREST
 * caps a response at 1000 rows whatever `limit` says. The backend's DRep
 * snapshot shipped a bug by believing the contract.
 *
 * `total` is `estimated`, not exact: every paged read asks for
 * `count: 'estimated'`, which is Postgres' own table statistics. A numbered
 * paginator built on it will be off; prev/next is the honest UI.
 */
const KOIOS_PAGING: PagingSupport = {
  cursor: 'honoured', // the cursor IS the next offset, as a decimal string
  offset: 'honoured',
  maxLimit: 1000, // KOIOS_MAX_PAGE_SIZE
  omittedLimitMeans: 'oneMaxPage',
  defaultLimit: 1000,
  total: 'estimated',
};

/**
 * Paging over a list this provider assembled in memory —
 * `src/common/paging.ts#paginateLocally`. The whole set is materialised, so an
 * omitted `limit` really does return everything and `total` is a count, not an
 * estimate.
 */
const LOCAL_PAGING: PagingSupport = {
  cursor: 'honoured',
  offset: 'honoured',
  maxLimit: 1000, // resolveLimit still clamps
  omittedLimitMeans: 'everything',
  total: 'exact',
};

/**
 * No search of any kind, but the empty string is accepted as "no filter".
 *
 * `dreps.list` refuses on `q.search !== undefined && q.search !== ''`, so even
 * an exact CIP-129 id is refused — Koios' `/drep_list` has no id filter, only
 * the POST `/drep_info` does, and the listing cannot reach it before paging.
 * The empty string is tolerated because GovTool's backend always sends
 * `search`, and rejecting on `!== undefined` made the directory unreachable.
 */
const NO_SEARCH_EMPTY_OK: SearchSupport = {
  modes: { exactId: 'rejected', freeText: 'rejected', adaHandle: 'rejected' },
  emptyStringAccepted: true,
};

/**
 * No search at all, not even the empty string: `proposals.list`, `votes.list`
 * and `pools.list` all refuse on `q.search !== undefined`.
 */
const NO_SEARCH: SearchSupport = {
  modes: { exactId: 'rejected', freeText: 'rejected', adaHandle: 'rejected' },
  emptyStringAccepted: false,
};

/**
 * Accepted and dropped on the floor. `dreps.listVotes` and `pools.listVotes`
 * never forward `search` anywhere, so a caller gets an unsearched page rather
 * than an error.
 */
const IGNORED_SEARCH: SearchSupport = {
  modes: { exactId: 'ignored', freeText: 'ignored', adaHandle: 'ignored' },
  emptyStringAccepted: true,
};

/** Every governance action type Koios' `proposal_type` column carries. */
const ALL_ACTION_TYPES = {
  ParameterChange: 'honoured',
  HardForkInitiation: 'honoured',
  TreasuryWithdrawals: 'honoured',
  NoConfidence: 'honoured',
  // Koios still spells this `NewCommittee`; `toKoiosType` renames it.
  UpdateCommittee: 'honoured',
  NewConstitution: 'honoured',
  InfoAction: 'honoured',
} as const;

/**
 * `dreps.listVotes` and `pools.listVotes` accept a `VoteListQuery` and read
 * nothing off it but the page — no `vote`, no `proposalType`, no `sort`. The
 * caller gets an unfiltered, chronologically ordered page presented as
 * filtered, which is why these are `ignored` rather than `rejected`.
 */
const IGNORED_ACTION_TYPES = {
  ParameterChange: 'ignored',
  HardForkInitiation: 'ignored',
  TreasuryWithdrawals: 'ignored',
  NoConfidence: 'ignored',
  UpdateCommittee: 'ignored',
  NewConstitution: 'ignored',
  InfoAction: 'ignored',
} as const;

const IGNORED_VOTE_CHOICES = {
  yes: 'ignored',
  no: 'ignored',
  abstain: 'ignored',
} as const;

const ALL_VOTE_CHOICES = {
  yes: 'honoured',
  no: 'honoured',
  abstain: 'honoured',
} as const;

/**
 * `toKoiosVoterRole` maps three roles and throws for `direct`. Koios has no
 * sole-voter concept, so a listing filtered to it cannot be built.
 *
 * Caveat for a reader cross-checking the code: that site throws `INVALID_INPUT`
 * rather than `CAPABILITY_UNSUPPORTED`. It is still a capability boundary and
 * a UI must not offer the option, so it is declared `rejected` here.
 */
const VOTE_LIST_ROLES = {
  drep: 'honoured',
  spo: 'honoured',
  cc: 'honoured',
  direct: 'rejected',
} as const;

/**
 * A vote listing carries the rationale and the action reference on every row
 * whether or not they were asked for, so requesting them is honoured. Per-vote
 * `votingPower` is a different matter: the field is on the record and Koios
 * never fills it, so naming it in `expand` changes nothing — `ignored`, the
 * value that tells a UI to remove the control rather than render an empty one.
 */
const VOTE_EXPANDS = {
  votingPower: 'ignored',
  rationale: 'honoured',
  proposal: 'honoured',
} as const;

/**
 * `rationale` is honoured on all four vote datasets, but it does not MEAN the
 * same thing on all four, and the `expand` axis has no room to say so.
 *
 * /vote_list carries `meta_json`, so `votes.list`, `proposals.listVotes` and
 * `votes.get` return a resolved projection — `status: 'valid'`, with the body.
 * /drep_votes and /pool_votes carry only `meta_url` and `meta_hash`, so
 * `mapDRepVote` and `pools.listVotes` call `projectVoteRationale` with
 * `json: undefined` and the projection comes back `status: 'pending'` with no
 * body, on every row, forever — not "not fetched yet", which is what a
 * consumer reads `pending` as.
 */
const ANCHOR_ONLY_RATIONALE = {
  kind: 'notExhaustive',
  note:
    'The rationale is an ANCHOR ONLY on this route: /drep_votes and ' +
    '/pool_votes do not carry meta_json, so every projection is ' +
    '`status: "pending"` with no body and re-reading will never resolve it. ' +
    'Read governance.votes.list scoped to the voter for the resolved ' +
    'document, which /vote_list does carry.',
} as const;

/* ------------------------------------------------------------------------- */
/* Datasets                                                                    */
/* ------------------------------------------------------------------------- */

const KOIOS_DATASETS: CapabilityTable = declareCapabilities(
  {
    /* -- the DRep directory: Koios can list, cannot rank, cannot search ----- */
    'drep.identity.current': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      // `/drep_list` exposes only drep_id, hex, has_script and registered, so
      // there is nothing to order 1000 arbitrary rows by. `list` refuses on
      // `q.sort !== undefined`, so EVERY member is rejected and the control
      // disappears rather than presenting an arbitrary page as a ranking.
      sort: {
        votingPower: 'rejected',
        registrationDate: 'rejected',
        activity: 'rejected',
        status: 'rejected',
        random: 'rejected',
      },
      filters: {
        status: {
          // Only `registered=eq.true` is pushed into `/drep_list`; active vs
          // inactive needs `/drep_info`, so the classification is applied to
          // the hydrated page. A filtered page can therefore come back short
          // while matches remain further on.
          values: {
            active: 'approximated',
            inactive: 'approximated',
            retired: 'approximated',
          },
          exhaustive: false,
          // No `defaultsTo`: an omitted `status` filters nothing at all, so
          // every status is returned rather than active ones.
        },
        kind: {
          // `list` refuses any `kind` that does not include 'drep'.
          values: { drep: 'honoured', directVoter: 'rejected' },
        },
      },
      expand: {
        metadata: 'honoured', // POST /drep_metadata
        // `get` throws for this; `list` silently ignores it. See the field
        // override on DRep.liveVotingPower below.
        liveVotingPower: 'rejected',
        // `live_delegator_count` is on every `/drep_info` row, so the counter
        // arrives whether or not it was asked for.
        delegators: 'honoured',
        activity: 'honoured', // one extra counting read, on `get` only
      },
      search: NO_SEARCH_EMPTY_OK,
    },

    'drep.registration.current': {
      reachability: 'served',
      // /drep_info + /drep_updates, plus /drep_metadata when expanded.
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
      // `drep_info.amount` is the current row of drep_voting_power_history —
      // the epoch-boundary snapshot. `getVotingPower` refuses `live`.
      basis: { active: 'honoured', live: 'rejected' },
      batch: {
        explicitIds: 'honoured',
        // `getVotingPowers()` with no ids walks `/drep_list` page by page and
        // then hydrates every id. It is served and it is not `batched`: a
        // caller doing this on the public tier meets the rate limiter. There
        // is no per-form cost axis, so the warning lives here.
        allIds: 'honoured',
        // Chunking is by BODY SIZE (4,096 bytes), not by count — a CIP-129
        // DRep id is ~62 bytes, so this is the count that fits, not a limit
        // Koios states.
        maxIdsPerRequest: 62,
      },
    },
    // Koios DOES have the series: `/drep_voting_power_history` is a real
    // per-epoch table, and `getVotingPower` reads it whenever `fromEpoch` or
    // `toEpoch` is given. This is the route db-sync cannot answer at all
    // (`get-voting-power.sql` is `ORDER BY epoch_no DESC LIMIT 1`).
    'drep.stake.series': {
      reachability: 'served',
      pollable: false,
      basis: { active: 'honoured', live: 'rejected' },
      caveats: [
        {
          kind: 'notExhaustive',
          note:
            'The history is read without paging, so a DRep registered for ' +
            'more than 1000 epochs would be truncated at Koios’ page cap ' +
            'before the range filter is applied.',
        },
      ],
    },

    'drep.delegation.current': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      basis: { active: 'honoured', live: 'rejected' },
    },

    // No join or leave times anywhere in Koios, and the REGISTRY says no known
    // source records them, so the derived feature comes out
    // `permanentlyAbsent` — delete the component rather than ship a toggle
    // that is off on every provider forever.
    'drep.delegation.events': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          'Koios has no per-DRep delegation event stream; /drep_delegators ' +
          'is a snapshot of who delegates now, with no join or leave times.',
      },
    },

    'drep.ballot.current': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      // `dreps.listVotes(id, q)` narrows `q` to the page and reads nothing
      // else off it. The rows come back `block_time.desc`, which is `newest`
      // by coincidence rather than by request; everything else is applied to
      // nothing and must not be offered.
      sort: { newest: 'honoured', oldest: 'ignored', votingPower: 'ignored' },
      filters: {
        vote: { values: IGNORED_VOTE_CHOICES },
        proposalType: { values: IGNORED_ACTION_TYPES },
      },
      expand: VOTE_EXPANDS,
      search: IGNORED_SEARCH,
      caveats: [ANCHOR_ONLY_RATIONALE],
    },
    'drep.aggregate.current': {
      reachability: 'served',
      // One extra `/drep_votes` read with `limit=1, count=exact` for the
      // lifetime vote count.
      pollable: true,
    },

    /* -- governance actions -------------------------------------------------- */
    'proposal.identity.current': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      // Three of five. Vote weights live on /proposal_voting_summary, which
      // takes one `_proposal_id` and cannot be joined into the listing, so the
      // two weighted orderings are refused and the chronological ones stay —
      // the sort menu loses two items rather than disappearing.
      sort: {
        newest: 'honoured',
        oldest: 'honoured',
        soonestToExpire: 'honoured',
        mostYesVotes: 'rejected',
        highestParticipation: 'rejected',
      },
      filters: {
        type: { values: ALL_ACTION_TYPES },
        status: {
          values: {
            live: 'honoured',
            ratified: 'honoured',
            enacted: 'honoured',
            expired: 'honoured',
            dropped: 'honoured',
          },
          // Status is four separate nullable epoch columns, so any ONE status
          // is expressible and two at once is not. No value-set can say this;
          // `statusFilter` throws on `length > 1`.
          maxSelected: 1,
        },
      },
      expand: {
        // One /proposal_voting_summary request PER ELEMENT. Served, but a
        // caller paging 1000 rows with this on will meet the rate limiter —
        // see `proposal.tally.current` for the per-action cost.
        tallies: 'honoured',
        thresholds: 'rejected',
        // Always populated from the row's own meta_* columns; asking changes
        // nothing but does deliver what was asked for.
        metadata: 'honoured',
        myVote: 'rejected',
        protocolParams: 'rejected',
      },
      // `proposals.list{voterId}`, `proposals.get{voterId}` and
      // `expand: 'myVote'` are the same capability. One name, declared once;
      // all three sites throw.
      joins: { callerVote: 'rejected' },
      search: NO_SEARCH,
    },
    'proposal.body.current': {
      reachability: 'served',
      pollable: true,
      // No `filters`: the dataset's only route is `proposals.get`, which takes
      // an id and no type filter. Declaring one would describe a control that
      // does not exist on the route.
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
            // `getTallies` filters the three tallies it built in memory, so a
            // `direct` filter is applied to a set that never contains one and
            // yields an empty list rather than an error. Not offerable.
            direct: 'ignored',
          },
        },
      },
      caveats: [
        {
          kind: 'differentUnit',
          reports: 'count',
          ledgerDecidesBy: 'count',
          note:
            'The committee tally carries counts and no stake at all, because ' +
            'the committee votes by head; a UI must not render an ₳ ' +
            'prefix or a stake denominator for the cc row.',
        },
      ],
    },

    'proposal.ballot.current': {
      reachability: 'served',
      // `proposals.listVotes` delegates to `votes.list` with a proposal
      // filter: /vote_list is the only endpoint carrying `vote_tx_hash`, which
      // `VoteRecord.txRef` requires. /proposal_votes omits it.
      pollable: true,
      paging: KOIOS_PAGING,
      // Chronological only. `votingPower` is not refused — the sort switch is
      // `sort === 'oldest' ? asc : desc`, so asking for it silently returns
      // newest-first. That is worse than a refusal and must not be offered.
      sort: { newest: 'honoured', oldest: 'honoured', votingPower: 'ignored' },
      filters: {
        vote: { values: ALL_VOTE_CHOICES },
        role: { values: VOTE_LIST_ROLES },
        proposalType: { values: ALL_ACTION_TYPES },
      },
      expand: VOTE_EXPANDS,
      // `proposals.listVotes` forwards the whole query to `votes.list`, which
      // refuses any defined `search` — the empty string included.
      search: NO_SEARCH,
      caveats: [
        {
          kind: 'identifierGranularity',
          note:
            'Koios does not number the voting procedures within a ' +
            'transaction, so two votes cast in one tx are not separable and ' +
            'votes.get refuses an index.',
        },
      ],
    },
    'proposal.identity.events': {
      reachability: 'served',
      // Synthesised, not read: Koios has no activity feed. One /proposal_list
      // for the lifecycle epochs plus one /vote_list for the votes.
      pollable: true,
      paging: LOCAL_PAGING,
      caveats: [
        {
          kind: 'notExhaustive',
          note:
            'The votes are read without paging, so an action with more than ' +
            '1000 votes loses the oldest of them from the feed.',
        },
        {
          kind: 'boundedWindow',
          note:
            'Only `submitted` and `voted` carry a timestamp. Ratified, ' +
            'enacted, expired and dropped are epoch numbers and nothing ' +
            'else, so the feed can only be ordered to epoch resolution.',
        },
      ],
    },
    'proposal.outcome.current': {
      reachability: 'served',
      pollable: false,
      filters: { type: { values: ALL_ACTION_TYPES } },
    },

    'vote.ballot.current': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      sort: { newest: 'honoured', oldest: 'honoured', votingPower: 'ignored' },
      filters: {
        vote: { values: ALL_VOTE_CHOICES },
        role: { values: VOTE_LIST_ROLES },
        proposalType: { values: ALL_ACTION_TYPES },
      },
      expand: VOTE_EXPANDS,
      // `voterId` and `proposalId` are both PostgREST equality filters.
      joins: { voterOfVote: 'honoured', proposalOfVote: 'honoured' },
      search: NO_SEARCH,
      caveats: [
        {
          kind: 'notExhaustive',
          note:
            '`isCurrent` is computed by markSuperseded over the rows in hand, ' +
            'so it is exact only when the read was scoped to one proposal or ' +
            'one voter. On an arbitrary page of the global feed a vote whose ' +
            'replacement sits on another page is reported as current, and the ' +
            'default includeSuperseded=false filter then keeps it.',
        },
      ],
    },

    /* -- network ------------------------------------------------------------- */
    'network.identity.current': {
      reachability: 'served',
      pollable: true,
    },
    'network.chain.series': {
      reachability: 'served',
      pollable: false,
      // No `paging`: `listEpochs` and `listBlocks` take `limit`/`before`/
      // `block` and no PageRequest, so there is no cursor or offset to
      // describe. An omitted `limit` still returns at most one 1000-row Koios
      // page. `listBlocks` deliberately omits `order` when unpinned, because
      // `order=block_height.desc` makes Koios sort the whole block table
      // (>70s against mainnet).
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
            'Koios reports the DRep and SPO vote thresholds as IEEE-754 ' +
            'doubles, inherited from db-sync; 0.67 is not 2/3, so the typed ' +
            'dvt/pvt stay undefined and the floats are available in `raw`.',
        },
      ],
    },
    'network.params.asAt': {
      reachability: 'served',
      // `/epoch_params?_epoch_no=N` — an arbitrary past epoch is a first-class
      // read here, unlike db-sync's frozen SQL.
      pollable: false,
    },
    'network.stake.current': {
      reachability: 'served',
      // Assembled from /epoch_info, /drep_epoch_summary and a POST /drep_info
      // for the two predefined options.
      pollable: false,
      basis: { active: 'honoured', live: 'rejected' },
    },
    'network.treasury.current': {
      reachability: 'served',
      pollable: false,
    },

    // Served through an extension, refused through `governance.metrics.get`:
    // ten counters are one cheap request each, six required ones need a walk
    // over every DRep. Per-field declaration is what lets the dashboard
    // degrade per tile, and none of the six is a field GovTool reads.
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
            // different reactions. Here the cheaper path is the same dataset
            // reached through the extension below.
            fallback: 'network.aggregate.current',
            fallbackMethod: 'governance.metrics.getAvailable',
            reason:
              'Six of GovernanceMetrics’ required fields need a walk ' +
              'over every DRep; getAvailable() serves the rest, each from one ' +
              'request. See UNCOMPUTABLE_METRICS.',
          },
        },
      ],
    },

    /* -- account -------------------------------------------------------------- */
    'account.identity.current': {
      reachability: 'served',
      // One /account_info covers registration, both delegations and the whole
      // balance breakdown; only the delegation certificate needs a second
      // /account_updates read.
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
      // No `basis` control: `getVotingPower` takes no basis argument. What it
      // returns is `basis: 'live'` — utxo + rewards + reward_rest −
      // withdrawals at the tip, recomputed rather than read off Koios'
      // `total_balance`, which omits reward_rest and goes negative for an
      // account holding a governance deposit refund (observed on mainnet at
      // −98,218,279,141 lovelace). A live figure is NOT a valid tally
      // denominator.
      caveats: [
        {
          kind: 'staleBasis',
          basis: 'live',
          note:
            'The figure is the live balance, not the epoch snapshot the ' +
            'ledger counts votes against, and cannot be compared against an ' +
            'active-basis stake total.',
        },
      ],
    },
    'account.stake.asAt': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          'Koios reports account balances at the tip only, with no per-epoch ' +
          'history; getVotingPower refuses an `epoch`.',
      },
    },
    'account.delegation.current': {
      reachability: 'served',
      pollable: true,
    },
    'account.delegation.events': {
      reachability: 'served',
      // /account_updates gives the certificates but never what a delegation
      // pointed at, so one batched /tx_info resolves every target.
      pollable: true,
      paging: LOCAL_PAGING,
      filters: {
        kind: { values: { governance: 'honoured', pool: 'honoured' } },
      },
      caveats: [
        {
          // `listDelegationHistory` walks /account_updates for the
          // certificates and then resolves what each one pointed AT from
          // /tx_info. A certificate whose transaction does not come back, or
          // whose `info` spells the target under a key this provider does not
          // probe, is `continue`d — the event is dropped, and the walk that
          // computes `from` then reports the NEXT event as replacing the one
          // before the hole. A short history is not a complete one.
          kind: 'notExhaustive',
          note:
            'An event whose delegation target cannot be resolved from ' +
            '/tx_info is skipped rather than returned with an unknown target, ' +
            'so a history can be short and the surviving events’ `from` can ' +
            'name the wrong predecessor. The total reported by LOCAL_PAGING ' +
            'counts the surviving events, not the certificates.',
        },
      ],
    },
    'account.registration.events': {
      reachability: 'served',
      pollable: true,
      paging: LOCAL_PAGING,
    },

    /* -- pools, committee, voters --------------------------------------------- */
    'pool.identity.current': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      search: NO_SEARCH,
      caveats: [
        {
          // `pools.list` pins `pool_status: 'eq.registered'` on /pool_list
          // without being asked, so a pool that has announced retirement, or
          // retired, is not in the listing — while `pools.get` will happily
          // return it, because /pool_info takes the id and no status. A UI
          // that pages the directory and then looks a voter up by id sees the
          // two disagree, and nothing in the query said why.
          kind: 'impliedFilter',
          param: 'pool_status',
          restrictedTo: ['registered'],
          note:
            'pools.list filters /pool_list to registered pools. Retiring and ' +
            'retired pools are absent from the directory but still reachable ' +
            'through pools.get, and a retiring pool can still cast an SPO ' +
            'vote, so pool.ballot.current can name a pool the listing omits.',
        },
      ],
    },
    'pool.ballot.current': {
      reachability: 'served',
      pollable: true,
      paging: KOIOS_PAGING,
      // As with `dreps.listVotes`, the `VoteListQuery` is read for its page
      // and nothing else; the order is always `block_time.desc`.
      sort: { newest: 'honoured', oldest: 'ignored', votingPower: 'ignored' },
      filters: {
        vote: { values: IGNORED_VOTE_CHOICES },
        proposalType: { values: IGNORED_ACTION_TYPES },
      },
      expand: VOTE_EXPANDS,
      search: IGNORED_SEARCH,
      caveats: [ANCHOR_ONLY_RATIONALE],
    },
    'committee.identity.current': {
      reachability: 'served',
      // /committee_info reads current membership from ledger gov-state, which
      // is what the contract asks for — not a replay of enacted actions.
      pollable: false,
    },
    'constitution.identity.current': {
      reachability: 'served',
      pollable: false,
      caveats: [
        {
          kind: 'notExhaustive',
          note:
            'Koios has no constitution endpoint; this is reconstructed from ' +
            'the latest enacted NewConstitution action. One action, so the ' +
            'accumulating-error failure mode of a replay does not apply, but ' +
            'the constitution document itself is an anchor only.',
        },
      ],
    },
    'constitution.identity.events': {
      reachability: 'served',
      pollable: false,
      paging: LOCAL_PAGING,
    },
    'voter.identity.current': {
      reachability: 'served',
      // Dispatches on the bech32 prefix to /drep_info, /pool_info or
      // /committee_info.
      pollable: true,
    },
    'voter.identity.list': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          'Koios indexes DReps, pools and the committee separately; a ' +
          'combined listing would have to page three unrelated endpoints in ' +
          'lockstep and invent a stable ordering across them.',
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
          'contract requires the label-17 payload as CBOR hex. /tx_cbor ' +
          'returns the whole transaction, and extracting the auxiliary data ' +
          'means a CBOR and tx-layout decoder inside the provider.',
        caveat: {
          kind: 'encodingMismatch',
          contractEncoding: 'CBOR hex',
          sourceEncoding: 'decoded JSON',
          note:
            'The same decoded-JSON property is a GAIN for DRep, proposal and ' +
            'vote metadata; it is a loss only where the bytes themselves ' +
            'matter. This is the one GovTool route with no Koios path at all.',
        },
      },
    },

    /* -- metadata (the Metadata Service owns these; Koios resolves them too) ---- */
    // Declared `served` because Koios ships the resolved, validated document
    // in the same response as the chain data — no fetch, no second component.
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
  // Nothing outstanding. A migrating provider lists here what it has not
  // assessed yet, so the under-claim is a reviewable diff.
  [],
);

/* ------------------------------------------------------------------------- */
/* Entities — exhaustive over every optional field                            */
/* ------------------------------------------------------------------------- */

const KOIOS_ENTITIES: EntityDeclarations = {
  NetworkInfo: {
    // /genesis carries the magic and /tip the era — two reads the db-sync
    // provider cannot make at all. Both are nonetheless guarded in
    // `getNetworkInfo`, and both row types make them optional: `/genesis` can
    // answer with no rows, and `TipRow.era` is absent on older deployments.
    // The field is then simply left off, so neither is `always`.
    fields: {
      networkMagic: sometimes(
        'From /genesis; omitted when that read returns no row.',
      ),
      era: sometimes(
        'From /tip; older Koios deployments do not send `era` at all, and a ' +
          'guessed era would be worse than none.',
      ),
    },
  },
  EpochSummary: {
    fields: {
      firstBlock: absent(
        'mapEpochSummary reads no block bounds off /epoch_info.',
      ),
      lastBlock: absent(
        'mapEpochSummary reads no block bounds off /epoch_info.',
      ),
    },
  },
  BlockSummary: { fields: { txCount: ALWAYS } },
  ProtocolParams: {
    /*
     * Every column below is on /epoch_params for a Conway epoch, and each is
     * mapped through `?? undefined` — so a null column omits the field.
     *
     * That distinction is load-bearing because `network.params.asAt` is
     * served for ANY past epoch, not just Conway ones: ask for epoch 250 and
     * the six governance columns come back null, so the six typed fields come
     * back undefined. They are `conditional`, not `always`, and the condition
     * is the era rather than the deployment.
     *
     * `minFeeRefScriptCostPerByte` and `coinsPerUtxoByte` are the two the
     * contract types as `?: T | null`, so the mapper passes the null through
     * and they ARE populated in every era.
     */
    fields: {
      govActionDeposit: sometimes(CONWAY_ONLY),
      drepDeposit: sometimes(CONWAY_ONLY),
      keyDeposit: ALWAYS,
      poolDeposit: ALWAYS,
      minFeeA: ALWAYS,
      minFeeB: ALWAYS,
      minFeeRefScriptCostPerByte: ALWAYS,
      coinsPerUtxoByte: ALWAYS,
      govActionLifetime: sometimes(CONWAY_ONLY),
      drepActivity: sometimes(CONWAY_ONLY),
      committeeMinSize: sometimes(CONWAY_ONLY),
      committeeMaxTermLength: sometimes(CONWAY_ONLY),
      dvt: lossy('Stored as floating point; cannot become an exact Ratio.'),
      pvt: lossy('Stored as floating point; cannot become an exact Ratio.'),
      protocolVersion: ALWAYS,
    },
  },
  StakeDistribution: {
    fields: {
      epoch: ALWAYS,
      totalActiveStake: sometimes(
        'Omitted when /epoch_info reports a null active_stake, which is the ' +
          'case for the current epoch before its boundary snapshot.',
      ),
      totalLiveStake: absent('Koios reports the epoch snapshot only.'),
      /*
       * On Cardano the active stake snapshot IS the pool-delegated stake, so
       * totalStakeControlledBySPOs and totalActiveStake are the same number by
       * definition, not by approximation — EXCEPT that they are filled
       * differently. `totalActiveStake` is guarded and absent when
       * /epoch_info.active_stake is null; the two below fall back to `'0'`
       * instead, so on the current epoch before its boundary snapshot a
       * consumer sees an absent active stake next to a zero SPO stake and a
       * zero DRep stake. `always` is literally true — the field is always
       * populated — and the zero is the thing to distrust.
       */
      totalStakeControlledByDReps: ALWAYS,
      totalStakeControlledBySPOs: ALWAYS,
      alwaysAbstainVotingPower: ALWAYS,
      alwaysNoConfidenceVotingPower: ALWAYS,
    },
  },
  Treasury: {
    fields: {
      // /totals is read two rows at a time, so the previous epoch is normally
      // in hand and `delta` costs no extra request — but `mapTreasury` only
      // fills it when that second row exists. `getTreasury({ epoch: 0 })`
      // narrows to `epoch_no=lte.0`, one row comes back, and the field is
      // undefined. Not `always`: the contract's `delta?: Lovelace` has no null
      // to mean "no previous epoch".
      delta: sometimes(
        'Needs the previous epoch’s /totals row, which the two-row read ' +
          'normally supplies; absent at the first epoch of the chain or of ' +
          'an `epoch`-narrowed read.',
      ),
    },
  },
  StakeBalance: {
    // The breakdown is on /account_info. `total` is recomputed from the
    // components rather than read off `total_balance`; see mapBalance.
    fields: { utxo: ALWAYS, rewards: ALWAYS, rewardsRest: ALWAYS },
  },
  VotingPower: {
    fields: {
      epoch: sometimes(
        '/drep_voting_power_history rows carry the epoch; /drep_info’s ' +
          '`amount` does not say which epoch it belongs to, and a guessed ' +
          'epoch is worse than none.',
      ),
      share: absent('No denominator is fetched, so no share is computed.'),
    },
  },
  Account: {
    fields: {
      providerId: absent('Koios has no internal row id to expose.'),
      balance: ON_EXPAND,
      votingPower: ON_EXPAND,
      delegation: ON_EXPAND,
      poolDelegation: ON_EXPAND,
      latestRegistration: absent(
        'accounts.get never reads /account_updates for the registration ' +
          'certificates; accounts.listStakeEvents serves them as their own ' +
          'dataset.',
      ),
      latestDeregistration: absent('See latestRegistration.'),
      drep: refused(
        'Koios cannot tell whether a stake credential is also registered as ' +
          'a DRep.',
      ),
      adaHandles: refused(
        'Resolving $handles needs an asset lookup this provider does not make.',
      ),
    },
  },
  Delegation: {
    fields: {
      since: sometimes(
        'Koios reports the delegation target, never the certificate that set ' +
          'it. Real only on accounts.get with expand: delegation, which reads ' +
          '/account_updates; accounts.getDelegation sends null.',
      ),
    },
    misreported: [
      {
        field: 'txRef',
        sends: 'null',
        wouldMean: 'no transaction set this delegation',
        note:
          '`mapDelegation` defaults txRef to null and only accounts.get with ' +
          'expand: delegation passes one in. accounts.getDelegation — the hot ' +
          'read after wallet connect — is one /account_info and therefore ' +
          'always sends null, although the certificate is on /account_updates.',
      },
    ],
  },
  PoolDelegation: {
    // Every field is required, so the gap below cannot be declared any other
    // way. Unlike `Delegation`, `since` is required here, so there is no
    // "absent means this read did not cover it" to fall back on.
    fields: {},
    misreported: [
      {
        field: 'since',
        sends: 'null',
        wouldMean: 'the pool delegation has no recorded start',
        note:
          'mapPoolDelegation hard-codes null even on accounts.get with ' +
          'expand: poolDelegation, where the latest delegation_pool ' +
          'certificate — and its epoch and block time — is already in hand ' +
          'and is used to fill txRef.',
      },
    ],
  },
  StakeRegistrationEvent: {
    fields: {
      at: ALWAYS,
      slot: ALWAYS,
      block: absent(
        '/account_updates reports the absolute slot but never the block ' +
          'height, and deriving one from the other would be a guess.',
      ),
    },
  },
  DelegationHistoryEvent: {
    fields: {
      at: ALWAYS,
      // The whole certificate stream is materialised and walked in order, so
      // each event's predecessor is in hand — unlike a paged source, which
      // only knows the target each certificate set.
      from: ALWAYS,
    },
  },
  DRep: {
    fields: {
      cip105Id: ALWAYS,
      registrationByKind: absent(
        'Koios cannot split DRep and direct-voter registrations on one key; ' +
          'it does not model the direct voter at all.',
      ),
      isCip119Compliant: absent(
        'mapDRep never sets it. Koios’ is_valid says the document ' +
          'validated, not that it carries a CIP-119 givenName.',
      ),
      liveVotingPower: refused('Koios reports the epoch snapshot only.'),
      delegators: sometimes(
        '`live_delegator_count` is on every /drep_info row, so the count ' +
          'arrives unrequested; it is omitted when that column is null.',
      ),
      activity: sometimes(
        'A stub with `inactiveFromEpoch` is attached whenever /drep_info has ' +
          'an expiry epoch; real counters need dreps.get with ' +
          'expand: activity.',
      ),
      adaHandles: absent('Koios does not resolve Ada Handles.'),
    },
  },
  Registration: {
    fields: {
      // Koios' own `active` flag applies the drepActivity rule for us — the
      // derivation db-sync's directory query cannot make.
      status: ALWAYS,
      registeredAt: sometimes(
        'Only /drep_updates dates the certificates, and only dreps.get reads ' +
          'it. Absent on the directory listing, which is the contract’s ' +
          '"this read did not cover it".',
      ),
      registrationTx: sometimes('See registeredAt.'),
      retiredAt: sometimes('See registeredAt.'),
      retirementTx: sometimes('See registeredAt.'),
    },
  },
  DRepActivity: {
    fields: {
      notVotedCount: absent(
        'Counting actions a DRep did NOT vote on needs the set of actions ' +
          'votable while it was registered, which Koios cannot express as a ' +
          'query.',
      ),
      lastVotedAt: sometimes(
        'Read from the newest /drep_votes row, so present only when the ' +
          'caller asked for expand: activity.',
      ),
      // Not `always`: both sites that build a DRepActivity guard on
      // `expires_epoch_no !== null` — mapDRep attaches no activity at all
      // without one, and `KoiosDRepsApi.activity` builds the counters and then
      // leaves the field off. A retired DRep and the predefined options have a
      // null expiry, so `dreps.get` with `expand: activity` returns an
      // activity record with no inactiveFromEpoch on them.
      inactiveFromEpoch: sometimes(
        'Only when /drep_info reports an expires_epoch_no; a retired DRep and ' +
          'the predefined options have none.',
      ),
      participationRate: absent('Derived from notVotedCount; see that field.'),
    },
    misreported: [
      {
        field: 'votesCast',
        sends: '0',
        wouldMean: 'the DRep has never voted',
        note:
          'mapDRep attaches `{ votesCast: 0 }` to any DRep with an expiry ' +
          'epoch. dreps.get with expand: activity replaces it with the real ' +
          'lifetime count; the directory listing does not, so every row on ' +
          'governance.dreps.list reads as a DRep that never voted.',
      },
    ],
  },
  DRepDelegator: {
    // No optional fields on the contract type — which is exactly why the two
    // gaps below have to be `misreported`. `null` on a required-nullable field
    // means "known to be absent on chain", and neither of these is.
    fields: {},
    misreported: [
      {
        field: 'since',
        sends: 'null',
        wouldMean: 'the delegation has no recorded start',
        note:
          '/drep_delegators does carry an `epoch_no` for the snapshot, but it ' +
          'is the epoch the row was taken in, not the epoch the delegation ' +
          'began; mapDelegator sends null rather than pass a snapshot epoch ' +
          'off as a join time. A delegator list therefore cannot be sorted or ' +
          'filtered by tenure.',
      },
      {
        field: 'txRef',
        sends: 'null',
        wouldMean: 'no transaction created this delegation',
        note:
          'Koios has no join between /drep_delegators and the certificate ' +
          'that produced each row, so the delegating transaction is not ' +
          'reachable from this read at all.',
      },
    ],
  },
  DRepHistoryEvent: {
    fields: {
      at: ALWAYS,
      anchor: ALWAYS,
      changes: absent('No diff between successive certificates is computed.'),
    },
  },
  DRepVotingPowerEntry: {
    fields: {
      givenName: absent(
        'getVotingPowers hydrates /drep_info only; the name is in the ' +
          'metadata document, which would be a second batched read per page.',
      ),
    },
  },
  SpoVoter: {
    fields: {
      cip105Id: absent('A pool id has no CIP-105 form.'),
      ticker: sometimes(
        '/pool_list carries it; /pool_info overrides it from meta_json. Null ' +
          'on either means the pool published none.',
      ),
      name: sometimes('Only /pool_info’s meta_json carries a name.'),
      liveStake: sometimes('Only on /pool_info, so only on pools.get.'),
      activeStake: sometimes('On both, when the column is not null.'),
      pledge: sometimes('On both, when the column is not null.'),
    },
    misreported: [
      {
        field: 'votingPower',
        sends: 'null',
        wouldMean: 'the pool holds no voting power',
        note:
          '/pool_list carries no voting power — it is on /pool_info, which ' +
          'takes a POST body of ids — so every row of pools.list is null. ' +
          'pools.get fills it.',
      },
    ],
  },
  CommitteeMember: {
    fields: {
      cip105Id: absent('A committee credential has no CIP-105 form.'),
    },
    misreported: [
      {
        field: 'termStartEpoch',
        sends: 'null',
        wouldMean: 'the member’s term has no recorded start',
        note: '/committee_info reports when a term ends and never when it began.',
      },
    ],
  },
  Committee: { fields: {} },
  Constitution: { fields: {} },
  GovAction: {
    fields: {
      providerId: absent('Koios has no internal row id to expose.'),
      // The single biggest thing Koios gives that db-sync's GovTool SQL does
      // not: `proposal_description` is the ledger's own JSON rendering, so all
      // seven variants are typed rather than handed over as a blob.
      body: sometimes(
        'mapBody returns undefined for a description it cannot parse, ' +
          'leaving the caller with rawBody rather than throwing.',
      ),
      rawBody: ALWAYS,
      deposit: ALWAYS,
      depositReturnAddress: ALWAYS,
      proposedBy: ALWAYS,
      tallies: ON_EXPAND,
      protocolParamsAtSubmission: refused(
        'A separate /epoch_params read; call network.getProtocolParams({ epoch }).',
      ),
      protocolParamsAtEnactment: refused('See protocolParamsAtSubmission.'),
      myVote: refused(
        'Koios cannot join a single voter onto a proposal read; read ' +
          '/governance/votes instead.',
      ),
    },
  },
  GovActionLifecycle: { fields: { submitted: ALWAYS } },
  GovActionActivityEvent: {
    fields: {
      voter: sometimes('Carried on `voted` events only.'),
      vote: sometimes('Carried on `voted` events only.'),
    },
  },
  EnactedActionSummary: {
    fields: {
      enactedAt: ALWAYS, // getEnacted filters on `enacted_epoch not.is.null`
      submittedTx: ALWAYS,
      body: sometimes('See GovAction.body.'),
      rawBody: ALWAYS,
    },
  },
  RoleTally: {
    fields: {
      stake: sometimes(
        'Absent on the committee row: the committee votes by head, so ' +
          '/proposal_voting_summary publishes counts and no stake for it.',
      ),
      count: ALWAYS,
      notVotedStake: sometimes(
        'The gap between Koios’ `*_no_vote_power` (the CIP-1694 counting ' +
          'figure, which folds in non-voters) and `*_active_no_vote_power`. ' +
          'Absent when either column is null, and never present for the cc row.',
      ),
      totalEligibleStake: absent(
        'The denominator would have to be aggregated over the whole ' +
          'distribution per action.',
      ),
      threshold: lossy(
        'The threshold is a float on /epoch_params; see ProtocolParams. ' +
          'Koios does publish a `*_pct`, but a percentage is a rendering of ' +
          'the comparison, not the ratio.',
      ),
      passing: lossy('Derived from a threshold that cannot be represented.'),
    },
  },
  VoteRecord: {
    fields: { at: ALWAYS },
    misreported: [
      {
        field: 'votingPower',
        sends: 'null',
        wouldMean: 'the vote carried no voting power',
        note:
          'Koios records who voted and how, never the power applied when the ' +
          'vote was counted. The per-role totals on ' +
          '/proposal_voting_summary cannot be attributed back to voters.',
      },
      {
        field: 'isCurrent',
        sends: 'true',
        wouldMean: 'no later vote by this voter replaced this one',
        note:
          'Hard-coded true by mapDRepVote, pools.listVotes and the ' +
          'transaction effect mapper, none of which sees the set a supersede ' +
          'pass needs. Only votes.list computes it, and only exactly when the ' +
          'read was scoped to one proposal or one voter.',
      },
    ],
  },
  GovernanceMetrics: {
    // All six are served through `metrics.getAvailable()`; `metrics.get` is
    // refused outright, so none of them is reachable on a contract route.
    fields: {
      epoch: sometimes('From /drep_epoch_summary, via getAvailable().'),
      totalDRepDistribution: sometimes(
        'From /drep_epoch_summary, via getAvailable().',
      ),
      totalLiveGovernanceActions: sometimes(
        'A counting /proposal_list, via getAvailable().',
      ),
      totalSpoVotes: sometimes('A counting /vote_list, via getAvailable().'),
      totalCcVotes: sometimes('A counting /vote_list, via getAvailable().'),
      treasury: sometimes('From /totals, via getAvailable().'),
    },
    // Required fields Koios cannot compute — why `governance.metrics.get` is
    // in `refusedRoutes` above. Naming them turns "the dashboard is
    // unavailable" into "these six tiles are", and none is among the fields
    // the frontend actually reads. Kept in step with UNCOMPUTABLE_METRICS.
    unfillable: [
      {
        field: 'uniqueDelegators',
        cause: 'tooExpensive',
        note: 'Would mean /drep_delegators once per DRep, de-duplicated.',
      },
      {
        field: 'totalDelegations',
        cause: 'tooExpensive',
        note: 'The same walk as uniqueDelegators.',
      },
      {
        field: 'totalActiveDReps',
        cause: 'tooExpensive',
        note:
          'The active flag is on /drep_info, which takes explicit ids; there ' +
          'is no aggregate.',
      },
      {
        field: 'totalInactiveDReps',
        cause: 'tooExpensive',
        note: 'The same as totalActiveDReps.',
      },
      {
        field: 'totalActiveCip119CompliantDReps',
        cause: 'tooExpensive',
        note:
          'Needs the metadata body of every DRep, one /drep_metadata batch ' +
          'at a time.',
      },
      {
        field: 'totalRegisteredDirectVoters',
        cause: 'notInSource',
        note: 'Koios does not model GovTool’s direct (sole) voter at all.',
      },
    ],
  },
  TransactionState: {
    fields: {
      confirmations: sometimes(
        'Absent while the transaction is `unknown` — a hash submitted seconds ' +
          'ago is legitimately not on /tx_status yet.',
      ),
      includedAt: sometimes('Present once /tx_info has the transaction.'),
      // /tx_info with _governance: true returns voting procedures, proposal
      // procedures and certificates, so the effects are classified rather than
      // handed over raw — something db-sync's GovTool SQL cannot do.
      effects: sometimes('Present once /tx_info has the transaction.'),
      votingProcedures: sometimes('Present once /tx_info has the transaction.'),
    },
  },
  SurveyDefinition: { fields: {} },
};

/**
 * One field, two answers, because two routes reading one dataset genuinely
 * disagree. Without these the dataset would have to pick one answer and lie
 * about the other route.
 */
const KOIOS_FIELD_OVERRIDES: readonly FieldOverride[] = [
  {
    entity: 'DRep',
    field: 'liveVotingPower',
    route: 'governance.dreps.list',
    support: {
      serves: 'never',
      cause: 'notInSource',
      // `list` never inspects `expand` for this value, so the caller gets a
      // 200 with the key missing where `get` throws for the same request.
      whenRequested: 'ignored',
      note: 'dreps.list ignores the expand; dreps.get throws for it.',
    },
    note: 'List and single-read differ on what asking for it does.',
  },
  {
    entity: 'DRep',
    field: 'activity',
    route: 'governance.dreps.list',
    support: {
      serves: 'conditional',
      note:
        'The listing never computes activity: every row carries the ' +
        '`votesCast: 0` stub. See DRepActivity.misreported.',
    },
    note: 'Only dreps.get with expand: activity fills the counters.',
  },
  {
    entity: 'StakeBalance',
    field: 'utxo',
    route: 'governance.dreps.listDelegators',
    support: {
      serves: 'never',
      cause: 'notInSource',
      whenRequested: 'ignored',
      note: '/drep_delegators reports one total amount, with no breakdown.',
    },
    note: 'A delegator balance has no components; an account balance does.',
  },
  {
    entity: 'StakeBalance',
    field: 'rewards',
    route: 'governance.dreps.listDelegators',
    support: {
      serves: 'never',
      cause: 'notInSource',
      whenRequested: 'ignored',
      note: '/drep_delegators reports one total amount, with no breakdown.',
    },
    note: 'A delegator balance has no components; an account balance does.',
  },
  {
    entity: 'StakeBalance',
    field: 'rewardsRest',
    route: 'governance.dreps.listDelegators',
    support: {
      serves: 'never',
      cause: 'notInSource',
      whenRequested: 'ignored',
      note: '/drep_delegators reports one total amount, with no breakdown.',
    },
    note: 'A delegator balance has no components; an account balance does.',
  },
];

/**
 * Authored, not generated. A wall-clock `generatedAt` would make every
 * response differ from the last and turn a capability diff into noise; this
 * moves when the declaration does.
 */
const DECLARED_AT = '2026-09-18T00:00:00Z';

/** Matches this package's version, so a consumer can pin behaviour to it. */
const PROVIDER_VERSION = '0.1.0';

/**
 * The document, bound to the network the client is pointed at.
 *
 * The network is a constructor-time or `/genesis`-time fact, not a static one:
 * the same build serves mainnet, preprod and preview, and a declaration that
 * hard-coded `mainnet` would be wrong on two of the three.
 */
export function koiosCapabilities(
  network: NetworkId,
): ProviderCapabilityDocument {
  return {
    schemaVersion: 2,
    provider: 'koios',
    network,
    providerVersion: PROVIDER_VERSION,
    generatedAt: DECLARED_AT,
    datasets: KOIOS_DATASETS,
    entities: KOIOS_ENTITIES,
    fieldOverrides: KOIOS_FIELD_OVERRIDES,
    unreviewed: [],
    // Koios has no instance faults baked in. A 500 on one endpoint or a stale
    // deployment belongs here, emitted by a health probe, never in the
    // constant — a fault that clears must not need a release.
    overrides: [],
    extensions: [
      {
        dataset: 'network.aggregate.current',
        method: 'governance.metrics.getAvailable',
        returns: 'partialRecord',
        description:
          'The ten GovernanceMetrics counters Koios serves in one request ' +
          'each, for a dashboard that degrades per tile instead of per page.',
      },
    ],
    metadata: {
      // Koios is the only surveyed provider that resolves off-chain metadata
      // AND says whether it validated it: meta_json arrives flattened, with
      // is_valid, warning and comment alongside the chain data.
      resolvedBy: 'provider',
      validatesAgainstStandard: true,
      carriesFailureMessage: true,
    },
  };
}

/**
 * Every `CAPABILITY_UNSUPPORTED` this package can throw, as the typed value the
 * declaration is checked against.
 *
 * One entry per refusal site in `src/`, so `refusalIsDeclared` closes the loop
 * the old string table never could: the thrown value and the declared value
 * are the same object shape, and a site with no declaration fails the build's
 * test run rather than being noticed in review.
 */
export const KOIOS_REFUSALS: readonly AnyCapabilityRefusal[] = [
  /* -- dreps.api.ts ------------------------------------------------------- */
  {
    dataset: 'drep.identity.current',
    control: { kind: 'sortControl' },
    cause: 'noIndex',
    scope: 'source',
    reason:
      '/drep_list carries no voting power, registration date or activity to ' +
      'sort on.',
  },
  {
    dataset: 'drep.identity.current',
    control: { kind: 'search', mode: 'freeText' },
    cause: 'noIndex',
    scope: 'source',
    reason: 'Koios has no index over DRep names or metadata.',
  },
  {
    dataset: 'drep.identity.current',
    control: { kind: 'search', mode: 'exactId' },
    cause: 'noIndex',
    scope: 'source',
    reason:
      'dreps.list refuses every non-empty search term, an exact id included: ' +
      '/drep_list has no id filter.',
  },
  {
    dataset: 'drep.identity.current',
    control: { kind: 'filter', name: 'kind', value: 'directVoter' },
    cause: 'notInSource',
    scope: 'source',
    reason: 'Koios cannot distinguish a direct voter from a DRep.',
  },
  {
    dataset: 'drep.identity.current',
    control: { kind: 'expand', field: 'liveVotingPower' },
    cause: 'notInSource',
    scope: 'source',
    reason:
      'Koios reports the epoch snapshot only; there is no un-snapshotted ' +
      'DRep power.',
  },
  {
    dataset: 'drep.delegation.current',
    control: { kind: 'basis', basis: 'live' },
    cause: 'notInSource',
    scope: 'source',
    reason: '/drep_delegators reports the epoch snapshot only.',
  },
  {
    dataset: 'drep.delegation.events',
    control: { kind: 'dataset' },
    cause: 'notInSource',
    scope: 'source',
    reason: '/drep_delegators is a snapshot with no join or leave times.',
  },
  {
    dataset: 'drep.stake.current',
    control: { kind: 'basis', basis: 'live' },
    cause: 'notInSource',
    scope: 'source',
    reason: 'Koios reports the epoch snapshot only.',
  },
  // The same site: `getVotingPower` reads both datasets, and refuses `live`
  // before it decides which.
  {
    dataset: 'drep.stake.series',
    control: { kind: 'basis', basis: 'live' },
    cause: 'notInSource',
    scope: 'source',
    reason: 'Koios reports the epoch snapshot only.',
  },

  /* -- proposals.api.ts ---------------------------------------------------- */
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'search', mode: 'freeText' },
    cause: 'noIndex',
    scope: 'source',
    reason: 'Koios has no text index over proposal metadata.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'join', join: 'callerVote' },
    cause: 'notInSource',
    scope: 'source',
    reason:
      'Koios cannot join a voter onto a proposal listing or a single ' +
      'proposal read; read /governance/votes instead.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'expand', field: 'myVote' },
    cause: 'notInSource',
    scope: 'source',
    reason: 'Koios cannot join a single voter onto a proposal read.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'expand', field: 'protocolParams' },
    cause: 'notInSource',
    scope: 'source',
    reason:
      'The parameters in force at submission are a separate /epoch_params ' +
      'read; call network.getProtocolParams({ epoch }).',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'expand', field: 'thresholds' },
    cause: 'representation',
    scope: 'source',
    reason:
      'Koios reports voting thresholds as floating-point numbers, which ' +
      'cannot be returned as an exact Ratio.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'filterCardinality', name: 'status', max: 1 },
    cause: 'notInSource',
    scope: 'source',
    reason:
      'Koios encodes proposal status as four separate epoch columns; filter ' +
      'one status at a time.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'sort', key: 'mostYesVotes' },
    cause: 'notInSource',
    scope: 'source',
    reason:
      'Vote weights live on /proposal_voting_summary, which cannot be joined ' +
      'into the listing.',
  },
  {
    dataset: 'proposal.identity.current',
    control: { kind: 'sort', key: 'highestParticipation' },
    cause: 'notInSource',
    scope: 'source',
    reason:
      'Vote weights live on /proposal_voting_summary, which cannot be joined ' +
      'into the listing.',
  },

  /* -- votes.api.ts -------------------------------------------------------- */
  {
    dataset: 'proposal.ballot.current',
    control: { kind: 'search', mode: 'freeText' },
    cause: 'noIndex',
    scope: 'source',
    reason:
      'proposals.listVotes forwards to votes.list, which has no text index ' +
      'over vote rationales.',
  },
  {
    dataset: 'vote.ballot.current',
    control: { kind: 'search', mode: 'freeText' },
    cause: 'noIndex',
    scope: 'source',
    reason: 'Koios has no text index over vote rationales.',
  },
  // NOT LISTED: `votes.get{index}`. It is a real CAPABILITY_UNSUPPORTED site,
  // and `ControlRef` has no kind for a route ARGUMENT that is not a filter,
  // sort, expand, basis, search, join, page or batch option. `kind: 'route'`
  // would be a lie — `governance.votes.get` works, it only refuses the second
  // argument — and a `route` refusal is only predicted by `refusedRoutes`,
  // which would then mark the whole read unreachable. The boundary is declared
  // instead as the `identifierGranularity` caveat on this dataset.

  /* -- pools.api.ts -------------------------------------------------------- */
  {
    dataset: 'pool.identity.current',
    control: { kind: 'search', mode: 'freeText' },
    cause: 'noIndex',
    scope: 'source',
    reason: 'Koios has no index over pool tickers or names.',
  },

  /* -- governance/index.ts ------------------------------------------------- */
  {
    dataset: 'voter.identity.list',
    control: { kind: 'dataset' },
    cause: 'notInSource',
    scope: 'source',
    reason:
      'Koios has no combined voter index; page /drep_list, /pool_list and ' +
      '/committee_info separately.',
  },

  /* -- metrics.api.ts ------------------------------------------------------ */
  {
    dataset: 'network.aggregate.current',
    control: { kind: 'route', route: 'governance.metrics.get' },
    cause: 'tooExpensive',
    scope: 'source',
    reason:
      'Six required GovernanceMetrics fields need a walk over every DRep; ' +
      'use getAvailable() for the rest.',
    fallback: 'network.aggregate.current',
  },

  /* -- network.api.ts ------------------------------------------------------ */
  {
    dataset: 'network.stake.current',
    control: { kind: 'basis', basis: 'live' },
    cause: 'notInSource',
    scope: 'source',
    reason: 'Koios reports the epoch-boundary snapshot only.',
  },

  /* -- accounts.api.ts ----------------------------------------------------- */
  {
    dataset: 'account.identity.current',
    control: { kind: 'expand', field: 'drep' },
    cause: 'notInSource',
    scope: 'source',
    reason:
      'Koios cannot tell whether a stake credential is also registered as a ' +
      'DRep.',
  },
  {
    dataset: 'account.identity.current',
    control: { kind: 'expand', field: 'adaHandles' },
    cause: 'notInSource',
    scope: 'source',
    reason:
      'Resolving $handles needs an asset lookup this provider does not make.',
  },
  {
    dataset: 'account.stake.asAt',
    control: { kind: 'dataset' },
    cause: 'notInSource',
    scope: 'source',
    reason:
      'Koios reports account balances at the tip only, with no per-epoch ' +
      'history.',
  },

  /* -- surveys.api.ts ------------------------------------------------------ */
  {
    dataset: 'survey.body.current',
    control: { kind: 'dataset' },
    cause: 'representation',
    scope: 'source',
    reason:
      'Koios exposes transaction metadata as decoded JSON only; the contract ' +
      'requires the label-17 payload as CBOR hex.',
  },
];
