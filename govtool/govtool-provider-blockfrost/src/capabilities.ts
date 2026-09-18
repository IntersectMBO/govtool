/**
 * What this provider can and cannot serve, declared against the contract's
 * capability vocabulary (`@govtool/data-providers/chain-data` → `capabilities`).
 *
 * Every value below is transcribed from a refusal site, a mapper or a row type
 * in this package — never from another provider's answers. Where a value is
 * not obvious the comment says WHY, citing the endpoint or the mapper.
 *
 * Three facts shape almost all of it:
 *
 *  1. **Blockfrost is a per-entity store.** Directory endpoints return
 *     identifiers only, so a page of 25 DReps is 50 requests (detail +
 *     metadata each). That is why most listings are `cost: 'fanout'`, why
 *     `pollable` is false on them, and why `sort` is refused rather than
 *     applied to one page.
 *  2. **Filters run after paging.** `status`, `kind` and `search` are applied
 *     to the already-hydrated page, so a filtered page can come back short
 *     while matches remain — `OptionSupport: 'approximated'` plus a
 *     `notExhaustive` caveat, never `'honoured'`.
 *  3. **Votes carry no voting power.** Every tally this provider produces is a
 *     head-count where the ledger decides by stake. That is a
 *     `differentUnit` caveat, and it is the most dangerous thing here: the
 *     number is well-formed, arrives in a 200, and means something else.
 *
 * Verified against blockfrost-ryo 3.1.1 at mainnet epoch 656, 2026-09-18.
 */

import type {
  CapabilityTable,
  DatasetId,
  EntityDeclarations,
  FieldOverride,
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

/** Populated on some reads and not others; the note says which. */
const sometimes = (note: string) => ({ serves: 'conditional', note }) as const;

/** The source does not record it. Naming it in `expand` is silently ignored. */
const absent = (note: string) =>
  ({
    serves: 'never',
    cause: 'notInSource',
    whenRequested: 'ignored',
    note,
  }) as const;

/** The source does not record it, and asking for it throws. */
const refused = (note: string) =>
  ({
    serves: 'never',
    cause: 'notInSource',
    whenRequested: 'throws',
    note,
  }) as const;

/** Reachable, and not worth what it costs. Asking is ignored. */
const tooCostly = (note: string) =>
  ({
    serves: 'never',
    cause: 'tooExpensive',
    whenRequested: 'ignored',
    note,
  }) as const;

/** Reachable, not worth what it costs, and asking for it throws. */
const tooCostlyRefused = (note: string) =>
  ({
    serves: 'never',
    cause: 'tooExpensive',
    whenRequested: 'throws',
    note,
  }) as const;

/** Blockfrost has it; this build does not join it in. Asking is ignored. */
const notBuilt = (note: string) =>
  ({
    serves: 'never',
    cause: 'notImplemented',
    whenRequested: 'ignored',
    note,
  }) as const;

/** Blockfrost has it; this build does not join it in, and asking throws. */
const notBuiltRefused = (note: string) =>
  ({
    serves: 'never',
    cause: 'notImplemented',
    whenRequested: 'throws',
    note,
  }) as const;

/** Exists upstream, cannot be expressed in the contract's type. */
const lossy = (note: string) =>
  ({
    serves: 'never',
    cause: 'representation',
    whenRequested: 'ignored',
    note,
  }) as const;

/**
 * Blockfrost paging, as `common/paging.ts` translates it.
 *
 * NOT `'everything'`. The contract says an omitted `limit` returns everything;
 * here every listing substitutes its own route default (25, or 100 for votes),
 * because "everything" on a per-entity store means tens of thousands of
 * requests. A consumer that believes the contract gets one page and thinks it
 * has the collection — the bug the db-sync-era DRep snapshot already shipped.
 *
 * `offset` is honoured only in whole pages: Blockfrost seeks by page number, so
 * an offset that is not a multiple of `limit` is rejected with `INVALID_INPUT`
 * rather than silently rounded. `PagingSupport` has no field for that
 * constraint, so it is recorded here.
 */
const BLOCKFROST_PAGING: PagingSupport = {
  cursor: 'honoured', // the cursor IS the next Blockfrost page number
  offset: 'honoured',
  maxLimit: 100, // MAX_PAGE_SIZE, silently clamped in toBlockfrostPage
  omittedLimitMeans: 'routeDefault',
  defaultLimit: 25,
  // No collection has a count endpoint, so `total` is never set and a
  // numbered paginator cannot be built; a short page is the last one.
  total: 'absent',
};

/** `proposals.listVotes` asks for 100 per page; everything else 25. */
const VOTE_PAGING: PagingSupport = { ...BLOCKFROST_PAGING, defaultLimit: 100 };

/**
 * Search over the hydrated page only.
 *
 * `exactId` and `freeText` are both real — the DRep filter matches the id, hex,
 * CIP-105 id and the metadata `givenName`; the proposal filter matches the id
 * and the metadata title/abstract/motivation/rationale — but both run over the
 * 25 rows already fetched, so they are `approximated`, not `honoured`. An exact
 * id search is the sharpest case: unless the row happens to be on the page the
 * caller asked for, the answer is empty.
 *
 * `adaHandle` is `ignored` rather than `rejected`: nothing throws, the handle
 * is simply treated as free text and matches nothing, so a UI must not offer
 * handle resolution.
 */
const PAGE_LOCAL_SEARCH: SearchSupport = {
  modes: {
    exactId: 'approximated',
    freeText: 'approximated',
    adaHandle: 'ignored',
  },
  // GovTool's backend always sends `search`; the empty string skips filtering.
  emptyStringAccepted: true,
};

/* ------------------------------------------------------------------------- */
/* Datasets                                                                    */
/* ------------------------------------------------------------------------- */

const BLOCKFROST_DATASETS: CapabilityTable = declareCapabilities(
  {
    /* -- network ------------------------------------------------------------ */
    'network.identity.current': {
      reachability: 'served',
      // /genesis (memoised) + /blocks/latest + /epochs/latest.
      pollable: true,
    },
    'network.chain.series': {
      reachability: 'served',
      // Blockfrost has no epoch-range or block-range endpoint, so listEpochs
      // and listBlocks issue one request per element, 8 at a time.
      pollable: false,
      // No `paging` declared on purpose: neither route takes a `PageRequest`.
      // They take a bare `limit` (default 10 epochs / 1 block, clamped to 100),
      // and `PagingSupport` cannot describe a listing with no cursor and no
      // offset without claiming a refusal that never happens.
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
            'Blockfrost reports every dvt_/pvt_ threshold as a double ' +
            '(dvt_motion_no_confidence: 0.67); a float cannot be turned back ' +
            'into the on-chain numerator/denominator, so they stay in `raw`.',
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
      // The epoch record carries `active_stake` and nothing else. The route
      // does not read `basis` at all, so asking for `live` returns the active
      // snapshot rather than throwing.
      basis: { active: 'honoured', live: 'ignored' },
      caveats: [
        {
          kind: 'staleBasis',
          basis: 'active',
          note:
            'getStakeDistribution ignores `basis`; the figure is always the ' +
            "epoch's own active-stake snapshot.",
        },
      ],
    },
    'network.treasury.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        // Not `deploymentFault`: that kind must live in `overrides` so it can
        // clear without a release, and this build refuses unconditionally —
        // fixing the deployment would not make the call work. `scope` records
        // where the limit actually is.
        kind: 'notImplemented',
        scope: 'deployment',
        reason:
          'The treasury and reserves are exposed only through /network, which ' +
          'answers 500 on the verified deployment, so getTreasury rejects ' +
          'unconditionally rather than probing.',
      },
    },
    'network.aggregate.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'tooExpensive',
        scope: 'source',
        // `tooExpensive` cannot compile without a fallback, and there is no
        // cheap provider-side path here: the named fallback is the caller
        // doing the walk itself over the DRep directory. See the per-field
        // `unfillable` list on `GovernanceMetrics` for which counter costs what.
        fallback: 'drep.identity.current',
        reason:
          'Every GovernanceMetrics counter is a collection-wide aggregate and ' +
          'Blockfrost publishes none of them. uniqueDelegators alone is every ' +
          'delegator of every DRep. The only path is for the caller to page ' +
          'the directories itself — a walk, not a cheaper route.',
      },
    },

    /* -- account ------------------------------------------------------------ */
    'account.identity.current': {
      reachability: 'served',
      // One /accounts/{stake} read covers registration, balance, the
      // governance delegation and the pool delegation, so the expands that
      // are served cost nothing extra.
      pollable: true,
      expand: {
        balance: 'honoured',
        delegation: 'honoured',
        poolDelegation: 'honoured',
        votingPower: 'rejected', // per-DRep only; never per stake account
        drep: 'rejected',
        adaHandles: 'rejected',
      },
    },
    'account.stake.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          'Blockfrost reports voting power per DRep, never per stake account; ' +
          "there is no route for a wallet's own power.",
      },
    },
    'account.stake.asAt': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          'accounts.getVotingPower is refused outright, so the `epoch` form of ' +
          'it is too.',
      },
    },
    'account.delegation.current': {
      reachability: 'served',
      pollable: true,
    },
    'account.delegation.events': {
      reachability: 'served',
      // One page read, then one /epochs/{n} per row to date it (memoised, but
      // cold it is a request per element).
      pollable: false,
      paging: BLOCKFROST_PAGING,
      filters: {
        kind: {
          values: {
            pool: 'honoured',
            // /accounts/{stake} carries only the CURRENT drep_id; there is no
            // governance-delegation history endpoint, so this throws rather
            // than quietly answering with the pool history.
            governance: 'rejected',
          },
          defaultsTo: 'pool',
        },
      },
    },
    'account.registration.events': {
      reachability: 'served',
      pollable: false,
      paging: BLOCKFROST_PAGING,
      caveats: [
        {
          kind: 'notExhaustive',
          note:
            'listStakeEvents drops any row whose `action` is neither ' +
            'registered nor deregistered, and — unlike dreps.list, ' +
            'proposals.list and proposals.listVotes, which re-derive the ' +
            'cursor from the RAW row count — it hands the filtered array to ' +
            'toContractPage. A page that loses a row therefore looks short, ' +
            'so nextCursor comes back null and the caller stops with later ' +
            'certificates unread.',
        },
      ],
    },

    /* -- drep --------------------------------------------------------------- */
    'drep.identity.current': {
      reachability: 'served',
      // THE defining cost of this provider: /governance/dreps is ids only, so
      // every element costs a detail read plus a metadata read. A page of 25
      // is 50 requests.
      // Never put the directory on a refresh interval.
      pollable: false,
      paging: BLOCKFROST_PAGING,
      // Refused, not ignored: sorting needs the whole collection, and sorting
      // one hydrated page would present an arbitrary 25 rows as a ranking.
      sort: {
        votingPower: 'rejected',
        registrationDate: 'rejected',
        activity: 'rejected',
        status: 'rejected',
        random: 'rejected',
      },
      filters: {
        // Applied to the hydrated page, never to the directory: a filtered
        // page can come back short while matches remain further on.
        status: {
          values: {
            active: 'approximated',
            inactive: 'approximated',
            retired: 'approximated',
          },
          exhaustive: false,
        },
        kind: {
          // `kind` is INFERRED (a credential with a metadata anchor is a
          // `drep`, one without a `directVoter` — see deriveKind), and then
          // filtered on the page. Two approximations stacked.
          values: { drep: 'approximated', directVoter: 'approximated' },
          exhaustive: false,
        },
      },
      expand: {
        // The metadata read happens on every hydration regardless, because it
        // is what decides `kind`.
        metadata: 'honoured',
        activity: 'honoured',
        // dreps.get throws for both. On dreps.list `expand` is never inspected,
        // so the same request is silently ignored there — see the field
        // overrides below.
        delegators: 'rejected',
        liveVotingPower: 'rejected',
      },
      search: PAGE_LOCAL_SEARCH,
      caveats: [
        {
          kind: 'notExhaustive',
          note:
            'status, kind and search are applied to the already-hydrated page, ' +
            'so a page can come back shorter than `limit` while more matches ' +
            'exist. nextCursor tracks the Blockfrost page, so a caller that ' +
            'follows it still sees every match.',
        },
      ],
    },
    'drep.registration.current': {
      reachability: 'served',
      pollable: true,
    },
    'drep.registration.events': {
      reachability: 'served',
      pollable: false,
      paging: BLOCKFROST_PAGING,
    },
    'drep.stake.current': {
      reachability: 'served',
      // getVotingPower is one request; getVotingPowers is two per id.
      pollable: false,
      // `amount` is the DRep's stake in the current distribution — an
      // epoch-boundary snapshot. Asking for live throws.
      basis: { active: 'honoured', live: 'rejected' },
      batch: {
        explicitIds: 'honoured',
        // Without ids this is one detail read per DRep in the directory.
        allIds: 'rejected',
        // No cap is enforced; the caller's list length is the cost.
      },
    },
    'drep.stake.series': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          'Blockfrost reports a DRep’s current amount only; there is no ' +
          'per-epoch history to range over, so fromEpoch/toEpoch throw.',
      },
    },
    'drep.delegation.current': {
      reachability: 'served',
      pollable: true,
      paging: BLOCKFROST_PAGING,
      // The mirror image of Koios: Blockfrost reports each delegator's LIVE
      // stake, and the epoch snapshot is what it cannot give.
      basis: { live: 'honoured', active: 'rejected' },
    },
    'drep.delegation.events': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          '/governance/dreps/{id}/delegators is a snapshot of who delegates ' +
          'now, with no join or leave times; the transitions are not recorded.',
      },
    },
    'drep.ballot.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        // Not `notInSource`: the votes ARE recorded, and reachable per
        // proposal. What is missing is the reverse index.
        kind: 'noIndex',
        scope: 'source',
        reason:
          '/governance/dreps/{id}/votes returns {tx_hash, cert_index, vote} ' +
          'and never says which proposal each vote was cast on. Rebuilding a ' +
          'voting record means reading every proposal’s votes looking for this ' +
          'voter — ~1,500 requests per DRep on mainnet.',
      },
    },
    'drep.aggregate.current': {
      reachability: 'served',
      pollable: true,
      // Served, but see `DRepActivity.misreported`: `votesCast` is sent as 0
      // because the contract requires it and Blockfrost counts nothing.
    },

    /* -- pool --------------------------------------------------------------- */
    'pool.identity.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'deployment',
        reason:
          '/pools lists ids but /pools/{id} times out (504) on the verified ' +
          'deployment, so a pool cannot be hydrated. Even working, an SpoVoter ' +
          'needs the snapshot voting power, which Blockfrost reports only as ' +
          'live stake.',
      },
    },
    'pool.ballot.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'noIndex',
        scope: 'source',
        reason:
          'Blockfrost indexes votes per proposal, not per pool; there is no ' +
          'route from a pool id to the votes it cast.',
      },
    },

    /* -- committee & constitution ------------------------------------------- */
    'committee.identity.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          'Blockfrost has no committee resource at all — /governance/committee ' +
          'answers 400 "Invalid path". Replaying enacted UpdateCommittee ' +
          'actions would be a guess presented as gov-state.',
      },
    },
    'constitution.identity.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          'Blockfrost has no constitution resource; /governance/constitution ' +
          'answers 400 "Invalid path".',
      },
    },
    'constitution.identity.events': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason: 'No constitution resource, so no succession of them either.',
      },
    },

    /* -- proposal ------------------------------------------------------------ */
    'proposal.identity.current': {
      reachability: 'served',
      // The directory carries {tx_hash, cert_index, governance_type}; the rest
      // is a detail read plus metadata plus, per type, a sub-resource.
      pollable: false,
      paging: BLOCKFROST_PAGING,
      sort: {
        newest: 'rejected',
        oldest: 'rejected',
        soonestToExpire: 'rejected',
        mostYesVotes: 'rejected',
        highestParticipation: 'rejected',
      },
      filters: {
        // Cheaper than the others and NOT exhaustive. `governance_type` is on
        // the directory row, so non-matching candidates are dropped before
        // hydration — that is a cost saving, not a server-side filter.
        // `/governance/proposals` takes no type parameter (the request carries
        // only count/page/order), so the filter still runs over the 25 refs
        // this page fetched: `{limit: 5, type: ['InfoAction']}` over a page
        // holding two InfoActions returns two rows, with more on page 2.
        type: {
          values: {
            ParameterChange: 'approximated',
            HardForkInitiation: 'approximated',
            TreasuryWithdrawals: 'approximated',
            NoConfidence: 'approximated',
            UpdateCommittee: 'approximated',
            NewConstitution: 'approximated',
            InfoAction: 'approximated',
          },
          exhaustive: false,
        },
        // Status needs the detail read, so it filters the hydrated page. Every
        // status is derivable (each record carries its terminal epoch), which
        // db-sync's live-only SQL cannot do — but only within the page.
        status: {
          values: {
            live: 'approximated',
            ratified: 'approximated',
            enacted: 'approximated',
            expired: 'approximated',
            dropped: 'approximated',
          },
          exhaustive: false,
        },
      },
      expand: {
        // `hydrate` reads `expand` on BOTH routes. Naming `metadata` fetches
        // the anchor document; naming anything else and NOT `metadata` skips
        // that read, and the action then carries `metadata: null` — see the
        // misreported entry on `GovAction`.
        metadata: 'honoured',
        // Honoured, and expensive: it reads EVERY vote on the action.
        tallies: 'honoured',
        // proposals.list validates `expand` and throws for these three.
        // proposals.get validates only `voterId`, so `hydrate` reaches these
        // three, matches none of them and silently does nothing — see the
        // field overrides below.
        thresholds: 'rejected',
        protocolParams: 'rejected',
        myVote: 'rejected',
      },
      // `list{voterId}`, `get{voterId}` and `expand: 'myVote'` are one
      // capability: annotating an action with the caller's vote means one
      // votes read per proposal.
      joins: { callerVote: 'rejected' },
      search: PAGE_LOCAL_SEARCH,
      caveats: [
        {
          kind: 'notExhaustive',
          note:
            'type, status and search all run over the refs of the Blockfrost ' +
            'page that was fetched — the directory request carries no filter ' +
            'parameter of any kind. `type` is cheaper than the other two ' +
            '(non-matching refs are dropped before hydration) but it is no ' +
            'more exhaustive: any filtered page can come back short while ' +
            'matches remain. nextCursor tracks the Blockfrost page, so a ' +
            'caller that follows it still sees every match.',
        },
      ],
    },
    'proposal.body.current': {
      reachability: 'served',
      // The description is on the detail record; ParameterChange and
      // TreasuryWithdrawals need one further sub-resource each.
      pollable: false,
      filters: {
        // All seven build a typed body. `buildBody` returns undefined — and
        // the action then carries `rawBody` only — when a description does not
        // match the shape this Blockfrost version was verified to produce.
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
      // getTallies pages the action's ENTIRE vote list through getAll.
      pollable: false,
      filters: {
        role: {
          values: {
            drep: 'honoured',
            spo: 'honoured',
            cc: 'honoured',
            // Declared `ignored`, not `honoured` and not `rejected`: nothing
            // throws, the filter IS applied, and it can never match, because
            // Blockfrost has no `direct` voter_role — a direct voter registers
            // a DRep credential and votes as `drep`. A UI must not offer an
            // always-empty option.
            direct: 'ignored',
          },
        },
      },
      caveats: [
        {
          // THE most dangerous thing this provider does. The call succeeds,
          // the number is well-formed, and it is turnout where the ledger
          // weighs stake. An "₳" prefix on it would be a lie and a percentage
          // against a stake denominator is meaningless.
          kind: 'differentUnit',
          reports: 'count',
          ledgerDecidesBy: 'stake',
          note:
            'Blockfrost attaches no voting power to a vote, so a DRep or SPO ' +
            'tally here is head-count turnout, not the weight the ledger ' +
            'counts. RoleTally.stake, threshold and passing are left unset ' +
            'rather than computed from counts. Only the committee, which the ' +
            'ledger really does count by head, is comparable with db-sync.',
        },
      ],
    },
    'proposal.ballot.current': {
      reachability: 'served',
      pollable: true,
      paging: VOTE_PAGING,
      // All three throw: the votes carry no timestamp and no voting power, so
      // there is nothing to order by — the control disappears rather than
      // losing one item.
      sort: { newest: 'rejected', oldest: 'rejected', votingPower: 'rejected' },
      filters: {
        vote: {
          values: {
            yes: 'approximated',
            no: 'approximated',
            abstain: 'approximated',
          },
          exhaustive: false,
        },
        role: {
          values: {
            drep: 'approximated',
            spo: 'approximated',
            cc: 'approximated',
            direct: 'ignored', // see proposal.tally.current
          },
          exhaustive: false,
        },
        // This route is scoped to ONE action, so filtering its votes by
        // proposal type is degenerate; the implementation never reads it.
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
      // listVotes never inspects `expand`. Asking for any of these is accepted
      // and does nothing: votingPower and rationale are null on every row, and
      // the proposal is already the ref the caller passed in.
      expand: {
        votingPower: 'ignored',
        rationale: 'ignored',
        proposal: 'ignored',
      },
      // `VoteListQuery.search` reaches this route and listVotes never reads
      // it, so every mode is ACCEPTED AND SILENTLY NOT APPLIED. Declared
      // rather than omitted: omitting it reads as "refused", and a caller who
      // sends a voter id here gets the whole unfiltered page back looking
      // like a search result.
      search: {
        modes: {
          exactId: 'ignored',
          freeText: 'ignored',
          adaHandle: 'ignored',
        },
        emptyStringAccepted: true,
      },
      caveats: [
        {
          kind: 'notExhaustive',
          note:
            'vote and role are applied to the fetched page, so a filtered page ' +
            'can be short while matches remain on later pages.',
        },
        {
          kind: 'impliedFilter',
          param: 'includeSuperseded',
          restrictedTo: ['false'],
          note:
            'The endpoint returns each voter’s current vote only, so every row ' +
            'is reported with isCurrent: true and superseded votes cannot be ' +
            'listed at all.',
        },
        {
          kind: 'encodingMismatch',
          contractEncoding: 'CIP-129 bech32',
          sourceEncoding: 'raw hex (committee credentials)',
          note:
            'Committee voters arrive as bare hex, not bech32, with no ' +
            'indication of hot vs cold or script-ness, so VoterRef.id for a ' +
            '`cc` row is the hex Blockfrost reported. DRep and SPO ids are ' +
            'bech32 and normalised.',
        },
      ],
    },
    'proposal.identity.events': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          'A lifecycle timeline needs dated events and Blockfrost dates ' +
          'neither the submission (no submission epoch on the record) nor the ' +
          'votes (no timestamp on a vote).',
      },
    },
    'proposal.outcome.current': {
      reachability: 'served',
      // A bounded crawl: the directory is scanned newest-first for up to 400
      // candidates of the requested type, hydrating only those.
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
      caveats: [
        {
          kind: 'notExhaustive',
          note:
            'getEnacted returns null once 400 candidates have been scanned ' +
            'without a hit. null therefore means "not found within the scan", ' +
            'not "no enacted action of this type exists".',
        },
      ],
    },

    /* -- vote ---------------------------------------------------------------- */
    'vote.ballot.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'noIndex',
        scope: 'source',
        reason:
          'Blockfrost indexes votes per proposal and per DRep and has no ' +
          'cross-cutting feed, and no route resolves a vote from its ' +
          'transaction hash. Use governance.proposals.listVotes.',
      },
    },

    /* -- voter directory ------------------------------------------------------ */
    'voter.identity.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          'Resolving an id role-agnostically needs the SPO and committee ' +
          'resources this provider cannot serve: there is no committee ' +
          'resource, and /pools/{id} times out on this deployment.',
      },
    },
    'voter.identity.list': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          'Blockfrost indexes DReps, pools and (nothing at all for) the ' +
          'committee separately; there is no role-agnostic voter listing.',
      },
    },

    /* -- transaction & survey -------------------------------------------------- */
    'transaction.identity.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        // As with the treasury: the CODE refuses unconditionally, so this is
        // not an override that can clear on its own.
        kind: 'notImplemented',
        scope: 'deployment',
        reason:
          '/txs/{hash} answers 500 on the verified deployment for every hash, ' +
          'including one taken from the tip block. No other route separates a ' +
          'submitted transaction from an unknown one, and reporting `unknown` ' +
          'would leave the post-submission screens spinning, so this rejects.',
      },
    },
    'survey.body.current': {
      // The THIRD state: `surveys` is not a property of
      // BlockfrostChainDataProvider at all, so `provider.surveys` is undefined
      // and calling it is a TypeError, not a rejected promise. A consumer must
      // check before dereferencing.
      reachability: 'missing',
      pollable: false,
      unavailable: {
        kind: 'notImplemented',
        scope: 'deployment',
        reason:
          'CIP-179 definitions are label-17 transaction metadata, and the /txs ' +
          'routes answer 500 here, so the namespace is omitted rather than ' +
          'present and always failing.',
      },
    },

    /* -- metadata (produced by the Metadata Service) ----------------------------- */
    'drep.metadata.current': {
      reachability: 'served',
      pollable: false,
    },
    'proposal.metadata.current': {
      reachability: 'served',
      pollable: false,
    },
    'vote.metadata.current': {
      reachability: 'refused',
      pollable: false,
      unavailable: {
        kind: 'notInSource',
        scope: 'source',
        reason:
          'A vote row carries no anchor and Blockfrost has no vote resource, ' +
          'so VoteRecord.rationale is null on every row.',
      },
    },
  },
  // Nothing outstanding: every dataset above was read against the
  // implementation rather than assumed.
  [] as readonly DatasetId[],
);

/* ------------------------------------------------------------------------- */
/* Entities — exhaustive over every optional field                            */
/* ------------------------------------------------------------------------- */

const BLOCKFROST_ENTITIES: EntityDeclarations = {
  NetworkInfo: {
    fields: {
      networkMagic: ALWAYS, // /genesis
      era: absent('Neither /genesis nor /blocks/latest names the era.'),
    },
  },
  EpochSummary: {
    fields: {
      // /epochs/{n} has first_block_time and last_block_time, but no block
      // NUMBERS, and the contract wants heights.
      firstBlock: absent(
        'The epoch record dates the first block, not its height.',
      ),
      lastBlock: absent(
        'The epoch record dates the last block, not its height.',
      ),
    },
  },
  BlockSummary: { fields: { txCount: ALWAYS } },
  ProtocolParams: {
    // The richest parameter source of any provider surveyed: every governance
    // parameter is present and named on /epochs/{n}/parameters.
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
      dvt: lossy('Reported as floating point; cannot become an exact Ratio.'),
      pvt: lossy('Reported as floating point; cannot become an exact Ratio.'),
      protocolVersion: ALWAYS,
    },
  },
  StakeDistribution: {
    fields: {
      epoch: ALWAYS,
      totalActiveStake: sometimes(
        'From the epoch record’s `active_stake`, which is null on the current ' +
          'epoch until its snapshot is taken.',
      ),
      totalLiveStake: absent(
        'The epoch record carries the active snapshot only; /network would ' +
          'carry live supply figures and answers 500 here.',
      ),
      totalStakeControlledByDReps: tooCostly(
        'Summing the DRep distribution is one detail request per DRep.',
      ),
      totalStakeControlledBySPOs: tooCostly(
        'Summing pool stake means one request per pool, and /pools/{id} times ' +
          'out on this deployment.',
      ),
      alwaysAbstainVotingPower: absent(
        'Blockfrost reports no amount for the predefined delegation targets.',
      ),
      alwaysNoConfidenceVotingPower: absent(
        'Blockfrost reports no amount for the predefined delegation targets.',
      ),
    },
  },
  Treasury: {
    fields: {
      delta: absent(
        'The treasury is not served at all; see network.treasury.current.',
      ),
    },
  },
  StakeBalance: {
    fields: {
      utxo: absent(
        'The account record gives `controlled_amount` as one total, with no ' +
          'utxo/rewards split beyond the reward sums.',
      ),
      rewards: ALWAYS, // rewards_sum
      // Filled from `reserves_sum` alone. Blockfrost reports `treasury_sum`
      // separately and it is NOT added in, so this understates the contract's
      // "non-staking rewards" by any treasury payout. Declared populated
      // because it is; `FieldSupport` has no way to say "populated, but from a
      // narrower source than the field defines".
      rewardsRest: ALWAYS,
    },
  },
  VotingPower: {
    fields: {
      epoch: absent(
        'A DRep’s `amount` arrives without the snapshot epoch it was taken in.',
      ),
      share: absent('Not computed; the denominator would be a second read.'),
    },
  },
  Account: {
    fields: {
      providerId: absent('Blockfrost has no internal row id to expose.'),
      balance: ON_EXPAND,
      delegation: ON_EXPAND,
      poolDelegation: ON_EXPAND,
      votingPower: refused(
        'Blockfrost reports voting power per DRep, never per stake account.',
      ),
      drep: notBuiltRefused(
        'The account carries `drep_id`; resolving it to a DRep record is a ' +
          'second read this build does not make.',
      ),
      adaHandles: refused('Blockfrost does not resolve Ada Handles.'),
      latestRegistration: notBuilt(
        '/accounts/{a}/registrations has the certificates; accounts.get does ' +
          'not join them. Use accounts.listStakeEvents.',
      ),
      latestDeregistration: notBuilt(
        'Same as latestRegistration: available from listStakeEvents, not joined.',
      ),
    },
  },
  Delegation: {
    fields: {
      since: absent(
        'The account record names the current DRep but not when the ' +
          'delegation started.',
      ),
    },
  },
  PoolDelegation: { fields: {} },
  StakeRegistrationEvent: {
    fields: {
      // /accounts/{a}/registrations returns {tx_hash, action} and nothing else.
      at: absent('The registrations listing dates none of the certificates.'),
      slot: absent('No slot on the registrations listing.'),
      block: absent('No block height on the registrations listing.'),
    },
  },
  DelegationHistoryEvent: {
    fields: {
      at: ALWAYS, // active_epoch, stamped through EpochTimeResolver
      from: absent(
        'The delegations listing gives the pool each certificate chose, not ' +
          'the one it replaced.',
      ),
    },
  },
  DRep: {
    fields: {
      cip105Id: ALWAYS, // re-encoded from the CIP-129 credential
      isCip119Compliant: notBuilt(
        'Derivable from the metadata body this provider already fetches; this ' +
          'build does not compute it.',
      ),
      registrationByKind: absent(
        'Blockfrost records no per-kind registration, and `kind` is itself ' +
          'inferred from the presence of a metadata anchor.',
      ),
      liveVotingPower: refused(
        'Blockfrost reports the epoch-snapshot `amount` only.',
      ),
      delegators: tooCostlyRefused(
        'A delegator count means paging the whole delegator list per DRep; ' +
          'dreps.get throws, dreps.listDelegators serves the list itself.',
      ),
      activity: sometimes(
        'Filled whenever `last_active_epoch` resolves to a stamp — which is ' +
          'independent of `expand`; absent when the DRep has never been active.',
      ),
      adaHandles: absent('Blockfrost does not resolve Ada Handles.'),
    },
    misreported: [
      {
        field: 'kind',
        sends: "'directVoter' for any credential with no metadata anchor",
        wouldMean: 'the credential was registered as a sole/direct voter',
        note:
          'Blockfrost has no field for this, so deriveKind infers it from the ' +
          'anchor — the legacy API’s rule. A DRep that registered without an ' +
          'anchor is reported as a direct voter.',
      },
    ],
  },
  Registration: {
    fields: {
      status: ALWAYS, // retired > active, so expired reads as inactive
      registeredAt: sometimes(
        'From `active_epoch`, which is null on some records; stamped through ' +
          '/epochs/{n}.',
      ),
      registrationTx: notBuilt(
        '/governance/dreps/{id}/updates names it; the DRep read does not join ' +
          'it. Use dreps.listHistory.',
      ),
      retiredAt: absent(
        'Nothing dates a retirement: the updates listing carries no epoch.',
      ),
      retirementTx: notBuilt(
        'Also on the updates listing, also not joined into the DRep read.',
      ),
    },
    misreported: [
      {
        field: 'deposit',
        sends: 'null',
        wouldMean: 'the DRep paid no deposit, or none is recorded on chain',
        note:
          'Blockfrost exposes no deposit on a DRep record and the contract ' +
          'requires the key, so null is sent. It is "unknown", not "absent".',
      },
    ],
  },
  DRepActivity: {
    fields: {
      lastVotedAt: sometimes(
        'Filled from `last_active_epoch`, which the ledger refreshes on ANY ' +
          'activity (a vote, a registration or an update certificate), not ' +
          'only on a vote. Absent when the field is null.',
      ),
      notVotedCount: absent(
        'Counting actions a DRep did NOT vote on needs its voting record, ' +
          'which this provider cannot assemble at all.',
      ),
      inactiveFromEpoch: notBuilt(
        'Computable as last_active_epoch + drep_activity; not computed, since ' +
          'the parameter may have changed in between.',
      ),
      participationRate: absent('Derived from notVotedCount; see that field.'),
    },
    misreported: [
      {
        field: 'votesCast',
        sends: '0',
        wouldMean: 'the DRep has never voted',
        note:
          'Blockfrost gives no vote count — only the last epoch the DRep was ' +
          'active — and `votesCast` is required, so 0 is sent. Every DRep from ' +
          'this provider reads as having never voted.',
      },
    ],
  },
  DRepDelegator: {
    fields: {},
    misreported: [
      {
        field: 'since',
        sends: 'null',
        wouldMean: 'the delegation has no recorded start on chain',
        note:
          '/governance/dreps/{id}/delegators is a snapshot of {address, ' +
          'amount}; when the delegation began is not reported.',
      },
      {
        field: 'txRef',
        sends: 'null',
        wouldMean: 'no transaction is recorded for the delegation',
        note: 'The delegator snapshot names no certificate.',
      },
    ],
  },
  DRepHistoryEvent: {
    fields: {
      at: absent('/governance/dreps/{id}/updates dates nothing.'),
      anchor: absent('The updates listing carries no anchor for an update.'),
      changes: absent('No diff between successive registrations is computed.'),
    },
  },
  DRepVotingPowerEntry: {
    fields: {
      givenName: sometimes(
        'From the CIP-119 body when the DRep has one; absent for a credential ' +
          'with no anchor.',
      ),
    },
  },
  SpoVoter: {
    // Nothing here is reachable: pool.identity.current is refused because
    // /pools/{id} times out on this deployment.
    fields: {
      cip105Id: notBuilt('Pools are not served; see pool.identity.current.'),
      ticker: notBuilt('Pools are not served.'),
      name: notBuilt('Pools are not served.'),
      liveStake: notBuilt('Pools are not served.'),
      activeStake: notBuilt('Pools are not served.'),
      pledge: notBuilt('Pools are not served.'),
    },
  },
  CommitteeMember: {
    fields: {
      cip105Id: absent(
        'There is no committee resource, and committee credentials arrive as ' +
          'raw hex on a vote row, with no CIP-105 form.',
      ),
    },
  },
  Committee: { fields: {} },
  Constitution: { fields: {} },
  GovAction: {
    fields: {
      providerId: absent('Blockfrost has no internal row id to expose.'),
      body: sometimes(
        'Built for all seven types; omitted (leaving rawBody) when a ' +
          'description does not match the shape this Blockfrost version was ' +
          'verified to produce.',
      ),
      rawBody: sometimes(
        'The ledger description verbatim, when the record carries one.',
      ),
      deposit: ALWAYS,
      depositReturnAddress: ALWAYS,
      proposedBy: absent(
        'The proposal record names only where the deposit returns, not who ' +
          'submitted it.',
      ),
      tallies: ON_EXPAND,
      protocolParamsAtSubmission: refused(
        'Parameters in force at submission are not joined onto a proposal; ' +
          'proposals.list throws for expand: protocolParams.',
      ),
      protocolParamsAtEnactment: refused(
        'Parameters in force at enactment are not joined onto a proposal.',
      ),
      myVote: refused(
        'Annotating an action with the caller’s vote is one votes read per ' +
          'proposal; proposals.list and proposals.get both throw on voterId.',
      ),
    },
    misreported: [
      {
        field: 'previousAction',
        sends: 'null',
        wouldMean: 'the action names no predecessor of its type on chain',
        note:
          'Blockfrost’s proposal record has no previous-action pointer. null ' +
          'is "unknown", and a consumer that chains actions by it will build ' +
          'an empty chain.',
      },
      {
        field: 'metadata',
        sends: 'null whenever `expand` is given and omits `metadata`',
        wouldMean: 'the action carries no metadata anchor',
        note:
          '`hydrate` fetches /metadata only when `expand` is absent or names ' +
          '`metadata`, and the field is required, so `expand: ["tallies"]` ' +
          'returns an action with an anchor as `metadata: null` — ' +
          'indistinguishable from one that has none. Name `metadata` ' +
          'alongside whatever else you expand.',
      },
    ],
  },
  GovActionLifecycle: {
    fields: {
      submitted: absent(
        'The proposal record has no submission epoch or time. It could be ' +
          'guessed as expiration - gov_action_lifetime, which is wrong ' +
          'whenever that parameter has changed, so it is left unset.',
      ),
    },
  },
  GovActionActivityEvent: {
    fields: {
      voter: absent('The activity feed is not served; votes carry no date.'),
      vote: absent('The activity feed is not served; votes carry no date.'),
    },
  },
  EnactedActionSummary: {
    fields: {
      // getEnacted only builds a summary inside `enacted_epoch !== null`,
      // and stamping a non-null epoch always yields at least `{epoch}`, so a
      // summary that exists always carries this.
      enactedAt: ALWAYS,
      submittedTx: ALWAYS, // the record's own key
      body: notBuilt(
        'getEnacted reports the raw description only; it does not run ' +
          'buildBody over a scanned candidate.',
      ),
      rawBody: sometimes('Present when the record carries a description.'),
    },
  },
  RoleTally: {
    fields: {
      count: ALWAYS,
      stake: absent(
        'Blockfrost attaches no voting power to a vote, so a stake tally ' +
          'cannot be summed at all.',
      ),
      notVotedStake: absent('No eligible-but-not-voted figure is published.'),
      totalEligibleStake: absent(
        'The denominator would mean aggregating the whole distribution per ' +
          'action — one request per DRep.',
      ),
      threshold: absent(
        'Thresholds are float protocol parameters and there is no stake ' +
          'denominator to apply them to; a threshold on a head count would be ' +
          'meaningless.',
      ),
      passing: absent('Derived from a threshold that cannot be applied.'),
    },
  },
  VoteRecord: {
    fields: {
      at: tooCostly(
        'A vote is dated only through its transaction, and resolving that is ' +
          'one read per vote.',
      ),
    },
    misreported: [
      {
        field: 'votingPower',
        sends: 'null',
        wouldMean: 'the vote carried no voting power',
        note:
          'The power a vote carried is the voter’s stake at the time, which ' +
          'Blockfrost does not report alongside the vote. null is "unknown".',
      },
      {
        field: 'rationale',
        sends: 'null',
        wouldMean: 'the voter attached no rationale anchor',
        note:
          'A vote row carries no anchor, so an attached rationale is ' +
          'indistinguishable from none.',
      },
      {
        field: 'voter',
        sends: 'the raw hex credential as `id` for a committee voter',
        wouldMean: 'a CIP-129 bech32 id that can be used in a URL',
        note:
          'Blockfrost returns committee voters as bare hex — not bech32, and ' +
          'with no hot/cold or script indication. DRep and SPO ids are bech32 ' +
          'and normalised to CIP-129.',
      },
      {
        field: 'isCurrent',
        sends: 'true',
        wouldMean: 'the provider checked for a later, superseding vote',
        note:
          'The endpoint returns each voter’s current vote, so true is correct ' +
          'for every row it returns — but no superseded vote is ever visible ' +
          'and `includeSuperseded` cannot be honoured.',
      },
    ],
  },
  GovernanceMetrics: {
    fields: {
      epoch: tooCostly('The whole record is unavailable; see the list below.'),
      totalDRepDistribution: tooCostly(
        'Summing the DRep distribution is one request per DRep.',
      ),
      totalLiveGovernanceActions: tooCostly(
        'Counting live actions means hydrating the whole proposal directory.',
      ),
      totalSpoVotes: tooCostly(
        'Votes are only reachable per proposal, so counting them is one read ' +
          'per proposal.',
      ),
      totalCcVotes: tooCostly('Same as totalSpoVotes: one read per proposal.'),
      treasury: absent('/network answers 500 on this deployment.'),
    },
    // Required counters, so a consumer knows the dashboard is unavailable
    // tile by tile rather than as a whole.
    unfillable: [
      {
        field: 'uniqueDelegators',
        cause: 'tooExpensive',
        note: 'Every delegator of every DRep, unioned.',
      },
      {
        field: 'totalDelegations',
        cause: 'tooExpensive',
        note: 'Needs the delegator list of every DRep.',
      },
      {
        field: 'totalRegisteredDReps',
        cause: 'tooExpensive',
        note: 'The whole directory, paged — Blockfrost gives no count.',
      },
      {
        field: 'totalActiveDReps',
        cause: 'tooExpensive',
        note: 'Status is on the detail record; classifying all of them is a walk.',
      },
      {
        field: 'totalInactiveDReps',
        cause: 'tooExpensive',
        note: 'Status is on the detail record; classifying all of them is a walk.',
      },
      {
        field: 'totalActiveCip119CompliantDReps',
        cause: 'tooExpensive',
        note: 'Needs every DRep’s metadata document resolved.',
      },
      {
        field: 'totalRegisteredDirectVoters',
        cause: 'tooExpensive',
        note:
          'A direct voter is inferred from the absence of a metadata anchor, ' +
          'so counting them means one metadata read per DRep.',
      },
      {
        field: 'totalGovernanceActions',
        cause: 'tooExpensive',
        note: 'The whole proposal directory, paged; there is no count.',
      },
      {
        field: 'totalDRepVotes',
        cause: 'tooExpensive',
        note: 'Votes are reachable per proposal only: one read per proposal.',
      },
      {
        field: 'committee',
        cause: 'notInSource',
        note: 'Blockfrost has no committee resource, so size and quorum are absent.',
      },
    ],
  },
  TransactionState: {
    fields: {
      confirmations: notBuilt(
        'transactions.get is refused; /txs/{hash} 500s here.',
      ),
      includedAt: notBuilt(
        'transactions.get is refused; /txs/{hash} 500s here.',
      ),
      effects: notBuilt('transactions.get is refused; /txs/{hash} 500s here.'),
      votingProcedures: notBuilt(
        'transactions.get is refused; /txs/{hash} 500s here.',
      ),
    },
  },
  SurveyDefinition: { fields: {} },
};

/**
 * Where one route disagrees with its dataset about a field.
 *
 * `dreps.list` and `proposals.list` validate `expand` and throw; `dreps.get`
 * throws for its two. `proposals.get` validates only `voterId` and hands
 * `expand` straight to `hydrate`, which acts on `metadata` and `tallies` and
 * has no branch for the other three — so the same request that throws on the
 * listing is silently ignored on the single read. A consumer building an
 * `expand` array needs that difference; without these the dataset would have
 * to pick one answer and lie about the other route.
 */
const BLOCKFROST_FIELD_OVERRIDES: readonly FieldOverride[] = [
  {
    entity: 'DRep',
    field: 'delegators',
    route: 'governance.dreps.list',
    support: tooCostly(
      'dreps.list never inspects `expand`, so asking for delegators there is ' +
        'accepted and silently does nothing, where dreps.get throws.',
    ),
    note: 'List ignores the request; the single read refuses it.',
  },
  {
    entity: 'DRep',
    field: 'liveVotingPower',
    route: 'governance.dreps.list',
    support: absent(
      'dreps.list never inspects `expand`; dreps.get throws for this field.',
    ),
    note: 'List ignores the request; the single read refuses it.',
  },
  {
    entity: 'GovAction',
    field: 'myVote',
    route: 'governance.proposals.get',
    support: tooCostly(
      'proposals.get validates only `voterId`; `hydrate` has no branch for ' +
        'myVote, so it is accepted and does nothing, where proposals.list ' +
        'throws.',
    ),
    note: 'The single read ignores the expand; the listing refuses it.',
  },
  {
    entity: 'GovAction',
    field: 'protocolParamsAtSubmission',
    route: 'governance.proposals.get',
    support: absent(
      '`hydrate` acts on `metadata` and `tallies` only, so protocolParams is ' +
        'ignored on proposals.get rather than refused.',
    ),
    note: 'The single read ignores the expand; the listing refuses it.',
  },
  {
    entity: 'GovAction',
    field: 'protocolParamsAtEnactment',
    route: 'governance.proposals.get',
    support: absent(
      '`hydrate` acts on `metadata` and `tallies` only, so protocolParams is ' +
        'ignored on proposals.get rather than refused.',
    ),
    note: 'The single read ignores the expand; the listing refuses it.',
  },
];

/**
 * What `system.getCapabilities()` serves. `network` is replaced at runtime from
 * /genesis, since one build talks to whichever network it is pointed at.
 */
export const BLOCKFROST_CAPABILITY_DOCUMENT: ProviderCapabilityDocument = {
  schemaVersion: 2,
  provider: 'blockfrost',
  network: 'mainnet',
  providerVersion: '0.1.0',
  generatedAt: '2026-09-18T00:00:00Z',
  datasets: BLOCKFROST_DATASETS,
  entities: BLOCKFROST_ENTITIES,
  fieldOverrides: BLOCKFROST_FIELD_OVERRIDES,
  unreviewed: [],
  // Empty on purpose. The three broken endpoints on the verified deployment
  // (/txs/{hash} and /network 500, /pools/{id} 504) are declared in the table
  // above as `notImplemented` with `scope: 'deployment'`, because this build
  // refuses those routes unconditionally — fixing the deployment would not
  // make them work. A health probe that demoted a dataset at runtime would
  // emit its finding here instead.
  overrides: [],
  extensions: [],
  metadata: {
    // Blockfrost fetches and parses the off-chain document itself and returns
    // it as `json_metadata`.
    resolvedBy: 'provider',
    // It does not say whether the hash matched or whether the document
    // conforms to its CIP, so this provider reports `valid` when a body came
    // back and `pending` when it did not — never `invalid`.
    validatesAgainstStandard: false,
    // There is no retrieval-failure record at all: a document Blockfrost could
    // not fetch is simply a 404.
    carriesFailureMessage: false,
  },
};
