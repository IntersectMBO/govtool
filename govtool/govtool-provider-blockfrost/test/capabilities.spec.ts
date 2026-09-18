import { readdirSync, readFileSync } from 'node:fs';
import { join } from 'node:path';

import type {
  AnyCapabilityRefusal,
  ChainDataApiV1,
} from '@govtool/data-providers/chain-data';
import {
  declarationProblems,
  deriveFeatures,
  refusalIsDeclared,
  resolveCapabilities,
} from '@govtool/data-providers/chain-data';

import { BLOCKFROST_CAPABILITY_DOCUMENT as DOC } from '../src/capabilities';
import { BlockfrostChainDataProvider } from '../src';
import { bfEpoch, bfProposal, FakeBlockfrost, PROPOSAL_TX } from './fake-http';

/* ------------------------------------------------------------------------- */
/* Every refusal site in this package, and the refusal it stands for          */
/* ------------------------------------------------------------------------- */

/**
 * Keyed by the literal first argument of each `unsupported(...)` call in
 * `src/`, so the scan below can prove the two sides match. A template site
 * (`` `accounts.get#${field}` ``) maps to one refusal per field it can name.
 *
 * These objects are the typed dual of the string-keyed error this package
 * throws today: `unsupported()` carries `{route, reason}` in `details`, not a
 * `CapabilityRefusal`, so the correspondence is asserted here rather than
 * produced by the thrower. Teaching `unsupported()` to take a
 * `CapabilityRefusal` would make it mechanical.
 */
const REFUSAL_SITES: Readonly<Record<string, readonly AnyCapabilityRefusal[]>> =
  {
    /* -- network ------------------------------------------------------------ */
    'network.getTreasury': [
      {
        dataset: 'network.treasury.current',
        control: { kind: 'dataset' },
        cause: 'notImplemented',
        scope: 'deployment',
        reason: '/network answers 500 on the verified deployment.',
      },
    ],

    /* -- accounts ----------------------------------------------------------- */
    'accounts.get#${field}': [
      {
        dataset: 'account.identity.current',
        control: { kind: 'expand', field: 'votingPower' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Voting power is per DRep, never per stake account.',
      },
      {
        dataset: 'account.identity.current',
        control: { kind: 'expand', field: 'drep' },
        cause: 'notImplemented',
        scope: 'source',
        reason: 'Resolving the account’s own DRep record is a second read.',
      },
      {
        dataset: 'account.identity.current',
        control: { kind: 'expand', field: 'adaHandles' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Blockfrost does not resolve Ada Handles.',
      },
    ],
    'accounts.getVotingPower': [
      {
        dataset: 'account.stake.current',
        control: { kind: 'dataset' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Voting power is per DRep, never per stake account.',
      },
      {
        dataset: 'account.stake.asAt',
        control: { kind: 'dataset' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'The same route serves both temporalities, and refuses both.',
      },
    ],
    'accounts.listDelegationHistory{kind=governance}': [
      {
        dataset: 'account.delegation.events',
        control: { kind: 'filter', name: 'kind', value: 'governance' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Only the current drep_id is on the account; no history.',
      },
    ],

    /* -- dreps -------------------------------------------------------------- */
    'governance.dreps.list{sort}': [
      {
        dataset: 'drep.identity.current',
        control: { kind: 'sortControl' },
        cause: 'noIndex',
        scope: 'source',
        reason:
          'Sorting needs the whole collection; the directory is ids only.',
      },
      {
        dataset: 'drep.identity.current',
        control: { kind: 'sort', key: 'votingPower' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'The directory carries no amount to order by.',
      },
      {
        dataset: 'drep.identity.current',
        control: { kind: 'sort', key: 'registrationDate' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'The directory carries no registration epoch to order by.',
      },
      {
        dataset: 'drep.identity.current',
        control: { kind: 'sort', key: 'activity' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'Activity is on the detail record, not the directory.',
      },
      {
        dataset: 'drep.identity.current',
        control: { kind: 'sort', key: 'status' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'Status is on the detail record, not the directory.',
      },
      {
        dataset: 'drep.identity.current',
        control: { kind: 'sort', key: 'random' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'Shuffling one hydrated page would misrepresent the order.',
      },
    ],
    'governance.dreps.get#${field}': [
      {
        dataset: 'drep.identity.current',
        control: { kind: 'expand', field: 'delegators' },
        cause: 'tooExpensive',
        scope: 'source',
        reason: 'A delegator count means paging the whole delegator list.',
        fallback: 'drep.delegation.current',
      },
      {
        dataset: 'drep.identity.current',
        control: { kind: 'expand', field: 'liveVotingPower' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Blockfrost reports the epoch-snapshot amount only.',
      },
    ],
    'governance.dreps.getVotingPower{fromEpoch,toEpoch}': [
      {
        dataset: 'drep.stake.series',
        control: { kind: 'dataset' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Only the current amount is reported; there is no history.',
      },
    ],
    'governance.dreps.getVotingPower{basis=live}': [
      {
        dataset: 'drep.stake.current',
        control: { kind: 'basis', basis: 'live' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Blockfrost reports the epoch-snapshot amount only.',
      },
    ],
    'governance.dreps.getVotingPowers{all}': [
      {
        dataset: 'drep.stake.current',
        control: { kind: 'batch', form: 'all' },
        cause: 'tooExpensive',
        scope: 'source',
        reason: 'Without ids this is one request per DRep in the directory.',
        fallback: 'drep.stake.current',
      },
    ],
    'governance.dreps.listVotes': [
      {
        dataset: 'drep.ballot.current',
        control: { kind: 'dataset' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'The per-DRep votes endpoint never names the proposal.',
      },
    ],
    'governance.dreps.listDelegators{basis=active}': [
      {
        dataset: 'drep.delegation.current',
        control: { kind: 'basis', basis: 'active' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Each delegator’s LIVE stake is reported, not the snapshot.',
      },
    ],
    'governance.dreps.listDelegationEvents': [
      {
        dataset: 'drep.delegation.events',
        control: { kind: 'dataset' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'The delegator list is a snapshot with no join or leave times.',
      },
    ],

    /* -- proposals ---------------------------------------------------------- */
    'governance.proposals.list{sort}': [
      {
        dataset: 'proposal.identity.current',
        control: { kind: 'sortControl' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'Sorting requires hydrating every proposal.',
      },
      {
        dataset: 'proposal.identity.current',
        control: { kind: 'sort', key: 'newest' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'The directory row carries no date to order by.',
      },
      {
        dataset: 'proposal.identity.current',
        control: { kind: 'sort', key: 'oldest' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'The directory row carries no date to order by.',
      },
      {
        dataset: 'proposal.identity.current',
        control: { kind: 'sort', key: 'soonestToExpire' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'The expiry epoch is on the detail record only.',
      },
      {
        dataset: 'proposal.identity.current',
        control: { kind: 'sort', key: 'mostYesVotes' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'Vote counts need one votes read per proposal.',
      },
      {
        dataset: 'proposal.identity.current',
        control: { kind: 'sort', key: 'highestParticipation' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'Participation needs one votes read per proposal.',
      },
    ],
    'governance.proposals.list{voterId}': [
      {
        dataset: 'proposal.identity.current',
        control: { kind: 'join', join: 'callerVote' },
        cause: 'tooExpensive',
        scope: 'source',
        reason: 'Per-voter annotation means one votes read per proposal.',
        fallback: 'proposal.ballot.current',
      },
    ],
    'governance.proposals.list#${field}': [
      {
        dataset: 'proposal.identity.current',
        control: { kind: 'expand', field: 'thresholds' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Thresholds are float parameters with no stake denominator.',
      },
      {
        dataset: 'proposal.identity.current',
        control: { kind: 'expand', field: 'protocolParams' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Parameters in force are not joined onto a proposal.',
      },
      {
        dataset: 'proposal.identity.current',
        control: { kind: 'expand', field: 'myVote' },
        cause: 'tooExpensive',
        scope: 'source',
        reason: 'One votes read per proposal.',
        fallback: 'proposal.ballot.current',
      },
    ],
    'governance.proposals.get{voterId}': [
      {
        dataset: 'proposal.identity.current',
        control: { kind: 'join', join: 'callerVote' },
        cause: 'tooExpensive',
        scope: 'source',
        reason: 'Per-voter annotation means a second votes read.',
        fallback: 'proposal.ballot.current',
      },
    ],
    'governance.proposals.listVotes{sort}': [
      {
        dataset: 'proposal.ballot.current',
        control: { kind: 'sortControl' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Votes carry neither a timestamp nor a voting power.',
      },
      {
        dataset: 'proposal.ballot.current',
        control: { kind: 'sort', key: 'newest' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Votes carry no timestamp.',
      },
      {
        dataset: 'proposal.ballot.current',
        control: { kind: 'sort', key: 'oldest' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Votes carry no timestamp.',
      },
      {
        dataset: 'proposal.ballot.current',
        control: { kind: 'sort', key: 'votingPower' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'No power is attached to a vote, so it cannot order by one.',
      },
    ],
    'governance.proposals.listActivity': [
      {
        dataset: 'proposal.identity.events',
        control: { kind: 'dataset' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Neither the submission nor the votes are dated.',
      },
    ],

    /* -- votes, pools, committee, voters, metrics ---------------------------- */
    'governance.votes.list': [
      {
        dataset: 'vote.ballot.current',
        control: { kind: 'dataset' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'There is no cross-cutting vote feed.',
      },
    ],
    'governance.votes.get': [
      {
        dataset: 'vote.ballot.current',
        control: { kind: 'dataset' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'No route resolves a vote from its transaction hash.',
      },
      {
        dataset: 'vote.metadata.current',
        control: { kind: 'dataset' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'A vote row carries no anchor.',
      },
    ],
    'governance.pools.list': [
      {
        dataset: 'pool.identity.current',
        control: { kind: 'dataset' },
        // `notImplemented`, not `deploymentFault`: the code rejects
        // unconditionally, so repairing the 504 would not make the call work.
        // The declaration says the same — see pool.identity.current.
        cause: 'notImplemented',
        scope: 'deployment',
        reason: '/pools/{id} times out (504) on the verified deployment.',
      },
    ],
    'governance.pools.get': [
      {
        dataset: 'pool.identity.current',
        control: { kind: 'dataset' },
        // `notImplemented`, not `deploymentFault`: the code rejects
        // unconditionally, so repairing the 504 would not make the call work.
        // The declaration says the same — see pool.identity.current.
        cause: 'notImplemented',
        scope: 'deployment',
        reason: '/pools/{id} times out (504) on the verified deployment.',
      },
    ],
    'governance.pools.listVotes': [
      {
        dataset: 'pool.ballot.current',
        control: { kind: 'dataset' },
        cause: 'noIndex',
        scope: 'source',
        reason: 'Votes are indexed per proposal, not per pool.',
      },
    ],
    'governance.committee.getCommittee': [
      {
        dataset: 'committee.identity.current',
        control: { kind: 'dataset' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'There is no committee resource.',
      },
    ],
    'governance.committee.getMember': [
      {
        dataset: 'committee.identity.current',
        control: { kind: 'dataset' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'There is no committee resource.',
      },
    ],
    'governance.committee.getConstitution': [
      {
        dataset: 'constitution.identity.current',
        control: { kind: 'dataset' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'There is no constitution resource.',
      },
    ],
    'governance.committee.listConstitutionHistory': [
      {
        dataset: 'constitution.identity.events',
        control: { kind: 'dataset' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'There is no constitution resource.',
      },
    ],
    'governance.voters.resolve': [
      {
        dataset: 'voter.identity.current',
        control: { kind: 'dataset' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'Resolving SPO and committee voters needs absent resources.',
      },
    ],
    'governance.voters.list': [
      {
        dataset: 'voter.identity.list',
        control: { kind: 'dataset' },
        cause: 'notInSource',
        scope: 'source',
        reason: 'There is no role-agnostic voter directory.',
      },
    ],
    'governance.metrics.get': [
      {
        dataset: 'network.aggregate.current',
        control: { kind: 'dataset' },
        cause: 'tooExpensive',
        scope: 'source',
        reason: 'Every counter is a collection-wide aggregate.',
        fallback: 'drep.identity.current',
      },
    ],

    /* -- transactions -------------------------------------------------------- */
    'transactions.get': [
      {
        dataset: 'transaction.identity.current',
        control: { kind: 'dataset' },
        cause: 'notImplemented',
        scope: 'deployment',
        reason: '/txs/{hash} answers 500 here for every hash.',
      },
    ],
  };

/** The first argument of every `unsupported(...)` call under `src/`. */
function refusalSitesInSource(): string[] {
  const root = join(__dirname, '..', 'src');
  const files: string[] = [];
  const walk = (dir: string): void => {
    for (const entry of readdirSync(dir, { withFileTypes: true })) {
      const full = join(dir, entry.name);
      if (entry.isDirectory()) walk(full);
      else if (entry.name.endsWith('.ts') && entry.name !== 'errors.ts') {
        files.push(full);
      }
    }
  };
  walk(root);

  const sites = new Set<string>();
  const pattern = /unsupported\(\s*(?:'([^']*)'|`([^`]*)`)/g;
  for (const file of files) {
    const source = readFileSync(file, 'utf8');
    for (const match of source.matchAll(pattern)) {
      sites.add(match[1] ?? match[2] ?? '');
    }
  }
  return [...sites].sort();
}

describe('the capability declaration', () => {
  it('has no problems the type system cannot catch', () => {
    expect(declarationProblems(DOC)).toEqual([]);
  });

  it('declares every dataset — nothing is left unreviewed', () => {
    expect(DOC.unreviewed).toEqual([]);
  });

  it('derives a feature set without throwing', () => {
    expect(() => deriveFeatures(DOC, 'sha256:test')).not.toThrow();
  });
});

describe('the declaration and the code agree', () => {
  it('knows every refusal site in src/, and invents none', () => {
    expect(refusalSitesInSource()).toEqual(Object.keys(REFUSAL_SITES).sort());
  });

  it('predicts every refusal this provider can throw', () => {
    const table = resolveCapabilities(DOC);
    const undeclared: string[] = [];

    for (const [site, refusals] of Object.entries(REFUSAL_SITES)) {
      for (const refusal of refusals) {
        if (!refusalIsDeclared(table, DOC.entities, refusal)) {
          undeclared.push(`${site} → ${refusal.dataset}`);
        }
      }
    }

    expect(undeclared).toEqual([]);
  });
});

describe('the facts that cost most to get wrong', () => {
  it('declares a DRep page as unpollable', () => {
    const dreps = DOC.datasets['drep.identity.current'];
    // /governance/dreps is ids only, so a page costs detail + metadata PER
    // ELEMENT. Support is binary, so that does not downgrade the declaration —
    // it is served. What it does mean is that no component may put it on an
    // interval, which is the one operational fact the declaration still carries.
    expect(dreps.pollable).toBe(false);
  });

  it('never claims an omitted limit returns everything', () => {
    for (const dataset of Object.values(DOC.datasets)) {
      expect(dataset.paging?.omittedLimitMeans ?? 'routeDefault').not.toBe(
        'everything',
      );
    }
  });

  it('declares page-local filters as approximated, not honoured', () => {
    const dreps = DOC.datasets['drep.identity.current'];
    expect(dreps.filters?.status?.values.active).toBe('approximated');
    expect(dreps.filters?.status?.exhaustive).toBe(false);

    const proposals = DOC.datasets['proposal.identity.current'];
    expect(proposals.filters?.status?.values.live).toBe('approximated');
    // `type` is CHEAPER than the others — the directory row carries
    // governance_type, so non-matching refs are dropped before hydration —
    // but it is no more exhaustive: `/governance/proposals` takes no type
    // parameter, so it still filters the page that was fetched. The test
    // below proves it rather than taking the declaration's word.
    expect(proposals.filters?.type?.values.InfoAction).toBe('approximated');
    expect(proposals.filters?.type?.exhaustive).toBe(false);
  });

  it('proves the type filter really is page-local', async () => {
    const refs = [
      { tx_hash: PROPOSAL_TX, cert_index: 0, governance_type: 'info_action' },
      { tx_hash: PROPOSAL_TX, cert_index: 1, governance_type: 'new_committee' },
      { tx_hash: PROPOSAL_TX, cert_index: 2, governance_type: 'info_action' },
      { tx_hash: PROPOSAL_TX, cert_index: 3, governance_type: 'new_committee' },
      { tx_hash: PROPOSAL_TX, cert_index: 4, governance_type: 'new_committee' },
    ];
    const bf = new FakeBlockfrost().on('/epochs/latest', bfEpoch(656));
    bf.on('/governance/proposals', refs);
    refs.forEach((ref, i) => {
      const base = `/governance/proposals/${PROPOSAL_TX}/${i}`;
      bf.on(
        base,
        bfProposal({
          cert_index: i,
          governance_type: ref.governance_type,
          governance_description: null,
        }),
      );
      bf.onStatus(`${base}/metadata`, 404);
    });

    const provider = new BlockfrostChainDataProvider(bf.client());
    const { data } = await provider.governance.proposals.list({
      limit: 5,
      type: ['InfoAction'],
    });

    // The directory request carries no filter at all...
    expect(bf.callsTo('/governance/proposals')[0]?.search).toEqual({
      count: '5',
      page: '1',
      order: 'desc',
    });
    // ...so a limit of 5 comes back as 2, with more InfoActions on page 2.
    expect(data.elements).toHaveLength(2);
    expect(data.nextCursor).toBe('2');
  });

  it('declares the tally as a head count where the ledger weighs stake', () => {
    const caveats = DOC.datasets['proposal.tally.current'].caveats ?? [];
    expect(caveats).toContainEqual(
      expect.objectContaining({
        kind: 'differentUnit',
        reports: 'count',
        ledgerDecidesBy: 'stake',
      }),
    );
    expect(DOC.entities.RoleTally.fields.stake.serves).toBe('never');
    expect(DOC.entities.RoleTally.fields.threshold.serves).toBe('never');
  });

  it('names the three required fields it is forced to misreport', () => {
    expect(DOC.entities.Registration.misreported).toContainEqual(
      expect.objectContaining({ field: 'deposit', sends: 'null' }),
    );
    expect(DOC.entities.DRepActivity.misreported).toContainEqual(
      expect.objectContaining({ field: 'votesCast', sends: '0' }),
    );
    expect(DOC.entities.GovAction.misreported).toContainEqual(
      expect.objectContaining({ field: 'previousAction', sends: 'null' }),
    );
  });

  it('declares surveys MISSING, and the namespace really is absent', () => {
    expect(DOC.datasets['survey.body.current'].reachability).toBe('missing');
    // Typed as the contract, where `surveys` is optional — the property is
    // genuinely not on BlockfrostChainDataProvider at all.
    const provider: ChainDataApiV1 = new BlockfrostChainDataProvider(
      new FakeBlockfrost().client(),
    );
    // Not a rejected promise — `provider.surveys` is undefined, so a consumer
    // must check before dereferencing.
    expect(provider.surveys).toBeUndefined();
  });
});
