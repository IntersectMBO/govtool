import type {
  AnyCapabilityRefusal,
  ChainDataApiV1,
} from '@govtool/data-providers/chain-data';
import {
  DATASET_IDS,
  declarationProblems,
  isRouteCallable,
  readCapability,
  refusalIsDeclared,
  refusalKey,
  resolveCapabilities,
} from '@govtool/data-providers/chain-data';

import { FakeKoios } from './fake-http';
import { expectChainDataError } from './expect-error';
import { KoiosChainDataProvider } from '../src/provider';
import { koiosCapabilities, KOIOS_REFUSALS } from '../src/capabilities';
import { PROVIDER_ID } from '../src/api/system.api';
import { UNCOMPUTABLE_METRICS } from '../src/api/governance/metrics.api';

function provider(koios: FakeKoios): KoiosChainDataProvider {
  return new KoiosChainDataProvider(koios.client());
}

const DOC = koiosCapabilities('mainnet');
const TABLE = resolveCapabilities(DOC);

describe('the declaration itself', () => {
  it('has no problem the type system cannot catch', () => {
    expect(declarationProblems(DOC)).toEqual([]);
  });

  it('answers for every dataset in the registry', () => {
    const missing = DATASET_IDS.filter(
      (id) => readCapability(DOC.datasets, id) === undefined,
    );
    expect(missing).toEqual([]);
    // Nothing is deferred: `unreviewed` is the honest place for a dataset
    // nobody has assessed, and this provider has assessed all of them.
    expect(DOC.unreviewed).toEqual([]);
  });

  it('never refuses without saying why', () => {
    for (const id of DATASET_IDS) {
      const cap = readCapability(DOC.datasets, id);
      if (cap.reachability !== 'served') {
        expect(cap.unavailable?.reason.length).toBeGreaterThan(0);
      }
      for (const route of cap.refusedRoutes ?? []) {
        expect(route.unavailable.reason.length).toBeGreaterThan(0);
      }
    }
  });

  it('bakes in no deployment fault', () => {
    // A 500 on one endpoint is a property of the box, not of this build; it
    // belongs in `overrides`, emitted by a health probe, so it can clear
    // without a release.
    expect(DOC.overrides).toEqual([]);
    for (const id of DATASET_IDS) {
      expect(readCapability(DOC.datasets, id).unavailable?.kind).not.toBe(
        'deploymentFault',
      );
    }
  });
});

/**
 * The drift cross-check.
 *
 * Every `CAPABILITY_UNSUPPORTED` this package can throw is listed as a typed
 * refusal in `src/capabilities.ts`; this asserts the declaration predicts each
 * one. It is what makes the document a contract rather than documentation:
 * a refusal site added without a matching declaration fails here.
 */
describe('declared refusals', () => {
  it.each(KOIOS_REFUSALS.map((r) => [refusalKey(r), r] as const))(
    'predicts %s',
    (_key, refusal: AnyCapabilityRefusal) => {
      expect(refusalIsDeclared(TABLE, DOC.entities, refusal)).toBe(true);
    },
  );

  it('covers every refusal site, one entry per key', () => {
    const keys = KOIOS_REFUSALS.map(refusalKey);
    expect(new Set(keys).size).toBe(keys.length);
  });
});

/**
 * The four routes that throw whatever they are passed, paired with the call
 * that proves it. The declaration says they are unreachable; these say the
 * code agrees.
 */
const UNSUPPORTED_ROUTES: [
  (
    | 'governance.dreps.listDelegationEvents'
    | 'governance.voters.list'
    | 'governance.metrics.get'
    | 'surveys.getDefinition'
  ),
  (api: ChainDataApiV1) => Promise<unknown>,
][] = [
  [
    'governance.dreps.listDelegationEvents',
    (api) => api.governance.dreps.listDelegationEvents('drep1x'),
  ],
  ['governance.voters.list', (api) => api.governance.voters.list()],
  ['governance.metrics.get', (api) => api.governance.metrics.get()],
  ['surveys.getDefinition', (api) => api.surveys!.getDefinition('ab')],
];

describe('unreachable routes', () => {
  it.each(UNSUPPORTED_ROUTES)(
    '%s throws CAPABILITY_UNSUPPORTED, and the declaration says so',
    async (route, call) => {
      expect(isRouteCallable(TABLE, route)).toBe(false);
      const error = await expectChainDataError(call(provider(new FakeKoios())));
      expect(error.code).toBe('CAPABILITY_UNSUPPORTED');
      // The reason is carried, because "which endpoint is missing" is almost
      // never the answer for Koios — it is "which field".
      expect(error.details).toMatchObject({ route });
      expect(typeof error.details!['reason']).toBe('string');
    },
  );

  it('keeps every other route callable', () => {
    const refused = new Set<string>(UNSUPPORTED_ROUTES.map(([route]) => route));
    const broken: string[] = [];
    for (const id of DATASET_IDS) {
      for (const route of [
        ...(readCapability(DOC.datasets, id).refusedRoutes ?? []).map((r) =>
          String(r.route),
        ),
      ]) {
        if (!refused.has(route)) broken.push(route);
      }
    }
    expect(broken).toEqual([]);
  });
});

describe('the served document', () => {
  it('names the provider and the network it is pointed at', async () => {
    const koios = new FakeKoios().on('genesis', [{ networkmagic: '1' }]);
    const { data } = await provider(koios).system.getCapabilities();
    expect(data.provider).toBe(PROVIDER_ID);
    expect(data.network).toBe('preprod');
    expect(data.schemaVersion).toBe(2);
  });

  it('declares the metrics extension it actually ships', () => {
    expect(DOC.extensions).toEqual([
      expect.objectContaining({
        dataset: 'network.aggregate.current',
        method: 'governance.metrics.getAvailable',
        returns: 'partialRecord',
      }),
    ]);
  });

  it('names exactly the metrics fields the code cannot compute', () => {
    const unfillable = (DOC.entities.GovernanceMetrics.unfillable ?? []).map(
      (entry) => entry.field,
    );
    expect(unfillable.sort()).toEqual(Object.keys(UNCOMPUTABLE_METRICS).sort());
  });
});

/**
 * The three headline cases, asserted against the declaration rather than
 * against prose, so a future change to the provider that quietly starts
 * serving one of them shows up as a failing expectation to update.
 */
describe('the cases that motivated the capability layer', () => {
  it('cannot rank the DRep directory by anything', () => {
    const sort = readCapability(TABLE, 'drep.identity.current').sort!;
    expect(Object.values(sort)).toEqual([
      'rejected',
      'rejected',
      'rejected',
      'rejected',
      'rejected',
    ]);
  });

  it('keeps three of five proposal orderings', () => {
    const sort = readCapability(TABLE, 'proposal.identity.current').sort!;
    expect(sort['newest']).toBe('honoured');
    expect(sort['oldest']).toBe('honoured');
    expect(sort['soonestToExpire']).toBe('honoured');
    expect(sort['mostYesVotes']).toBe('rejected');
    expect(sort['highestParticipation']).toBe('rejected');
  });

  it('serves a proposal vote listing but cannot order it by power', () => {
    const cap = readCapability(TABLE, 'proposal.ballot.current');
    expect(cap.reachability).toBe('served');
    // Not `rejected`: the sort switch falls through to newest-first, so a
    // caller asking for this gets a chronological list presented as a ranking.
    expect(cap.sort!['votingPower']).toBe('ignored');
  });

  it('refuses a DRep delegation timeline no source records', () => {
    const cap = readCapability(TABLE, 'drep.delegation.events');
    expect(cap.reachability).toBe('refused');
    expect(cap.unavailable).toMatchObject({
      kind: 'notInSource',
      scope: 'source',
    });
  });

  it('refuses `live` where Blockfrost refuses `active`', () => {
    for (const id of [
      'drep.stake.current',
      'drep.stake.series',
      'drep.delegation.current',
      'network.stake.current',
    ] as const) {
      expect(readCapability(TABLE, id).basis).toEqual({
        active: 'honoured',
        live: 'rejected',
      });
    }
  });

  it('caps a page at 1000 rows, and an omitted limit at one of them', () => {
    const paging = readCapability(TABLE, 'drep.identity.current').paging!;
    expect(paging.maxLimit).toBe(1000);
    expect(paging.omittedLimitMeans).toBe('oneMaxPage');
    expect(paging.total).toBe('estimated');
  });
});

describe('metrics', () => {
  it('refuses the whole route and names every field it cannot compute', async () => {
    const error = await expectChainDataError(
      provider(new FakeKoios()).governance.metrics.get(),
    );
    expect(error.code).toBe('CAPABILITY_UNSUPPORTED');
    for (const field of Object.keys(UNCOMPUTABLE_METRICS)) {
      expect(error.message).toContain(field);
    }
  });

  it('serves the computable subset through getAvailable', async () => {
    const koios = new FakeKoios()
      .on('drep_epoch_summary', [
        { epoch_no: 656, amount: '15738903638210223', dreps: 866 },
      ])
      .on('committee_info', [
        {
          quorum_numerator: 2,
          quorum_denominator: 3,
          members: [{}, {}, {}, {}, {}, {}, {}],
        },
      ])
      .on('totals', [{ epoch_no: 656, treasury: '1500', reserves: '90' }])
      .on('drep_list', [], { total: 1402 })
      .on('proposal_list', [], { total: 310 })
      .on('vote_list', [], { total: 9000 });

    const { data } = await provider(koios).governance.metrics.getAvailable();

    expect(data).toMatchObject({
      epoch: 656,
      totalDRepDistribution: '15738903638210223',
      totalRegisteredDReps: 1402,
      totalGovernanceActions: 310,
      committee: { size: 7, quorum: { numerator: 2, denominator: 3 } },
      treasury: { balance: '1500', reserves: '90' },
    });
    // The fields Koios cannot compute stay absent rather than becoming zero.
    for (const field of Object.keys(UNCOMPUTABLE_METRICS)) {
      expect(data).not.toHaveProperty(field);
    }
  });

  it('counts without serialising rows', async () => {
    const koios = new FakeKoios().on('drep_list', [], { total: 1402 });
    await provider(koios).governance.metrics.getAvailable();
    const call = koios.lastCallTo('drep_list')!;
    expect(call.params['limit']).toBe('0');
    expect(call.params['select']).toBe('drep_id');
    expect(call.headers['prefer']).toBe('count=exact');
  });
});

describe('voter resolution', () => {
  it.each([
    ['drep1ytc6867ae0xmkekvmex79r9akyy28eu8nu03jf3xu9fle6c82l4eq', 'drep_info'],
    ['pool1m83drqwlugdt9jn7jkz8hx3pne53acfkd539d9cj8yr92dr4k9y', 'pool_info'],
    [
      'cc_hot1qgsl88anw66s9yg9hqzux6yujm3wwgcd8mjdt92pp7qn7nqcsc3nq',
      'committee_info',
    ],
  ])('dispatches %s on its bech32 prefix', async (id, endpoint) => {
    const koios = new FakeKoios()
      .on('drep_info', [
        {
          drep_id: id,
          hex: null,
          has_script: false,
          drep_status: 'registered',
          active: true,
          deposit: null,
          expires_epoch_no: null,
          amount: '1',
          meta_url: null,
          meta_hash: null,
          live_delegator_count: 0,
        },
      ])
      .on('drep_updates', [])
      .on('pool_info', [
        { pool_id_bech32: id, pool_id_hex: 'ab', pool_status: 'registered' },
      ])
      .on('committee_info', [
        {
          quorum_numerator: 2,
          quorum_denominator: 3,
          members: [
            {
              status: 'authorized',
              cc_hot_id: id,
              cc_cold_id: 'cc_cold1x',
              cc_hot_hex: 'aa',
              cc_cold_hex: 'bb',
              expiration_epoch: 799,
              cc_hot_has_script: false,
              cc_cold_has_script: false,
            },
          ],
        },
      ]);

    await provider(koios).governance.voters.resolve(id);
    expect(koios.callsTo(endpoint).length).toBeGreaterThan(0);
  });

  it('raises NOT_FOUND for an id that is none of the three', async () => {
    const error = await expectChainDataError(
      provider(new FakeKoios()).governance.voters.resolve('stake1abc'),
    );
    expect(error.code).toBe('NOT_FOUND');
  });
});

/**
 * The drift audit: every field the declaration calls `conditional` rather than
 * `always`, paired with the call that produces it absent.
 *
 * These exist because `serves: 'always'` is the easiest value to get wrong —
 * nothing fails when a field is silently missing, so the claim rots quietly.
 * Each case below was an `always` in the first draft of this declaration.
 */
describe('fields the declaration does not claim are always there', () => {
  function supportOf(entity: keyof typeof DOC.entities, field: string): string {
    const table = DOC.entities[entity].fields as Record<
      string,
      { serves: string }
    >;
    return table[field]!.serves;
  }

  it('Treasury.delta needs a second /totals row it does not always get', async () => {
    expect(supportOf('Treasury', 'delta')).toBe('conditional');
    const koios = new FakeKoios().on('totals', [
      { epoch_no: 0, treasury: '10', reserves: '5' },
    ]);
    const { data } = await provider(koios).network.getTreasury();
    expect(data.delta).toBeUndefined();
  });

  it('the Conway protocol parameters are absent for a pre-Conway epoch', async () => {
    const conwayOnly = [
      'govActionDeposit',
      'drepDeposit',
      'govActionLifetime',
      'drepActivity',
      'committeeMinSize',
      'committeeMaxTermLength',
    ] as const;
    for (const field of conwayOnly) {
      expect(supportOf('ProtocolParams', field)).toBe('conditional');
    }

    const koios = new FakeKoios().on('epoch_params', [
      {
        epoch_no: 250,
        min_fee_a: 44,
        min_fee_b: 155381,
        key_deposit: '2000000',
        pool_deposit: '500000000',
        coins_per_utxo_size: null,
        min_fee_ref_script_cost_per_byte: null,
        protocol_major: 4,
        protocol_minor: 0,
        gov_action_deposit: null,
        drep_deposit: null,
        drep_activity: null,
        gov_action_lifetime: null,
        committee_min_size: null,
        committee_max_term_length: null,
      },
    ]);
    const { data } = await provider(koios).network.getProtocolParams({
      epoch: 250,
    });
    const params = data as unknown as Record<string, unknown>;
    for (const field of conwayOnly) {
      expect(params[field]).toBeUndefined();
    }
    // The two the contract types as nullable are populated in every era.
    expect(data.coinsPerUtxoByte).toBeNull();
    expect(data.minFeeRefScriptCostPerByte).toBeNull();
  });

  it('NetworkInfo drops the magic and the era when the rows omit them', async () => {
    expect(supportOf('NetworkInfo', 'networkMagic')).toBe('conditional');
    expect(supportOf('NetworkInfo', 'era')).toBe('conditional');

    const koios = new FakeKoios()
      .on('tip', [
        {
          hash: 'aa',
          epoch_no: 500,
          abs_slot: 1,
          epoch_slot: 1,
          block_height: 9,
          block_time: 1700000000,
        },
      ])
      .on('genesis', [])
      .on('epoch_info', []);
    const { data } = await provider(koios).network.getNetworkInfo();
    expect(data.networkMagic).toBeUndefined();
    expect(data.era).toBeUndefined();
  });

  it('DRepActivity.inactiveFromEpoch needs an expiry epoch', async () => {
    expect(supportOf('DRepActivity', 'inactiveFromEpoch')).toBe('conditional');

    const id = 'drep1ytc6867ae0xmkekvmex79r9akyy28eu8nu03jf3xu9fle6c82l4eq';
    const koios = new FakeKoios()
      .on('drep_info', [
        {
          drep_id: id,
          hex: null,
          has_script: false,
          drep_status: 'registered',
          active: true,
          deposit: null,
          // A retired DRep and the predefined options have no expiry.
          expires_epoch_no: null,
          amount: '1',
          meta_url: null,
          meta_hash: null,
          live_delegator_count: 0,
        },
      ])
      .on('drep_updates', [])
      .on('drep_votes', [], { total: 3 });

    const { data } = await provider(koios).governance.dreps.get(id, {
      expand: ['activity'],
    });
    expect(data.activity!.votesCast).toBe(3);
    expect(data.activity!.inactiveFromEpoch).toBeUndefined();
  });
});

/**
 * The other half: required-nullable fields filled with a plausible falsehood.
 *
 * `null` on a required field means "known to be absent on chain". Where Koios
 * simply does not report the value, sending `null` misleads, and the only
 * honest place to say so is `misreported` — there is no optional key to leave
 * off. A consumer that trusts these renders "never delegated" over a real
 * delegation, so each one is pinned to the call that produces it.
 */
describe('required fields sent as a plausible falsehood', () => {
  function misreports(
    entity: keyof typeof DOC.entities,
    field: string,
  ): boolean {
    return (DOC.entities[entity].misreported ?? []).some(
      (entry) => String(entry.field) === field,
    );
  }

  it('a delegator row has neither a join time nor a transaction', async () => {
    expect(misreports('DRepDelegator', 'since')).toBe(true);
    expect(misreports('DRepDelegator', 'txRef')).toBe(true);

    const koios = new FakeKoios().on('drep_delegators', [
      {
        stake_address: 'stake1x',
        stake_address_hex: 'aa',
        script_hash: null,
        // Koios does carry an epoch here; it is the snapshot's, not the
        // delegation's, which is why it is not passed off as `since`.
        epoch_no: 655,
        amount: '100',
      },
    ]);
    const { data } = await provider(koios).governance.dreps.listDelegators(
      'drep1ytc6867ae0xmkekvmex79r9akyy28eu8nu03jf3xu9fle6c82l4eq',
    );
    expect(data.elements[0]!.since).toBeNull();
    expect(data.elements[0]!.txRef).toBeNull();
  });

  it('getDelegation sends a null txRef although a certificate exists', async () => {
    expect(misreports('Delegation', 'txRef')).toBe(true);

    const koios = new FakeKoios().on('account_info', [
      {
        stake_address: 'stake1x',
        status: 'registered',
        delegated_pool: null,
        delegated_drep:
          'drep1ytc6867ae0xmkekvmex79r9akyy28eu8nu03jf3xu9fle6c82l4eq',
        total_balance: '0',
        utxo: '0',
        rewards: '0',
        withdrawals: '0',
        rewards_available: '0',
        deposit: '0',
        reserves: '0',
        treasury: '0',
      },
    ]);
    const { data } = await provider(koios).accounts.getDelegation(
      'stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw',
    );
    expect(data!.target.kind).toBe('drep');
    expect(data!.txRef).toBeNull();
  });

  it('a pool delegation has no start even when the certificate is in hand', async () => {
    expect(misreports('PoolDelegation', 'since')).toBe(true);

    const koios = new FakeKoios()
      .on('account_info', [
        {
          stake_address: 'stake1x',
          status: 'registered',
          delegated_pool: 'pool1x',
          delegated_drep: null,
          total_balance: '0',
          utxo: '0',
          rewards: '0',
          withdrawals: '0',
          rewards_available: '0',
          deposit: '0',
          reserves: '0',
          treasury: '0',
        },
      ])
      .on('account_updates', [
        {
          stake_address: 'stake1x',
          updates: [
            {
              action_type: 'delegation_pool',
              tx_hash: 'ab',
              epoch_no: 500,
              epoch_slot: 1,
              absolute_slot: 2,
              block_time: 1700000000,
            },
          ],
        },
      ]);
    const { data } = await provider(koios).accounts.get(
      'stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw',
      { expand: ['poolDelegation'] },
    );
    // The transaction IS filled from the certificate; its date is not.
    expect(data.poolDelegation!.txRef).not.toBeNull();
    expect(data.poolDelegation!.since).toBeNull();
  });
});

/**
 * Caveats: 200 responses that mean less than they look like they mean.
 */
describe('what a served answer actually covers', () => {
  it('the pool directory is registered pools only, and says so', async () => {
    const caveats =
      readCapability(TABLE, 'pool.identity.current').caveats ?? [];
    expect(caveats).toContainEqual(
      expect.objectContaining({
        kind: 'impliedFilter',
        param: 'pool_status',
        restrictedTo: ['registered'],
      }),
    );

    const koios = new FakeKoios().on('pool_list', []);
    await provider(koios).governance.pools.list();
    expect(koios.lastCallTo('pool_list')!.params['pool_status']).toBe(
      'eq.registered',
    );
  });

  it("a DRep's own vote listing carries the rationale anchor and never the body", async () => {
    for (const id of ['drep.ballot.current', 'pool.ballot.current'] as const) {
      expect(readCapability(TABLE, id).caveats).toContainEqual(
        expect.objectContaining({ kind: 'notExhaustive' }),
      );
      // The expand is still honoured: the caller does get a projection.
      expect(readCapability(TABLE, id).expand!['rationale']).toBe('honoured');
    }

    const koios = new FakeKoios().on('pool_votes', [
      {
        proposal_id:
          'gov_action1ffe4f5k6nlvrl0hcq2qcl9rlqxy4z3k6lqxq9xayp2gfz4z8t3ysqu4gzpp',
        proposal_tx_hash: 'ab',
        proposal_index: 0,
        vote_tx_hash: 'cd',
        block_time: 1700000000,
        vote: 'Yes',
        meta_url: 'https://example.test/rationale.json',
        meta_hash: 'ff',
      },
    ]);
    const { data } = await provider(koios).governance.pools.listVotes(
      'pool1m83drqwlugdt9jn7jkz8hx3pne53acfkd539d9cj8yr92dr4k9y',
    );
    const rationale = data.elements[0]!.rationale!;
    expect(rationale.anchor.url).toBe('https://example.test/rationale.json');
    // `pending` reads as "not resolved yet"; on this route it never will be.
    expect(rationale.status).toBe('pending');
    expect(rationale.body).toBeUndefined();
  });

  it('a delegation history drops an event it cannot resolve a target for', async () => {
    expect(
      readCapability(TABLE, 'account.delegation.events').caveats,
    ).toContainEqual(expect.objectContaining({ kind: 'notExhaustive' }));

    const koios = new FakeKoios()
      .on('account_updates', [
        {
          stake_address: 'stake1x',
          updates: [
            {
              action_type: 'delegation_drep',
              tx_hash: 'ab',
              epoch_no: 500,
              epoch_slot: 1,
              absolute_slot: 2,
              block_time: 1700000000,
            },
          ],
        },
      ])
      // /tx_info answers, but with no certificate for this account.
      .on('tx_info', [{ tx_hash: 'ab', certificates: [] }]);

    const { data } = await provider(koios).accounts.listDelegationHistory(
      'stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw',
    );
    expect(data.elements).toEqual([]);
    // And `total` counts the survivors, not the certificates.
    expect(data.total).toBe(0);
  });
});
