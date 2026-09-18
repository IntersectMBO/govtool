import { BlockfrostChainDataProvider } from '../src';
import { bfBlock, bfEpoch, FakeBlockfrost, GENESIS } from './fake-http';

const STAKE = 'stake1u97v0sjx96u5lydjfe2g5qdwkj6plm87h80q5vc0ma6wjpq22mh4c';

function provider(bf: FakeBlockfrost) {
  return new BlockfrostChainDataProvider(bf.client());
}

function bfAccount(overrides: Record<string, unknown> = {}) {
  return {
    stake_address: STAKE,
    active: true,
    active_epoch: 236,
    controlled_amount: '45000000000000000',
    rewards_sum: '162579856329',
    withdrawals_sum: '163579856329',
    reserves_sum: '0',
    treasury_sum: '0',
    withdrawable_amount: '0',
    pool_id: 'pool1kchver88u3kygsak8wgll7htr8uxn5v35lfrsyy842nkscrzyvj',
    // note: the account endpoint returns CIP-105, unlike /governance/dreps
    drep_id: 'drep1000002hneyj7jl9y95m0zxdsg6dp9n9y59lvlm7xjqp4qh2jx7h',
    ...overrides,
  };
}

describe('network', () => {
  it('combines genesis and the tip, and reports asOf', async () => {
    const bf = new FakeBlockfrost()
      .on('/genesis', GENESIS)
      .on('/blocks/latest', bfBlock())
      .on('/epochs/latest', bfEpoch(656));

    const { data, meta } = await provider(bf).network.getNetworkInfo();
    expect(data.network).toBe('mainnet');
    expect(data.networkMagic).toBe(764824073);
    expect(data.tip).toMatchObject({ epoch: 656, block: 13955938 });
    expect(data.tip.blockHash).toBe(bfBlock().hash);
    // this route already fetched the tip, so it reports it
    expect(meta.asOf).toEqual(data.tip);
    expect(data.epoch.startTime).toMatch(/^\d{4}-/);
  });

  it('reads genesis once across calls', async () => {
    const bf = new FakeBlockfrost()
      .on('/genesis', GENESIS)
      .on('/blocks/latest', bfBlock())
      .on('/epochs/latest', bfEpoch(656));
    const api = provider(bf).network;
    await api.getNetworkInfo();
    await api.getNetworkInfo();
    expect(bf.callsTo('/genesis')).toHaveLength(1);
  });

  it('lifts the governance parameters and leaves the float thresholds in raw', async () => {
    const bf = new FakeBlockfrost().on('/epochs/latest/parameters', {
      epoch: 656,
      drep_deposit: '500000000',
      gov_action_deposit: '100000000000',
      drep_activity: 20,
      gov_action_lifetime: 6,
      committee_min_size: 5,
      committee_max_term_length: 146,
      min_fee_a: 44,
      protocol_major_ver: 11,
      protocol_minor_ver: 0,
      dvt_motion_no_confidence: 0.67,
    });

    const { data } = await provider(bf).network.getProtocolParams();
    expect(data).toMatchObject({
      epoch: 656,
      drepDeposit: '500000000',
      govActionDeposit: '100000000000',
      drepActivity: 20,
      govActionLifetime: 6,
      committeeMinSize: 5,
      committeeMaxTermLength: 146,
      protocolVersion: { major: 11, minor: 0 },
    });
    // a double cannot become an exact on-chain ratio
    expect(data.dvt).toBeUndefined();
    expect(data.pvt).toBeUndefined();
    expect(data.raw.dvt_motion_no_confidence).toBe(0.67);
  });

  it('serves only the active stake total, omitting the breakdown it cannot compute', async () => {
    const bf = new FakeBlockfrost().on('/epochs/latest', bfEpoch(656));
    const { data } = await provider(bf).network.getStakeDistribution();

    expect(data).toEqual({
      epoch: 656,
      totalActiveStake: '21370797071255004',
    });
    expect(data.totalStakeControlledByDReps).toBeUndefined();
  });

  it('walks epochs backwards from the tip', async () => {
    const bf = new FakeBlockfrost().on('/epochs/latest', bfEpoch(656));
    for (const e of [655, 654]) bf.on(`/epochs/${e}`, bfEpoch(e));

    const { data } = await provider(bf).network.listEpochs({ limit: 3 });
    expect(data.map((e) => e.epoch)).toEqual([656, 655, 654]);
    expect(data[0]!.durationSeconds).toBe(432000);
  });
});

describe('accounts', () => {
  it('serves registration, balance and both delegations from one read', async () => {
    const bf = new FakeBlockfrost().on(`/accounts/${STAKE}`, bfAccount());
    const { data } = await provider(bf).accounts.get(STAKE, {
      expand: ['balance', 'delegation', 'poolDelegation'],
    });

    expect(data.isRegistered).toBe(true);
    expect(data.stakeKeyHash).toHaveLength(56);
    // total supply scale, kept a string
    expect(data.balance?.total).toBe('45000000000000000');
    expect(data.delegation?.target).toMatchObject({ kind: 'drep' });
    expect(data.poolDelegation?.poolId).toBe(bfAccount().pool_id);
    expect(bf.callsTo(`/accounts/${STAKE}`)).toHaveLength(1);
  });

  it('normalises the account’s CIP-105 drep_id to the CIP-129 id', async () => {
    const bf = new FakeBlockfrost().on(`/accounts/${STAKE}`, bfAccount());
    const { data } = await provider(bf).accounts.getDelegation(STAKE);

    expect(data?.target.kind).toBe('drep');
    if (data?.target.kind === 'drep') {
      // Blockfrost is inconsistent per endpoint; the contract is not.
      expect(data.target.drep.id).toMatch(/^drep1y/);
      expect(data.target.drep.cip105Id).toBe(bfAccount().drep_id);
      expect(data.target.drep.hash).toHaveLength(56);
    }
    // the endpoint does not say which tx set it
    expect(data?.txRef).toBeNull();
  });

  it('returns null delegation when the account never delegated', async () => {
    const bf = new FakeBlockfrost().on(
      `/accounts/${STAKE}`,
      bfAccount({ drep_id: null }),
    );
    await expect(
      provider(bf).accounts.getDelegation(STAKE),
    ).resolves.toMatchObject({ data: null });
  });

  it('refuses the expands with no Blockfrost source', async () => {
    const bf = new FakeBlockfrost().on(`/accounts/${STAKE}`, bfAccount());
    for (const field of ['votingPower', 'drep', 'adaHandles'] as const) {
      await expect(
        provider(bf).accounts.get(STAKE, { expand: [field] }),
      ).rejects.toMatchObject({ code: 'CAPABILITY_UNSUPPORTED' });
    }
  });

  it('requires a bech32 stake address, since Blockfrost accepts nothing else', async () => {
    const bf = new FakeBlockfrost();
    await expect(
      provider(bf).accounts.get('ff'.repeat(28)),
    ).rejects.toMatchObject({
      code: 'INVALID_INPUT',
    });
    expect(bf.calls).toHaveLength(0);
  });

  it('refuses governance delegation history, which has no endpoint', async () => {
    const bf = new FakeBlockfrost();
    await expect(
      provider(bf).accounts.listDelegationHistory(STAKE, {
        kind: ['governance'],
      }),
    ).rejects.toMatchObject({ code: 'CAPABILITY_UNSUPPORTED' });
  });
});

describe('paging translation', () => {
  it('turns limit/cursor into Blockfrost count/page', async () => {
    const bf = new FakeBlockfrost().on(`/accounts/${STAKE}/registrations`, []);
    await provider(bf).accounts.listStakeEvents(STAKE, {
      limit: 10,
      cursor: '3',
    });

    expect(
      bf.callsTo(`/accounts/${STAKE}/registrations`)[0]!.search,
    ).toMatchObject({
      count: '10',
      page: '3',
    });
  });

  it('caps a limit above Blockfrost’s maximum', async () => {
    const bf = new FakeBlockfrost().on(`/accounts/${STAKE}/registrations`, []);
    await provider(bf).accounts.listStakeEvents(STAKE, { limit: 5000 });
    expect(
      bf.callsTo(`/accounts/${STAKE}/registrations`)[0]!.search.count,
    ).toBe('100');
  });

  it('accepts an offset that lands on a page boundary and refuses one that does not', async () => {
    const bf = new FakeBlockfrost().on(`/accounts/${STAKE}/registrations`, []);
    const api = provider(bf).accounts;

    await api.listStakeEvents(STAKE, { limit: 10, offset: 20 });
    expect(bf.callsTo(`/accounts/${STAKE}/registrations`)[0]!.search.page).toBe(
      '3',
    );

    // Blockfrost can only seek whole pages; rounding would return wrong rows
    await expect(
      api.listStakeEvents(STAKE, { limit: 10, offset: 25 }),
    ).rejects.toMatchObject({ code: 'INVALID_INPUT' });
  });

  it('rejects a cursor it did not issue', async () => {
    const bf = new FakeBlockfrost().on(`/accounts/${STAKE}/registrations`, []);
    await expect(
      provider(bf).accounts.listStakeEvents(STAKE, { cursor: 'abc' }),
    ).rejects.toMatchObject({ code: 'INVALID_INPUT' });
  });
});

describe('system.getCapabilities', () => {
  it('declares the provider, the network read from genesis, and the datasets', async () => {
    const bf = new FakeBlockfrost().on('/genesis', GENESIS);
    const { data } = await provider(bf).system.getCapabilities();

    expect(data.provider).toBe('blockfrost');
    expect(data.network).toBe('mainnet');
    expect(data.schemaVersion).toBe(2);
    expect(data.datasets['proposal.ballot.current'].reachability).toBe(
      'served',
    );
    expect(data.datasets['drep.delegation.current'].reachability).toBe(
      'served',
    );
    expect(data.datasets['network.aggregate.current'].reachability).toBe(
      'refused',
    );
    expect(data.datasets['transaction.identity.current'].reachability).toBe(
      'refused',
    );
    // The namespace is absent from the object, not merely refused.
    expect(data.datasets['survey.body.current'].reachability).toBe('missing');
  });

  it('still reports capabilities when the backend is unreachable', async () => {
    const bf = new FakeBlockfrost().onThrow('/genesis', new Error('down'));
    const { data } = await provider(bf).system.getCapabilities();
    expect(data.network).toBe('unknown');
    expect(Object.keys(data.datasets).length).toBeGreaterThan(30);
  });

  it('reports unavailable when /health fails and degraded when it says so', async () => {
    const down = new FakeBlockfrost().onThrow('/health', new Error('refused'));
    await expect(provider(down).system.getHealth()).resolves.toMatchObject({
      data: [{ status: 'unavailable' }],
    });

    const unhealthy = new FakeBlockfrost().on('/health', { is_healthy: false });
    await expect(provider(unhealthy).system.getHealth()).resolves.toMatchObject(
      {
        data: [{ status: 'degraded' }],
      },
    );
  });
});
