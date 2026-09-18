import { resolveCapabilities } from '@govtool/data-providers/chain-data';

import { DbSyncChainDataProvider, missingUtxoViewOverride } from '../src';
import { fakeDb } from './fake-db';

describe('network.getNetworkInfo', () => {
  it('maps the tip and passes the network name through', async () => {
    const db = fakeDb('get-network-info.sql').on('get-network-info.sql', [
      {
        current_epoch: 500,
        current_block: 11_000_000,
        network_name: 'preview',
      },
    ]);
    const { data, meta } = await new DbSyncChainDataProvider(
      db,
    ).network.getNetworkInfo();

    expect(data).toEqual({
      network: 'preview',
      tip: { epoch: 500, block: 11_000_000 },
      epoch: { no: 500 },
    });
    // no statement returns the tip alongside a query, so asOf is omitted
    expect(meta.asOf).toBeUndefined();
  });

  it('passes through a network name outside the known three', async () => {
    const db = fakeDb('get-network-info.sql').on('get-network-info.sql', [
      { current_epoch: 1, current_block: 2, network_name: 'sanchonet' },
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).network.getNetworkInfo();
    expect(data.network).toBe('sanchonet');
  });

  it('fails with the legacy message when the tip is incomplete', async () => {
    const db = fakeDb('get-network-info.sql').on('get-network-info.sql', [
      { current_epoch: null, current_block: 2, network_name: 'mainnet' },
    ]);
    await expect(
      new DbSyncChainDataProvider(db).network.getNetworkInfo(),
    ).rejects.toMatchObject({
      code: 'INTERNAL',
      message: 'Could not query the network info. This should never happen.',
    });
  });
});

describe('network.getStakeDistribution', () => {
  it('keeps every total a string and omits the totals it has no column for', async () => {
    const db = fakeDb('get-network-total-stake.sql').on(
      'get-network-total-stake.sql',
      [
        {
          total_stake_controlled_by_active_dreps: '31000000000000000',
          total_stake_controlled_by_spos: '22000000000000000',
          always_abstain_voting_power: '4000000000000',
          always_no_confidence_voting_power: '900000000',
        },
      ],
    );
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).network.getStakeDistribution();

    // 3.1e16 exceeds Number.MAX_SAFE_INTEGER; it must not round-trip a number
    expect(data.totalStakeControlledByDReps).toBe('31000000000000000');
    expect(data.totalStakeControlledBySPOs).toBe('22000000000000000');
    expect(data.totalActiveStake).toBeUndefined();
    expect(data.epoch).toBeUndefined();
  });
});

describe('network.getProtocolParams', () => {
  it('returns the raw row and lifts the scalars, but not the float thresholds', async () => {
    const db = fakeDb('get-current-epoch-params.sql').on(
      'get-current-epoch-params.sql',
      [
        {
          epoch_param: {
            epoch_no: 500,
            gov_action_deposit: 100000000000,
            drep_deposit: 500000000,
            drep_activity: 20,
            gov_action_lifetime: 6,
            committee_min_size: 7,
            min_fee_a: 44,
            protocol_major: 10,
            protocol_minor: 0,
            dvt_motion_no_confidence: 0.67,
            cost_model: null,
          },
        },
      ],
    );
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).network.getProtocolParams();

    expect(data.epoch).toBe(500);
    expect(data.govActionDeposit).toBe('100000000000');
    expect(data.drepDeposit).toBe('500000000');
    expect(data.drepActivity).toBe(20);
    expect(data.protocolVersion).toEqual({ major: 10, minor: 0 });
    // db-sync stores thresholds as doubles, which cannot be turned back into
    // the on-chain ratio, so they are left to `raw`.
    expect(data.dvt).toBeUndefined();
    expect(data.raw).toMatchObject({ dvt_motion_no_confidence: 0.67 });
  });

  it('refuses a request for a past epoch rather than answering with the current one', async () => {
    const db = fakeDb('get-current-epoch-params.sql');
    await expect(
      new DbSyncChainDataProvider(db).network.getProtocolParams({ epoch: 499 }),
    ).rejects.toMatchObject({ code: 'CAPABILITY_UNSUPPORTED' });
    expect(db.calls).toHaveLength(0);
  });
});

describe('governance.metrics.get', () => {
  it('maps counters as numbers and the stake total as a string', async () => {
    const db = fakeDb('get-network-metrics.sql').on('get-network-metrics.sql', [
      {
        unique_delegators: 1000,
        total_delegations: 1200,
        total_gov_action_proposals: 50,
        total_drep_votes: 900,
        total_registered_dreps: 700,
        total_drep_distr: '31000000000000000',
        total_active_dreps: 400,
        total_inactive_dreps: 300,
        total_active_cip119_compliant_dreps: 250,
        total_registered_direct_voters: 20,
        no_of_committee_members: 7,
        quorum_numerator: 2,
        quorum_denominator: 3,
      },
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).governance.metrics.get();

    expect(data).toEqual({
      uniqueDelegators: 1000,
      totalDelegations: 1200,
      totalGovernanceActions: 50,
      totalDRepVotes: 900,
      totalRegisteredDReps: 700,
      totalDRepDistribution: '31000000000000000',
      totalActiveDReps: 400,
      totalInactiveDReps: 300,
      totalActiveCip119CompliantDReps: 250,
      totalRegisteredDirectVoters: 20,
      committee: { size: 7, quorum: { numerator: 2, denominator: 3 } },
    });
  });

  it('fails with the legacy message on a non-integer counter', async () => {
    const db = fakeDb('get-network-metrics.sql').on('get-network-metrics.sql', [
      { unique_delegators: 'not a number' },
    ]);
    await expect(
      new DbSyncChainDataProvider(db).governance.metrics.get(),
    ).rejects.toMatchObject({
      code: 'INTERNAL',
      message: 'Unexpected non-integer value returned from database.',
    });
  });
});

describe('system', () => {
  it('declares its capabilities, gaps included', async () => {
    const db = fakeDb('get-network-info.sql').on('get-network-info.sql', [
      { current_epoch: 1, current_block: 2, network_name: 'preprod' },
    ]);
    const { data } = await new DbSyncChainDataProvider(
      db,
    ).system.getCapabilities();

    expect(data.provider).toBe('dbsync');
    // The network is read from `meta`, not hard-coded into the declaration.
    expect(data.network).toBe('preprod');
    expect(data.schemaVersion).toBe(2);
    expect(data.datasets['drep.identity.current'].reachability).toBe('served');
    expect(data.datasets['pool.identity.current'].reachability).toBe('refused');
    expect(data.datasets['proposal.identity.current'].reachability).toBe(
      'served',
    );
    // A deployment fault is never baked into the constant.
    expect(data.overrides).toEqual([]);
  });

  it('applies a deployment override without touching the static table', async () => {
    const db = fakeDb('get-network-info.sql').on('get-network-info.sql', [
      { current_epoch: 1, current_block: 2, network_name: 'preview' },
    ]);
    const { data } = await new DbSyncChainDataProvider(db, {
      capabilityOverrides: [missingUtxoViewOverride()],
    }).system.getCapabilities();

    expect(data.datasets['account.stake.current'].reachability).toBe('served');
    expect(
      resolveCapabilities(data)['account.stake.current'].reachability,
    ).toBe('refused');
  });

  it('reports the tip when healthy and unavailable when the database is down', async () => {
    const up = fakeDb('get-network-info.sql').on('get-network-info.sql', [
      {
        current_epoch: 500,
        current_block: 11_000_000,
        network_name: 'mainnet',
      },
    ]);
    await expect(
      new DbSyncChainDataProvider(up, {
        stalenessThresholdSeconds: 300,
      }).system.getHealth(),
    ).resolves.toMatchObject({
      data: [
        {
          provider: 'dbsync',
          status: 'healthy',
          tip: { epoch: 500, block: 11_000_000 },
          stalenessThresholdSeconds: 300,
        },
      ],
    });

    const down = fakeDb('get-network-info.sql').failOn(
      'get-network-info.sql',
      new Error('connection refused'),
    );
    const { data } = await new DbSyncChainDataProvider(down).system.getHealth();
    expect(data[0]).toMatchObject({ status: 'unavailable' });
  });
});
